	.file	"bitfield.mc"
	.section	".text"
	.align 4
	.type	bitfield__read_var_BitField_offset1_count1, #function
	.proc	04
bitfield__read_var_BitField_offset1_count1:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	bitfield__read_var_BitField_offset1_count1, .-bitfield__read_var_BitField_offset1_count1
	.align 4
	.type	bitfield__write_var_BitField_offset1_count1, #function
	.proc	020
bitfield__write_var_BitField_offset1_count1:
.LLFB1:
	save	%sp, -104, %sp
.LLCFI1:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL3
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL4
	 nop
.LL3:
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL4:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -65, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE1:
	.size	bitfield__write_var_BitField_offset1_count1, .-bitfield__write_var_BitField_offset1_count1
	.align 4
	.type	bitfield__read_var_BitField_offset1_count2, #function
	.proc	04
bitfield__read_var_BitField_offset1_count2:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	bitfield__read_var_BitField_offset1_count2, .-bitfield__read_var_BitField_offset1_count2
	.align 4
	.type	bitfield__write_var_BitField_offset1_count2, #function
	.proc	020
bitfield__write_var_BitField_offset1_count2:
.LLFB3:
	save	%sp, -104, %sp
.LLCFI3:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL7
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL8
	 nop
.LL7:
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -97, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE3:
	.size	bitfield__write_var_BitField_offset1_count2, .-bitfield__write_var_BitField_offset1_count2
	.align 4
	.type	bitfield__read_var_BitField_offset1_count3, #function
	.proc	04
bitfield__read_var_BitField_offset1_count3:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	bitfield__read_var_BitField_offset1_count3, .-bitfield__read_var_BitField_offset1_count3
	.align 4
	.type	bitfield__write_var_BitField_offset1_count3, #function
	.proc	020
bitfield__write_var_BitField_offset1_count3:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL11
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL12
	 nop
.LL11:
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL12:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -113, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE5:
	.size	bitfield__write_var_BitField_offset1_count3, .-bitfield__write_var_BitField_offset1_count3
	.align 4
	.type	bitfield__read_var_BitField_offset1_count4, #function
	.proc	04
bitfield__read_var_BitField_offset1_count4:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	bitfield__read_var_BitField_offset1_count4, .-bitfield__read_var_BitField_offset1_count4
	.align 4
	.type	bitfield__write_var_BitField_offset1_count4, #function
	.proc	020
bitfield__write_var_BitField_offset1_count4:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL15
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL16
	 nop
.LL15:
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL16:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -121, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE7:
	.size	bitfield__write_var_BitField_offset1_count4, .-bitfield__write_var_BitField_offset1_count4
	.align 4
	.type	bitfield__read_var_BitField_offset1_count5, #function
	.proc	04
bitfield__read_var_BitField_offset1_count5:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	bitfield__read_var_BitField_offset1_count5, .-bitfield__read_var_BitField_offset1_count5
	.align 4
	.type	bitfield__write_var_BitField_offset1_count5, #function
	.proc	020
bitfield__write_var_BitField_offset1_count5:
.LLFB9:
	save	%sp, -104, %sp
.LLCFI9:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL19
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL20
	 nop
.LL19:
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL20:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -125, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE9:
	.size	bitfield__write_var_BitField_offset1_count5, .-bitfield__write_var_BitField_offset1_count5
	.align 4
	.type	bitfield__read_var_BitField_offset1_count6, #function
	.proc	04
bitfield__read_var_BitField_offset1_count6:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	bitfield__read_var_BitField_offset1_count6, .-bitfield__read_var_BitField_offset1_count6
	.align 4
	.type	bitfield__write_var_BitField_offset1_count6, #function
	.proc	020
bitfield__write_var_BitField_offset1_count6:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL23
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL24
	 nop
.LL23:
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL24:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ldub	[%g1], %g1
	and	%g1, -127, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE11:
	.size	bitfield__write_var_BitField_offset1_count6, .-bitfield__write_var_BitField_offset1_count6
	.align 4
	.type	bitfield__read_var_BitField_offset1_count7, #function
	.proc	04
bitfield__read_var_BitField_offset1_count7:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	bitfield__read_var_BitField_offset1_count7, .-bitfield__read_var_BitField_offset1_count7
	.align 4
	.type	bitfield__write_var_BitField_offset1_count7, #function
	.proc	020
bitfield__write_var_BitField_offset1_count7:
.LLFB13:
	save	%sp, -104, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL27
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL28
	 nop
.LL27:
	mov	3489, %o0
	call	_m3_fault, 0
	 nop
.LL28:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -128, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE13:
	.size	bitfield__write_var_BitField_offset1_count7, .-bitfield__write_var_BitField_offset1_count7
	.align 4
	.type	bitfield__read_var_BitField_offset1_count8, #function
	.proc	04
bitfield__read_var_BitField_offset1_count8:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	bitfield__read_var_BitField_offset1_count8, .-bitfield__read_var_BitField_offset1_count8
	.align 4
	.type	bitfield__write_var_BitField_offset1_count8, #function
	.proc	020
bitfield__write_var_BitField_offset1_count8:
.LLFB15:
	save	%sp, -104, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL31
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL32
	 nop
.LL31:
	mov	3745, %o0
	call	_m3_fault, 0
	 nop
.LL32:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE15:
	.size	bitfield__write_var_BitField_offset1_count8, .-bitfield__write_var_BitField_offset1_count8
	.align 4
	.type	bitfield__read_var_BitField_offset1_count9, #function
	.proc	04
bitfield__read_var_BitField_offset1_count9:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	bitfield__read_var_BitField_offset1_count9, .-bitfield__read_var_BitField_offset1_count9
	.align 4
	.type	bitfield__write_var_BitField_offset1_count9, #function
	.proc	020
bitfield__write_var_BitField_offset1_count9:
.LLFB17:
	save	%sp, -104, %sp
.LLCFI17:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL35
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL36
	 nop
.LL35:
	mov	4001, %o0
	call	_m3_fault, 0
	 nop
.LL36:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE17:
	.size	bitfield__write_var_BitField_offset1_count9, .-bitfield__write_var_BitField_offset1_count9
	.align 4
	.type	bitfield__read_var_BitField_offset1_count10, #function
	.proc	04
bitfield__read_var_BitField_offset1_count10:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	bitfield__read_var_BitField_offset1_count10, .-bitfield__read_var_BitField_offset1_count10
	.align 4
	.type	bitfield__write_var_BitField_offset1_count10, #function
	.proc	020
bitfield__write_var_BitField_offset1_count10:
.LLFB19:
	save	%sp, -104, %sp
.LLCFI19:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL39
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL40
	 nop
.LL39:
	sethi	%hi(4096), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL40:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE19:
	.size	bitfield__write_var_BitField_offset1_count10, .-bitfield__write_var_BitField_offset1_count10
	.align 4
	.type	bitfield__read_var_BitField_offset1_count11, #function
	.proc	04
bitfield__read_var_BitField_offset1_count11:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	bitfield__read_var_BitField_offset1_count11, .-bitfield__read_var_BitField_offset1_count11
	.align 4
	.type	bitfield__write_var_BitField_offset1_count11, #function
	.proc	020
bitfield__write_var_BitField_offset1_count11:
.LLFB21:
	save	%sp, -104, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL43
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL44
	 nop
.LL43:
	sethi	%hi(4096), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL44:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE21:
	.size	bitfield__write_var_BitField_offset1_count11, .-bitfield__write_var_BitField_offset1_count11
	.align 4
	.type	bitfield__read_var_BitField_offset1_count12, #function
	.proc	04
bitfield__read_var_BitField_offset1_count12:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	bitfield__read_var_BitField_offset1_count12, .-bitfield__read_var_BitField_offset1_count12
	.align 4
	.type	bitfield__write_var_BitField_offset1_count12, #function
	.proc	020
bitfield__write_var_BitField_offset1_count12:
.LLFB23:
	save	%sp, -104, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL47
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL48
	 nop
.LL47:
	sethi	%hi(4096), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL48:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE23:
	.size	bitfield__write_var_BitField_offset1_count12, .-bitfield__write_var_BitField_offset1_count12
	.align 4
	.type	bitfield__read_var_BitField_offset1_count13, #function
	.proc	04
bitfield__read_var_BitField_offset1_count13:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	bitfield__read_var_BitField_offset1_count13, .-bitfield__read_var_BitField_offset1_count13
	.align 4
	.type	bitfield__write_var_BitField_offset1_count13, #function
	.proc	020
bitfield__write_var_BitField_offset1_count13:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL51
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL52
	 nop
.LL51:
	sethi	%hi(4096), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL52:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE25:
	.size	bitfield__write_var_BitField_offset1_count13, .-bitfield__write_var_BitField_offset1_count13
	.align 4
	.type	bitfield__read_var_BitField_offset1_count14, #function
	.proc	04
bitfield__read_var_BitField_offset1_count14:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	bitfield__read_var_BitField_offset1_count14, .-bitfield__read_var_BitField_offset1_count14
	.align 4
	.type	bitfield__write_var_BitField_offset1_count14, #function
	.proc	020
bitfield__write_var_BitField_offset1_count14:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL55
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL56
	 nop
.LL55:
	sethi	%hi(5120), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL56:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-32768), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE27:
	.size	bitfield__write_var_BitField_offset1_count14, .-bitfield__write_var_BitField_offset1_count14
	.align 4
	.type	bitfield__read_var_BitField_offset1_count15, #function
	.proc	04
bitfield__read_var_BitField_offset1_count15:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	bitfield__read_var_BitField_offset1_count15, .-bitfield__read_var_BitField_offset1_count15
	.align 4
	.type	bitfield__write_var_BitField_offset1_count15, #function
	.proc	020
bitfield__write_var_BitField_offset1_count15:
.LLFB29:
	save	%sp, -104, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL59
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL60
	 nop
.LL59:
	sethi	%hi(5120), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL60:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	mov	%o5, %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE29:
	.size	bitfield__write_var_BitField_offset1_count15, .-bitfield__write_var_BitField_offset1_count15
	.align 4
	.type	bitfield__read_var_BitField_offset1_count16, #function
	.proc	04
bitfield__read_var_BitField_offset1_count16:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	bitfield__read_var_BitField_offset1_count16, .-bitfield__read_var_BitField_offset1_count16
	.align 4
	.type	bitfield__write_var_BitField_offset1_count16, #function
	.proc	020
bitfield__write_var_BitField_offset1_count16:
.LLFB31:
	save	%sp, -104, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL63
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL64
	 nop
.LL63:
	sethi	%hi(5120), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL64:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147451904), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE31:
	.size	bitfield__write_var_BitField_offset1_count16, .-bitfield__write_var_BitField_offset1_count16
	.align 4
	.type	bitfield__read_var_BitField_offset1_count17, #function
	.proc	04
bitfield__read_var_BitField_offset1_count17:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	bitfield__read_var_BitField_offset1_count17, .-bitfield__read_var_BitField_offset1_count17
	.align 4
	.type	bitfield__write_var_BitField_offset1_count17, #function
	.proc	020
bitfield__write_var_BitField_offset1_count17:
.LLFB33:
	save	%sp, -104, %sp
.LLCFI33:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL67
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL68
	 nop
.LL67:
	sethi	%hi(5120), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL68:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147468288), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE33:
	.size	bitfield__write_var_BitField_offset1_count17, .-bitfield__write_var_BitField_offset1_count17
	.align 4
	.type	bitfield__read_var_BitField_offset1_count18, #function
	.proc	04
bitfield__read_var_BitField_offset1_count18:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	bitfield__read_var_BitField_offset1_count18, .-bitfield__read_var_BitField_offset1_count18
	.align 4
	.type	bitfield__write_var_BitField_offset1_count18, #function
	.proc	020
bitfield__write_var_BitField_offset1_count18:
.LLFB35:
	save	%sp, -104, %sp
.LLCFI35:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL71
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL72
	 nop
.LL71:
	sethi	%hi(6144), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL72:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147476480), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE35:
	.size	bitfield__write_var_BitField_offset1_count18, .-bitfield__write_var_BitField_offset1_count18
	.align 4
	.type	bitfield__read_var_BitField_offset1_count19, #function
	.proc	04
bitfield__read_var_BitField_offset1_count19:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	bitfield__read_var_BitField_offset1_count19, .-bitfield__read_var_BitField_offset1_count19
	.align 4
	.type	bitfield__write_var_BitField_offset1_count19, #function
	.proc	020
bitfield__write_var_BitField_offset1_count19:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL75
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL76
	 nop
.LL75:
	sethi	%hi(6144), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL76:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147480576), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE37:
	.size	bitfield__write_var_BitField_offset1_count19, .-bitfield__write_var_BitField_offset1_count19
	.align 4
	.type	bitfield__read_var_BitField_offset1_count20, #function
	.proc	04
bitfield__read_var_BitField_offset1_count20:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE38:
	.size	bitfield__read_var_BitField_offset1_count20, .-bitfield__read_var_BitField_offset1_count20
	.align 4
	.type	bitfield__write_var_BitField_offset1_count20, #function
	.proc	020
bitfield__write_var_BitField_offset1_count20:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL79
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL80
	 nop
.LL79:
	sethi	%hi(6144), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL80:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147482624), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE39:
	.size	bitfield__write_var_BitField_offset1_count20, .-bitfield__write_var_BitField_offset1_count20
	.align 4
	.type	bitfield__read_var_BitField_offset1_count21, #function
	.proc	04
bitfield__read_var_BitField_offset1_count21:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	bitfield__read_var_BitField_offset1_count21, .-bitfield__read_var_BitField_offset1_count21
	.align 4
	.type	bitfield__write_var_BitField_offset1_count21, #function
	.proc	020
bitfield__write_var_BitField_offset1_count21:
.LLFB41:
	save	%sp, -104, %sp
.LLCFI41:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL83
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL84
	 nop
.LL83:
	sethi	%hi(6144), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL84:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE41:
	.size	bitfield__write_var_BitField_offset1_count21, .-bitfield__write_var_BitField_offset1_count21
	.align 4
	.type	bitfield__read_var_BitField_offset1_count22, #function
	.proc	04
bitfield__read_var_BitField_offset1_count22:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	bitfield__read_var_BitField_offset1_count22, .-bitfield__read_var_BitField_offset1_count22
	.align 4
	.type	bitfield__write_var_BitField_offset1_count22, #function
	.proc	020
bitfield__write_var_BitField_offset1_count22:
.LLFB43:
	save	%sp, -104, %sp
.LLCFI43:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL87
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL88
	 nop
.LL87:
	sethi	%hi(7168), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL88:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE43:
	.size	bitfield__write_var_BitField_offset1_count22, .-bitfield__write_var_BitField_offset1_count22
	.align 4
	.type	bitfield__read_var_BitField_offset1_count23, #function
	.proc	04
bitfield__read_var_BitField_offset1_count23:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	bitfield__read_var_BitField_offset1_count23, .-bitfield__read_var_BitField_offset1_count23
	.align 4
	.type	bitfield__write_var_BitField_offset1_count23, #function
	.proc	020
bitfield__write_var_BitField_offset1_count23:
.LLFB45:
	save	%sp, -104, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL91
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL92
	 nop
.LL91:
	sethi	%hi(7168), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL92:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE45:
	.size	bitfield__write_var_BitField_offset1_count23, .-bitfield__write_var_BitField_offset1_count23
	.align 4
	.type	bitfield__read_var_BitField_offset1_count24, #function
	.proc	04
bitfield__read_var_BitField_offset1_count24:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	bitfield__read_var_BitField_offset1_count24, .-bitfield__read_var_BitField_offset1_count24
	.align 4
	.type	bitfield__write_var_BitField_offset1_count24, #function
	.proc	020
bitfield__write_var_BitField_offset1_count24:
.LLFB47:
	save	%sp, -104, %sp
.LLCFI47:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL95
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL96
	 nop
.LL95:
	sethi	%hi(7168), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL96:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE47:
	.size	bitfield__write_var_BitField_offset1_count24, .-bitfield__write_var_BitField_offset1_count24
	.align 4
	.type	bitfield__read_var_BitField_offset1_count25, #function
	.proc	04
bitfield__read_var_BitField_offset1_count25:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	bitfield__read_var_BitField_offset1_count25, .-bitfield__read_var_BitField_offset1_count25
	.align 4
	.type	bitfield__write_var_BitField_offset1_count25, #function
	.proc	020
bitfield__write_var_BitField_offset1_count25:
.LLFB49:
	save	%sp, -104, %sp
.LLCFI49:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL99
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL100
	 nop
.LL99:
	sethi	%hi(7168), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL100:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE49:
	.size	bitfield__write_var_BitField_offset1_count25, .-bitfield__write_var_BitField_offset1_count25
	.align 4
	.type	bitfield__read_var_BitField_offset1_count26, #function
	.proc	04
bitfield__read_var_BitField_offset1_count26:
.LLFB50:
	save	%sp, -96, %sp
.LLCFI50:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE50:
	.size	bitfield__read_var_BitField_offset1_count26, .-bitfield__read_var_BitField_offset1_count26
	.align 4
	.type	bitfield__write_var_BitField_offset1_count26, #function
	.proc	020
bitfield__write_var_BitField_offset1_count26:
.LLFB51:
	save	%sp, -104, %sp
.LLCFI51:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL103
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL104
	 nop
.LL103:
	sethi	%hi(8192), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL104:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE51:
	.size	bitfield__write_var_BitField_offset1_count26, .-bitfield__write_var_BitField_offset1_count26
	.align 4
	.type	bitfield__read_var_BitField_offset1_count27, #function
	.proc	04
bitfield__read_var_BitField_offset1_count27:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	bitfield__read_var_BitField_offset1_count27, .-bitfield__read_var_BitField_offset1_count27
	.align 4
	.type	bitfield__write_var_BitField_offset1_count27, #function
	.proc	020
bitfield__write_var_BitField_offset1_count27:
.LLFB53:
	save	%sp, -104, %sp
.LLCFI53:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL107
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL108
	 nop
.LL107:
	sethi	%hi(8192), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL108:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE53:
	.size	bitfield__write_var_BitField_offset1_count27, .-bitfield__write_var_BitField_offset1_count27
	.align 4
	.type	bitfield__read_var_BitField_offset1_count28, #function
	.proc	04
bitfield__read_var_BitField_offset1_count28:
.LLFB54:
	save	%sp, -96, %sp
.LLCFI54:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE54:
	.size	bitfield__read_var_BitField_offset1_count28, .-bitfield__read_var_BitField_offset1_count28
	.align 4
	.type	bitfield__write_var_BitField_offset1_count28, #function
	.proc	020
bitfield__write_var_BitField_offset1_count28:
.LLFB55:
	save	%sp, -104, %sp
.LLCFI55:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL111
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL112
	 nop
.LL111:
	sethi	%hi(8192), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL112:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(268434432), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE55:
	.size	bitfield__write_var_BitField_offset1_count28, .-bitfield__write_var_BitField_offset1_count28
	.align 4
	.type	bitfield__read_var_BitField_offset1_count29, #function
	.proc	04
bitfield__read_var_BitField_offset1_count29:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE56:
	.size	bitfield__read_var_BitField_offset1_count29, .-bitfield__read_var_BitField_offset1_count29
	.align 4
	.type	bitfield__write_var_BitField_offset1_count29, #function
	.proc	020
bitfield__write_var_BitField_offset1_count29:
.LLFB57:
	save	%sp, -104, %sp
.LLCFI57:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL115
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL116
	 nop
.LL115:
	sethi	%hi(8192), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL116:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(536869888), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE57:
	.size	bitfield__write_var_BitField_offset1_count29, .-bitfield__write_var_BitField_offset1_count29
	.align 4
	.type	bitfield__read_var_BitField_offset1_count30, #function
	.proc	04
bitfield__read_var_BitField_offset1_count30:
.LLFB58:
	save	%sp, -96, %sp
.LLCFI58:
	mov	%i0, %g1
	ld	[%g1], %g1
	add	%g1, %g1, %g1
	srl	%g1, 2, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE58:
	.size	bitfield__read_var_BitField_offset1_count30, .-bitfield__read_var_BitField_offset1_count30
	.align 4
	.type	bitfield__write_var_BitField_offset1_count30, #function
	.proc	020
bitfield__write_var_BitField_offset1_count30:
.LLFB59:
	save	%sp, -104, %sp
.LLCFI59:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL119
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL120
	 nop
.LL119:
	sethi	%hi(9216), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL120:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-2147483648), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE59:
	.size	bitfield__write_var_BitField_offset1_count30, .-bitfield__write_var_BitField_offset1_count30
	.align 4
	.type	bitfield__read_var_BitField_offset1_count31, #function
	.proc	04
bitfield__read_var_BitField_offset1_count31:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(2147482624), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	bitfield__read_var_BitField_offset1_count31, .-bitfield__read_var_BitField_offset1_count31
	.align 4
	.type	bitfield__write_var_BitField_offset1_count31, #function
	.proc	020
bitfield__write_var_BitField_offset1_count31:
.LLFB61:
	save	%sp, -104, %sp
.LLCFI61:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bge	%icc, .LL123
	 nop
	sethi	%hi(9216), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL123:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2147483648), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2147482624), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE61:
	.size	bitfield__write_var_BitField_offset1_count31, .-bitfield__write_var_BitField_offset1_count31
	.align 4
	.type	bitfield__read_var_BitField_offset2_count1, #function
	.proc	04
bitfield__read_var_BitField_offset2_count1:
.LLFB62:
	save	%sp, -96, %sp
.LLCFI62:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE62:
	.size	bitfield__read_var_BitField_offset2_count1, .-bitfield__read_var_BitField_offset2_count1
	.align 4
	.type	bitfield__write_var_BitField_offset2_count1, #function
	.proc	020
bitfield__write_var_BitField_offset2_count1:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL126
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL127
	 nop
.LL126:
	sethi	%hi(9216), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL127:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -33, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE63:
	.size	bitfield__write_var_BitField_offset2_count1, .-bitfield__write_var_BitField_offset2_count1
	.align 4
	.type	bitfield__read_var_BitField_offset2_count2, #function
	.proc	04
bitfield__read_var_BitField_offset2_count2:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	bitfield__read_var_BitField_offset2_count2, .-bitfield__read_var_BitField_offset2_count2
	.align 4
	.type	bitfield__write_var_BitField_offset2_count2, #function
	.proc	020
bitfield__write_var_BitField_offset2_count2:
.LLFB65:
	save	%sp, -104, %sp
.LLCFI65:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL130
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL131
	 nop
.LL130:
	sethi	%hi(9216), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL131:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -49, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE65:
	.size	bitfield__write_var_BitField_offset2_count2, .-bitfield__write_var_BitField_offset2_count2
	.align 4
	.type	bitfield__read_var_BitField_offset2_count3, #function
	.proc	04
bitfield__read_var_BitField_offset2_count3:
.LLFB66:
	save	%sp, -96, %sp
.LLCFI66:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	bitfield__read_var_BitField_offset2_count3, .-bitfield__read_var_BitField_offset2_count3
	.align 4
	.type	bitfield__write_var_BitField_offset2_count3, #function
	.proc	020
bitfield__write_var_BitField_offset2_count3:
.LLFB67:
	save	%sp, -104, %sp
.LLCFI67:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL134
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL135
	 nop
.LL134:
	sethi	%hi(10240), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL135:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -57, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE67:
	.size	bitfield__write_var_BitField_offset2_count3, .-bitfield__write_var_BitField_offset2_count3
	.align 4
	.type	bitfield__read_var_BitField_offset2_count4, #function
	.proc	04
bitfield__read_var_BitField_offset2_count4:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	bitfield__read_var_BitField_offset2_count4, .-bitfield__read_var_BitField_offset2_count4
	.align 4
	.type	bitfield__write_var_BitField_offset2_count4, #function
	.proc	020
bitfield__write_var_BitField_offset2_count4:
.LLFB69:
	save	%sp, -104, %sp
.LLCFI69:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL138
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL139
	 nop
.LL138:
	sethi	%hi(10240), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL139:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -61, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE69:
	.size	bitfield__write_var_BitField_offset2_count4, .-bitfield__write_var_BitField_offset2_count4
	.align 4
	.type	bitfield__read_var_BitField_offset2_count5, #function
	.proc	04
bitfield__read_var_BitField_offset2_count5:
.LLFB70:
	save	%sp, -96, %sp
.LLCFI70:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	bitfield__read_var_BitField_offset2_count5, .-bitfield__read_var_BitField_offset2_count5
	.align 4
	.type	bitfield__write_var_BitField_offset2_count5, #function
	.proc	020
bitfield__write_var_BitField_offset2_count5:
.LLFB71:
	save	%sp, -104, %sp
.LLCFI71:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL142
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL143
	 nop
.LL142:
	sethi	%hi(10240), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL143:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ldub	[%g1], %g1
	and	%g1, -63, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE71:
	.size	bitfield__write_var_BitField_offset2_count5, .-bitfield__write_var_BitField_offset2_count5
	.align 4
	.type	bitfield__read_var_BitField_offset2_count6, #function
	.proc	04
bitfield__read_var_BitField_offset2_count6:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	bitfield__read_var_BitField_offset2_count6, .-bitfield__read_var_BitField_offset2_count6
	.align 4
	.type	bitfield__write_var_BitField_offset2_count6, #function
	.proc	020
bitfield__write_var_BitField_offset2_count6:
.LLFB73:
	save	%sp, -104, %sp
.LLCFI73:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL146
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL147
	 nop
.LL146:
	sethi	%hi(10240), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL147:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -64, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE73:
	.size	bitfield__write_var_BitField_offset2_count6, .-bitfield__write_var_BitField_offset2_count6
	.align 4
	.type	bitfield__read_var_BitField_offset2_count7, #function
	.proc	04
bitfield__read_var_BitField_offset2_count7:
.LLFB74:
	save	%sp, -96, %sp
.LLCFI74:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE74:
	.size	bitfield__read_var_BitField_offset2_count7, .-bitfield__read_var_BitField_offset2_count7
	.align 4
	.type	bitfield__write_var_BitField_offset2_count7, #function
	.proc	020
bitfield__write_var_BitField_offset2_count7:
.LLFB75:
	save	%sp, -104, %sp
.LLCFI75:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL150
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL151
	 nop
.LL150:
	sethi	%hi(11264), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL151:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE75:
	.size	bitfield__write_var_BitField_offset2_count7, .-bitfield__write_var_BitField_offset2_count7
	.align 4
	.type	bitfield__read_var_BitField_offset2_count8, #function
	.proc	04
bitfield__read_var_BitField_offset2_count8:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	bitfield__read_var_BitField_offset2_count8, .-bitfield__read_var_BitField_offset2_count8
	.align 4
	.type	bitfield__write_var_BitField_offset2_count8, #function
	.proc	020
bitfield__write_var_BitField_offset2_count8:
.LLFB77:
	save	%sp, -104, %sp
.LLCFI77:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL154
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL155
	 nop
.LL154:
	sethi	%hi(11264), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL155:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE77:
	.size	bitfield__write_var_BitField_offset2_count8, .-bitfield__write_var_BitField_offset2_count8
	.align 4
	.type	bitfield__read_var_BitField_offset2_count9, #function
	.proc	04
bitfield__read_var_BitField_offset2_count9:
.LLFB78:
	save	%sp, -96, %sp
.LLCFI78:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	bitfield__read_var_BitField_offset2_count9, .-bitfield__read_var_BitField_offset2_count9
	.align 4
	.type	bitfield__write_var_BitField_offset2_count9, #function
	.proc	020
bitfield__write_var_BitField_offset2_count9:
.LLFB79:
	save	%sp, -104, %sp
.LLCFI79:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL158
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL159
	 nop
.LL158:
	sethi	%hi(11264), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL159:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE79:
	.size	bitfield__write_var_BitField_offset2_count9, .-bitfield__write_var_BitField_offset2_count9
	.align 4
	.type	bitfield__read_var_BitField_offset2_count10, #function
	.proc	04
bitfield__read_var_BitField_offset2_count10:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	bitfield__read_var_BitField_offset2_count10, .-bitfield__read_var_BitField_offset2_count10
	.align 4
	.type	bitfield__write_var_BitField_offset2_count10, #function
	.proc	020
bitfield__write_var_BitField_offset2_count10:
.LLFB81:
	save	%sp, -104, %sp
.LLCFI81:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL162
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL163
	 nop
.LL162:
	sethi	%hi(11264), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL163:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE81:
	.size	bitfield__write_var_BitField_offset2_count10, .-bitfield__write_var_BitField_offset2_count10
	.align 4
	.type	bitfield__read_var_BitField_offset2_count11, #function
	.proc	04
bitfield__read_var_BitField_offset2_count11:
.LLFB82:
	save	%sp, -96, %sp
.LLCFI82:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	bitfield__read_var_BitField_offset2_count11, .-bitfield__read_var_BitField_offset2_count11
	.align 4
	.type	bitfield__write_var_BitField_offset2_count11, #function
	.proc	020
bitfield__write_var_BitField_offset2_count11:
.LLFB83:
	save	%sp, -104, %sp
.LLCFI83:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL166
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL167
	 nop
.LL166:
	sethi	%hi(12288), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL167:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE83:
	.size	bitfield__write_var_BitField_offset2_count11, .-bitfield__write_var_BitField_offset2_count11
	.align 4
	.type	bitfield__read_var_BitField_offset2_count12, #function
	.proc	04
bitfield__read_var_BitField_offset2_count12:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE84:
	.size	bitfield__read_var_BitField_offset2_count12, .-bitfield__read_var_BitField_offset2_count12
	.align 4
	.type	bitfield__write_var_BitField_offset2_count12, #function
	.proc	020
bitfield__write_var_BitField_offset2_count12:
.LLFB85:
	save	%sp, -104, %sp
.LLCFI85:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL170
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL171
	 nop
.LL170:
	sethi	%hi(12288), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL171:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE85:
	.size	bitfield__write_var_BitField_offset2_count12, .-bitfield__write_var_BitField_offset2_count12
	.align 4
	.type	bitfield__read_var_BitField_offset2_count13, #function
	.proc	04
bitfield__read_var_BitField_offset2_count13:
.LLFB86:
	save	%sp, -96, %sp
.LLCFI86:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE86:
	.size	bitfield__read_var_BitField_offset2_count13, .-bitfield__read_var_BitField_offset2_count13
	.align 4
	.type	bitfield__write_var_BitField_offset2_count13, #function
	.proc	020
bitfield__write_var_BitField_offset2_count13:
.LLFB87:
	save	%sp, -104, %sp
.LLCFI87:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL174
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL175
	 nop
.LL174:
	sethi	%hi(12288), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL175:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-16384), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE87:
	.size	bitfield__write_var_BitField_offset2_count13, .-bitfield__write_var_BitField_offset2_count13
	.align 4
	.type	bitfield__read_var_BitField_offset2_count14, #function
	.proc	04
bitfield__read_var_BitField_offset2_count14:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	bitfield__read_var_BitField_offset2_count14, .-bitfield__read_var_BitField_offset2_count14
	.align 4
	.type	bitfield__write_var_BitField_offset2_count14, #function
	.proc	020
bitfield__write_var_BitField_offset2_count14:
.LLFB89:
	save	%sp, -104, %sp
.LLCFI89:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL178
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL179
	 nop
.LL178:
	sethi	%hi(12288), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL179:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	mov	%o5, %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE89:
	.size	bitfield__write_var_BitField_offset2_count14, .-bitfield__write_var_BitField_offset2_count14
	.align 4
	.type	bitfield__read_var_BitField_offset2_count15, #function
	.proc	04
bitfield__read_var_BitField_offset2_count15:
.LLFB90:
	save	%sp, -96, %sp
.LLCFI90:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE90:
	.size	bitfield__read_var_BitField_offset2_count15, .-bitfield__read_var_BitField_offset2_count15
	.align 4
	.type	bitfield__write_var_BitField_offset2_count15, #function
	.proc	020
bitfield__write_var_BitField_offset2_count15:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL182
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL183
	 nop
.LL182:
	sethi	%hi(13312), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL183:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073710080), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE91:
	.size	bitfield__write_var_BitField_offset2_count15, .-bitfield__write_var_BitField_offset2_count15
	.align 4
	.type	bitfield__read_var_BitField_offset2_count16, #function
	.proc	04
bitfield__read_var_BitField_offset2_count16:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	bitfield__read_var_BitField_offset2_count16, .-bitfield__read_var_BitField_offset2_count16
	.align 4
	.type	bitfield__write_var_BitField_offset2_count16, #function
	.proc	020
bitfield__write_var_BitField_offset2_count16:
.LLFB93:
	save	%sp, -104, %sp
.LLCFI93:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL186
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL187
	 nop
.LL186:
	sethi	%hi(13312), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL187:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073726464), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE93:
	.size	bitfield__write_var_BitField_offset2_count16, .-bitfield__write_var_BitField_offset2_count16
	.align 4
	.type	bitfield__read_var_BitField_offset2_count17, #function
	.proc	04
bitfield__read_var_BitField_offset2_count17:
.LLFB94:
	save	%sp, -96, %sp
.LLCFI94:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	bitfield__read_var_BitField_offset2_count17, .-bitfield__read_var_BitField_offset2_count17
	.align 4
	.type	bitfield__write_var_BitField_offset2_count17, #function
	.proc	020
bitfield__write_var_BitField_offset2_count17:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL190
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL191
	 nop
.LL190:
	sethi	%hi(13312), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL191:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073734656), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE95:
	.size	bitfield__write_var_BitField_offset2_count17, .-bitfield__write_var_BitField_offset2_count17
	.align 4
	.type	bitfield__read_var_BitField_offset2_count18, #function
	.proc	04
bitfield__read_var_BitField_offset2_count18:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	bitfield__read_var_BitField_offset2_count18, .-bitfield__read_var_BitField_offset2_count18
	.align 4
	.type	bitfield__write_var_BitField_offset2_count18, #function
	.proc	020
bitfield__write_var_BitField_offset2_count18:
.LLFB97:
	save	%sp, -104, %sp
.LLCFI97:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL194
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL195
	 nop
.LL194:
	sethi	%hi(13312), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL195:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073738752), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE97:
	.size	bitfield__write_var_BitField_offset2_count18, .-bitfield__write_var_BitField_offset2_count18
	.align 4
	.type	bitfield__read_var_BitField_offset2_count19, #function
	.proc	04
bitfield__read_var_BitField_offset2_count19:
.LLFB98:
	save	%sp, -96, %sp
.LLCFI98:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE98:
	.size	bitfield__read_var_BitField_offset2_count19, .-bitfield__read_var_BitField_offset2_count19
	.align 4
	.type	bitfield__write_var_BitField_offset2_count19, #function
	.proc	020
bitfield__write_var_BitField_offset2_count19:
.LLFB99:
	save	%sp, -104, %sp
.LLCFI99:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL198
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL199
	 nop
.LL198:
	sethi	%hi(14336), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL199:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073740800), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE99:
	.size	bitfield__write_var_BitField_offset2_count19, .-bitfield__write_var_BitField_offset2_count19
	.align 4
	.type	bitfield__read_var_BitField_offset2_count20, #function
	.proc	04
bitfield__read_var_BitField_offset2_count20:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE100:
	.size	bitfield__read_var_BitField_offset2_count20, .-bitfield__read_var_BitField_offset2_count20
	.align 4
	.type	bitfield__write_var_BitField_offset2_count20, #function
	.proc	020
bitfield__write_var_BitField_offset2_count20:
.LLFB101:
	save	%sp, -104, %sp
.LLCFI101:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL202
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL203
	 nop
.LL202:
	sethi	%hi(14336), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL203:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE101:
	.size	bitfield__write_var_BitField_offset2_count20, .-bitfield__write_var_BitField_offset2_count20
	.align 4
	.type	bitfield__read_var_BitField_offset2_count21, #function
	.proc	04
bitfield__read_var_BitField_offset2_count21:
.LLFB102:
	save	%sp, -96, %sp
.LLCFI102:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	bitfield__read_var_BitField_offset2_count21, .-bitfield__read_var_BitField_offset2_count21
	.align 4
	.type	bitfield__write_var_BitField_offset2_count21, #function
	.proc	020
bitfield__write_var_BitField_offset2_count21:
.LLFB103:
	save	%sp, -104, %sp
.LLCFI103:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL206
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL207
	 nop
.LL206:
	sethi	%hi(14336), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL207:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE103:
	.size	bitfield__write_var_BitField_offset2_count21, .-bitfield__write_var_BitField_offset2_count21
	.align 4
	.type	bitfield__read_var_BitField_offset2_count22, #function
	.proc	04
bitfield__read_var_BitField_offset2_count22:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	bitfield__read_var_BitField_offset2_count22, .-bitfield__read_var_BitField_offset2_count22
	.align 4
	.type	bitfield__write_var_BitField_offset2_count22, #function
	.proc	020
bitfield__write_var_BitField_offset2_count22:
.LLFB105:
	save	%sp, -104, %sp
.LLCFI105:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL210
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL211
	 nop
.LL210:
	sethi	%hi(14336), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL211:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE105:
	.size	bitfield__write_var_BitField_offset2_count22, .-bitfield__write_var_BitField_offset2_count22
	.align 4
	.type	bitfield__read_var_BitField_offset2_count23, #function
	.proc	04
bitfield__read_var_BitField_offset2_count23:
.LLFB106:
	save	%sp, -96, %sp
.LLCFI106:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	bitfield__read_var_BitField_offset2_count23, .-bitfield__read_var_BitField_offset2_count23
	.align 4
	.type	bitfield__write_var_BitField_offset2_count23, #function
	.proc	020
bitfield__write_var_BitField_offset2_count23:
.LLFB107:
	save	%sp, -104, %sp
.LLCFI107:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL214
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL215
	 nop
.LL214:
	sethi	%hi(15360), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL215:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE107:
	.size	bitfield__write_var_BitField_offset2_count23, .-bitfield__write_var_BitField_offset2_count23
	.align 4
	.type	bitfield__read_var_BitField_offset2_count24, #function
	.proc	04
bitfield__read_var_BitField_offset2_count24:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE108:
	.size	bitfield__read_var_BitField_offset2_count24, .-bitfield__read_var_BitField_offset2_count24
	.align 4
	.type	bitfield__write_var_BitField_offset2_count24, #function
	.proc	020
bitfield__write_var_BitField_offset2_count24:
.LLFB109:
	save	%sp, -104, %sp
.LLCFI109:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL218
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL219
	 nop
.LL218:
	sethi	%hi(15360), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL219:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE109:
	.size	bitfield__write_var_BitField_offset2_count24, .-bitfield__write_var_BitField_offset2_count24
	.align 4
	.type	bitfield__read_var_BitField_offset2_count25, #function
	.proc	04
bitfield__read_var_BitField_offset2_count25:
.LLFB110:
	save	%sp, -96, %sp
.LLCFI110:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE110:
	.size	bitfield__read_var_BitField_offset2_count25, .-bitfield__read_var_BitField_offset2_count25
	.align 4
	.type	bitfield__write_var_BitField_offset2_count25, #function
	.proc	020
bitfield__write_var_BitField_offset2_count25:
.LLFB111:
	save	%sp, -104, %sp
.LLCFI111:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL222
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL223
	 nop
.LL222:
	sethi	%hi(15360), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL223:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE111:
	.size	bitfield__write_var_BitField_offset2_count25, .-bitfield__write_var_BitField_offset2_count25
	.align 4
	.type	bitfield__read_var_BitField_offset2_count26, #function
	.proc	04
bitfield__read_var_BitField_offset2_count26:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	bitfield__read_var_BitField_offset2_count26, .-bitfield__read_var_BitField_offset2_count26
	.align 4
	.type	bitfield__write_var_BitField_offset2_count26, #function
	.proc	020
bitfield__write_var_BitField_offset2_count26:
.LLFB113:
	save	%sp, -104, %sp
.LLCFI113:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL226
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL227
	 nop
.LL226:
	sethi	%hi(15360), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL227:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE113:
	.size	bitfield__write_var_BitField_offset2_count26, .-bitfield__write_var_BitField_offset2_count26
	.align 4
	.type	bitfield__read_var_BitField_offset2_count27, #function
	.proc	04
bitfield__read_var_BitField_offset2_count27:
.LLFB114:
	save	%sp, -96, %sp
.LLCFI114:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE114:
	.size	bitfield__read_var_BitField_offset2_count27, .-bitfield__read_var_BitField_offset2_count27
	.align 4
	.type	bitfield__write_var_BitField_offset2_count27, #function
	.proc	020
bitfield__write_var_BitField_offset2_count27:
.LLFB115:
	save	%sp, -104, %sp
.LLCFI115:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL230
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL231
	 nop
.LL230:
	sethi	%hi(16384), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL231:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE115:
	.size	bitfield__write_var_BitField_offset2_count27, .-bitfield__write_var_BitField_offset2_count27
	.align 4
	.type	bitfield__read_var_BitField_offset2_count28, #function
	.proc	04
bitfield__read_var_BitField_offset2_count28:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	bitfield__read_var_BitField_offset2_count28, .-bitfield__read_var_BitField_offset2_count28
	.align 4
	.type	bitfield__write_var_BitField_offset2_count28, #function
	.proc	020
bitfield__write_var_BitField_offset2_count28:
.LLFB117:
	save	%sp, -104, %sp
.LLCFI117:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL234
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL235
	 nop
.LL234:
	sethi	%hi(16384), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL235:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(268434432), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE117:
	.size	bitfield__write_var_BitField_offset2_count28, .-bitfield__write_var_BitField_offset2_count28
	.align 4
	.type	bitfield__read_var_BitField_offset2_count29, #function
	.proc	04
bitfield__read_var_BitField_offset2_count29:
.LLFB118:
	save	%sp, -96, %sp
.LLCFI118:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 2, %g1
	srl	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE118:
	.size	bitfield__read_var_BitField_offset2_count29, .-bitfield__read_var_BitField_offset2_count29
	.align 4
	.type	bitfield__write_var_BitField_offset2_count29, #function
	.proc	020
bitfield__write_var_BitField_offset2_count29:
.LLFB119:
	save	%sp, -104, %sp
.LLCFI119:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL238
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL239
	 nop
.LL238:
	sethi	%hi(16384), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL239:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-1073741824), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE119:
	.size	bitfield__write_var_BitField_offset2_count29, .-bitfield__write_var_BitField_offset2_count29
	.align 4
	.type	bitfield__read_var_BitField_offset2_count30, #function
	.proc	04
bitfield__read_var_BitField_offset2_count30:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	bitfield__read_var_BitField_offset2_count30, .-bitfield__read_var_BitField_offset2_count30
	.align 4
	.type	bitfield__write_var_BitField_offset2_count30, #function
	.proc	020
bitfield__write_var_BitField_offset2_count30:
.LLFB121:
	save	%sp, -104, %sp
.LLCFI121:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL242
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL243
	 nop
.LL242:
	sethi	%hi(16384), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL243:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1073741824), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1073740800), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE121:
	.size	bitfield__write_var_BitField_offset2_count30, .-bitfield__write_var_BitField_offset2_count30
	.align 4
	.type	bitfield__read_var_BitField_offset3_count1, #function
	.proc	04
bitfield__read_var_BitField_offset3_count1:
.LLFB122:
	save	%sp, -96, %sp
.LLCFI122:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE122:
	.size	bitfield__read_var_BitField_offset3_count1, .-bitfield__read_var_BitField_offset3_count1
	.align 4
	.type	bitfield__write_var_BitField_offset3_count1, #function
	.proc	020
bitfield__write_var_BitField_offset3_count1:
.LLFB123:
	save	%sp, -104, %sp
.LLCFI123:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL246
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL247
	 nop
.LL246:
	sethi	%hi(17408), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL247:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -17, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE123:
	.size	bitfield__write_var_BitField_offset3_count1, .-bitfield__write_var_BitField_offset3_count1
	.align 4
	.type	bitfield__read_var_BitField_offset3_count2, #function
	.proc	04
bitfield__read_var_BitField_offset3_count2:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	bitfield__read_var_BitField_offset3_count2, .-bitfield__read_var_BitField_offset3_count2
	.align 4
	.type	bitfield__write_var_BitField_offset3_count2, #function
	.proc	020
bitfield__write_var_BitField_offset3_count2:
.LLFB125:
	save	%sp, -104, %sp
.LLCFI125:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL250
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL251
	 nop
.LL250:
	sethi	%hi(17408), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL251:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -25, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE125:
	.size	bitfield__write_var_BitField_offset3_count2, .-bitfield__write_var_BitField_offset3_count2
	.align 4
	.type	bitfield__read_var_BitField_offset3_count3, #function
	.proc	04
bitfield__read_var_BitField_offset3_count3:
.LLFB126:
	save	%sp, -96, %sp
.LLCFI126:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	bitfield__read_var_BitField_offset3_count3, .-bitfield__read_var_BitField_offset3_count3
	.align 4
	.type	bitfield__write_var_BitField_offset3_count3, #function
	.proc	020
bitfield__write_var_BitField_offset3_count3:
.LLFB127:
	save	%sp, -104, %sp
.LLCFI127:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL254
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL255
	 nop
.LL254:
	sethi	%hi(17408), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL255:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -29, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE127:
	.size	bitfield__write_var_BitField_offset3_count3, .-bitfield__write_var_BitField_offset3_count3
	.align 4
	.type	bitfield__read_var_BitField_offset3_count4, #function
	.proc	04
bitfield__read_var_BitField_offset3_count4:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	bitfield__read_var_BitField_offset3_count4, .-bitfield__read_var_BitField_offset3_count4
	.align 4
	.type	bitfield__write_var_BitField_offset3_count4, #function
	.proc	020
bitfield__write_var_BitField_offset3_count4:
.LLFB129:
	save	%sp, -104, %sp
.LLCFI129:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL258
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL259
	 nop
.LL258:
	sethi	%hi(17408), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL259:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ldub	[%g1], %g1
	and	%g1, -31, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE129:
	.size	bitfield__write_var_BitField_offset3_count4, .-bitfield__write_var_BitField_offset3_count4
	.align 4
	.type	bitfield__read_var_BitField_offset3_count5, #function
	.proc	04
bitfield__read_var_BitField_offset3_count5:
.LLFB130:
	save	%sp, -96, %sp
.LLCFI130:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	bitfield__read_var_BitField_offset3_count5, .-bitfield__read_var_BitField_offset3_count5
	.align 4
	.type	bitfield__write_var_BitField_offset3_count5, #function
	.proc	020
bitfield__write_var_BitField_offset3_count5:
.LLFB131:
	save	%sp, -104, %sp
.LLCFI131:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL262
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL263
	 nop
.LL262:
	sethi	%hi(18432), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL263:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -32, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE131:
	.size	bitfield__write_var_BitField_offset3_count5, .-bitfield__write_var_BitField_offset3_count5
	.align 4
	.type	bitfield__read_var_BitField_offset3_count6, #function
	.proc	04
bitfield__read_var_BitField_offset3_count6:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE132:
	.size	bitfield__read_var_BitField_offset3_count6, .-bitfield__read_var_BitField_offset3_count6
	.align 4
	.type	bitfield__write_var_BitField_offset3_count6, #function
	.proc	020
bitfield__write_var_BitField_offset3_count6:
.LLFB133:
	save	%sp, -104, %sp
.LLCFI133:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL266
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL267
	 nop
.LL266:
	sethi	%hi(18432), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL267:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE133:
	.size	bitfield__write_var_BitField_offset3_count6, .-bitfield__write_var_BitField_offset3_count6
	.align 4
	.type	bitfield__read_var_BitField_offset3_count7, #function
	.proc	04
bitfield__read_var_BitField_offset3_count7:
.LLFB134:
	save	%sp, -96, %sp
.LLCFI134:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE134:
	.size	bitfield__read_var_BitField_offset3_count7, .-bitfield__read_var_BitField_offset3_count7
	.align 4
	.type	bitfield__write_var_BitField_offset3_count7, #function
	.proc	020
bitfield__write_var_BitField_offset3_count7:
.LLFB135:
	save	%sp, -104, %sp
.LLCFI135:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL270
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL271
	 nop
.LL270:
	sethi	%hi(18432), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL271:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE135:
	.size	bitfield__write_var_BitField_offset3_count7, .-bitfield__write_var_BitField_offset3_count7
	.align 4
	.type	bitfield__read_var_BitField_offset3_count8, #function
	.proc	04
bitfield__read_var_BitField_offset3_count8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	bitfield__read_var_BitField_offset3_count8, .-bitfield__read_var_BitField_offset3_count8
	.align 4
	.type	bitfield__write_var_BitField_offset3_count8, #function
	.proc	020
bitfield__write_var_BitField_offset3_count8:
.LLFB137:
	save	%sp, -104, %sp
.LLCFI137:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL274
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL275
	 nop
.LL274:
	sethi	%hi(18432), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL275:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE137:
	.size	bitfield__write_var_BitField_offset3_count8, .-bitfield__write_var_BitField_offset3_count8
	.align 4
	.type	bitfield__read_var_BitField_offset3_count9, #function
	.proc	04
bitfield__read_var_BitField_offset3_count9:
.LLFB138:
	save	%sp, -96, %sp
.LLCFI138:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE138:
	.size	bitfield__read_var_BitField_offset3_count9, .-bitfield__read_var_BitField_offset3_count9
	.align 4
	.type	bitfield__write_var_BitField_offset3_count9, #function
	.proc	020
bitfield__write_var_BitField_offset3_count9:
.LLFB139:
	save	%sp, -104, %sp
.LLCFI139:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL278
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL279
	 nop
.LL278:
	sethi	%hi(19456), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL279:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE139:
	.size	bitfield__write_var_BitField_offset3_count9, .-bitfield__write_var_BitField_offset3_count9
	.align 4
	.type	bitfield__read_var_BitField_offset3_count10, #function
	.proc	04
bitfield__read_var_BitField_offset3_count10:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	bitfield__read_var_BitField_offset3_count10, .-bitfield__read_var_BitField_offset3_count10
	.align 4
	.type	bitfield__write_var_BitField_offset3_count10, #function
	.proc	020
bitfield__write_var_BitField_offset3_count10:
.LLFB141:
	save	%sp, -104, %sp
.LLCFI141:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL282
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL283
	 nop
.LL282:
	sethi	%hi(19456), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL283:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE141:
	.size	bitfield__write_var_BitField_offset3_count10, .-bitfield__write_var_BitField_offset3_count10
	.align 4
	.type	bitfield__read_var_BitField_offset3_count11, #function
	.proc	04
bitfield__read_var_BitField_offset3_count11:
.LLFB142:
	save	%sp, -96, %sp
.LLCFI142:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	bitfield__read_var_BitField_offset3_count11, .-bitfield__read_var_BitField_offset3_count11
	.align 4
	.type	bitfield__write_var_BitField_offset3_count11, #function
	.proc	020
bitfield__write_var_BitField_offset3_count11:
.LLFB143:
	save	%sp, -104, %sp
.LLCFI143:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL286
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL287
	 nop
.LL286:
	sethi	%hi(19456), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL287:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE143:
	.size	bitfield__write_var_BitField_offset3_count11, .-bitfield__write_var_BitField_offset3_count11
	.align 4
	.type	bitfield__read_var_BitField_offset3_count12, #function
	.proc	04
bitfield__read_var_BitField_offset3_count12:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	bitfield__read_var_BitField_offset3_count12, .-bitfield__read_var_BitField_offset3_count12
	.align 4
	.type	bitfield__write_var_BitField_offset3_count12, #function
	.proc	020
bitfield__write_var_BitField_offset3_count12:
.LLFB145:
	save	%sp, -104, %sp
.LLCFI145:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL290
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL291
	 nop
.LL290:
	sethi	%hi(19456), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL291:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-8192), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 4095, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE145:
	.size	bitfield__write_var_BitField_offset3_count12, .-bitfield__write_var_BitField_offset3_count12
	.align 4
	.type	bitfield__read_var_BitField_offset3_count13, #function
	.proc	04
bitfield__read_var_BitField_offset3_count13:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	bitfield__read_var_BitField_offset3_count13, .-bitfield__read_var_BitField_offset3_count13
	.align 4
	.type	bitfield__write_var_BitField_offset3_count13, #function
	.proc	020
bitfield__write_var_BitField_offset3_count13:
.LLFB147:
	save	%sp, -104, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL294
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL295
	 nop
.LL294:
	sethi	%hi(20480), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL295:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	mov	%o5, %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE147:
	.size	bitfield__write_var_BitField_offset3_count13, .-bitfield__write_var_BitField_offset3_count13
	.align 4
	.type	bitfield__read_var_BitField_offset3_count14, #function
	.proc	04
bitfield__read_var_BitField_offset3_count14:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE148:
	.size	bitfield__read_var_BitField_offset3_count14, .-bitfield__read_var_BitField_offset3_count14
	.align 4
	.type	bitfield__write_var_BitField_offset3_count14, #function
	.proc	020
bitfield__write_var_BitField_offset3_count14:
.LLFB149:
	save	%sp, -104, %sp
.LLCFI149:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL298
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL299
	 nop
.LL298:
	sethi	%hi(20480), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL299:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536839168), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE149:
	.size	bitfield__write_var_BitField_offset3_count14, .-bitfield__write_var_BitField_offset3_count14
	.align 4
	.type	bitfield__read_var_BitField_offset3_count15, #function
	.proc	04
bitfield__read_var_BitField_offset3_count15:
.LLFB150:
	save	%sp, -96, %sp
.LLCFI150:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE150:
	.size	bitfield__read_var_BitField_offset3_count15, .-bitfield__read_var_BitField_offset3_count15
	.align 4
	.type	bitfield__write_var_BitField_offset3_count15, #function
	.proc	020
bitfield__write_var_BitField_offset3_count15:
.LLFB151:
	save	%sp, -104, %sp
.LLCFI151:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL302
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL303
	 nop
.LL302:
	sethi	%hi(20480), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL303:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536855552), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE151:
	.size	bitfield__write_var_BitField_offset3_count15, .-bitfield__write_var_BitField_offset3_count15
	.align 4
	.type	bitfield__read_var_BitField_offset3_count16, #function
	.proc	04
bitfield__read_var_BitField_offset3_count16:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE152:
	.size	bitfield__read_var_BitField_offset3_count16, .-bitfield__read_var_BitField_offset3_count16
	.align 4
	.type	bitfield__write_var_BitField_offset3_count16, #function
	.proc	020
bitfield__write_var_BitField_offset3_count16:
.LLFB153:
	save	%sp, -104, %sp
.LLCFI153:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL306
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL307
	 nop
.LL306:
	sethi	%hi(20480), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL307:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536863744), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE153:
	.size	bitfield__write_var_BitField_offset3_count16, .-bitfield__write_var_BitField_offset3_count16
	.align 4
	.type	bitfield__read_var_BitField_offset3_count17, #function
	.proc	04
bitfield__read_var_BitField_offset3_count17:
.LLFB154:
	save	%sp, -96, %sp
.LLCFI154:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE154:
	.size	bitfield__read_var_BitField_offset3_count17, .-bitfield__read_var_BitField_offset3_count17
	.align 4
	.type	bitfield__write_var_BitField_offset3_count17, #function
	.proc	020
bitfield__write_var_BitField_offset3_count17:
.LLFB155:
	save	%sp, -104, %sp
.LLCFI155:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL310
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL311
	 nop
.LL310:
	sethi	%hi(21504), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL311:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536867840), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE155:
	.size	bitfield__write_var_BitField_offset3_count17, .-bitfield__write_var_BitField_offset3_count17
	.align 4
	.type	bitfield__read_var_BitField_offset3_count18, #function
	.proc	04
bitfield__read_var_BitField_offset3_count18:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	bitfield__read_var_BitField_offset3_count18, .-bitfield__read_var_BitField_offset3_count18
	.align 4
	.type	bitfield__write_var_BitField_offset3_count18, #function
	.proc	020
bitfield__write_var_BitField_offset3_count18:
.LLFB157:
	save	%sp, -104, %sp
.LLCFI157:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL314
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL315
	 nop
.LL314:
	sethi	%hi(21504), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL315:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536869888), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE157:
	.size	bitfield__write_var_BitField_offset3_count18, .-bitfield__write_var_BitField_offset3_count18
	.align 4
	.type	bitfield__read_var_BitField_offset3_count19, #function
	.proc	04
bitfield__read_var_BitField_offset3_count19:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	bitfield__read_var_BitField_offset3_count19, .-bitfield__read_var_BitField_offset3_count19
	.align 4
	.type	bitfield__write_var_BitField_offset3_count19, #function
	.proc	020
bitfield__write_var_BitField_offset3_count19:
.LLFB159:
	save	%sp, -104, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL318
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL319
	 nop
.LL318:
	sethi	%hi(21504), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL319:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE159:
	.size	bitfield__write_var_BitField_offset3_count19, .-bitfield__write_var_BitField_offset3_count19
	.align 4
	.type	bitfield__read_var_BitField_offset3_count20, #function
	.proc	04
bitfield__read_var_BitField_offset3_count20:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	bitfield__read_var_BitField_offset3_count20, .-bitfield__read_var_BitField_offset3_count20
	.align 4
	.type	bitfield__write_var_BitField_offset3_count20, #function
	.proc	020
bitfield__write_var_BitField_offset3_count20:
.LLFB161:
	save	%sp, -104, %sp
.LLCFI161:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL322
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL323
	 nop
.LL322:
	sethi	%hi(21504), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL323:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE161:
	.size	bitfield__write_var_BitField_offset3_count20, .-bitfield__write_var_BitField_offset3_count20
	.align 4
	.type	bitfield__read_var_BitField_offset3_count21, #function
	.proc	04
bitfield__read_var_BitField_offset3_count21:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	bitfield__read_var_BitField_offset3_count21, .-bitfield__read_var_BitField_offset3_count21
	.align 4
	.type	bitfield__write_var_BitField_offset3_count21, #function
	.proc	020
bitfield__write_var_BitField_offset3_count21:
.LLFB163:
	save	%sp, -104, %sp
.LLCFI163:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL326
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL327
	 nop
.LL326:
	sethi	%hi(22528), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL327:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE163:
	.size	bitfield__write_var_BitField_offset3_count21, .-bitfield__write_var_BitField_offset3_count21
	.align 4
	.type	bitfield__read_var_BitField_offset3_count22, #function
	.proc	04
bitfield__read_var_BitField_offset3_count22:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	bitfield__read_var_BitField_offset3_count22, .-bitfield__read_var_BitField_offset3_count22
	.align 4
	.type	bitfield__write_var_BitField_offset3_count22, #function
	.proc	020
bitfield__write_var_BitField_offset3_count22:
.LLFB165:
	save	%sp, -104, %sp
.LLCFI165:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL330
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL331
	 nop
.LL330:
	sethi	%hi(22528), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL331:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE165:
	.size	bitfield__write_var_BitField_offset3_count22, .-bitfield__write_var_BitField_offset3_count22
	.align 4
	.type	bitfield__read_var_BitField_offset3_count23, #function
	.proc	04
bitfield__read_var_BitField_offset3_count23:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	bitfield__read_var_BitField_offset3_count23, .-bitfield__read_var_BitField_offset3_count23
	.align 4
	.type	bitfield__write_var_BitField_offset3_count23, #function
	.proc	020
bitfield__write_var_BitField_offset3_count23:
.LLFB167:
	save	%sp, -104, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL334
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL335
	 nop
.LL334:
	sethi	%hi(22528), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL335:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE167:
	.size	bitfield__write_var_BitField_offset3_count23, .-bitfield__write_var_BitField_offset3_count23
	.align 4
	.type	bitfield__read_var_BitField_offset3_count24, #function
	.proc	04
bitfield__read_var_BitField_offset3_count24:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	bitfield__read_var_BitField_offset3_count24, .-bitfield__read_var_BitField_offset3_count24
	.align 4
	.type	bitfield__write_var_BitField_offset3_count24, #function
	.proc	020
bitfield__write_var_BitField_offset3_count24:
.LLFB169:
	save	%sp, -104, %sp
.LLCFI169:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL338
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL339
	 nop
.LL338:
	sethi	%hi(22528), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL339:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE169:
	.size	bitfield__write_var_BitField_offset3_count24, .-bitfield__write_var_BitField_offset3_count24
	.align 4
	.type	bitfield__read_var_BitField_offset3_count25, #function
	.proc	04
bitfield__read_var_BitField_offset3_count25:
.LLFB170:
	save	%sp, -96, %sp
.LLCFI170:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE170:
	.size	bitfield__read_var_BitField_offset3_count25, .-bitfield__read_var_BitField_offset3_count25
	.align 4
	.type	bitfield__write_var_BitField_offset3_count25, #function
	.proc	020
bitfield__write_var_BitField_offset3_count25:
.LLFB171:
	save	%sp, -104, %sp
.LLCFI171:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL342
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL343
	 nop
.LL342:
	sethi	%hi(23552), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL343:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE171:
	.size	bitfield__write_var_BitField_offset3_count25, .-bitfield__write_var_BitField_offset3_count25
	.align 4
	.type	bitfield__read_var_BitField_offset3_count26, #function
	.proc	04
bitfield__read_var_BitField_offset3_count26:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	bitfield__read_var_BitField_offset3_count26, .-bitfield__read_var_BitField_offset3_count26
	.align 4
	.type	bitfield__write_var_BitField_offset3_count26, #function
	.proc	020
bitfield__write_var_BitField_offset3_count26:
.LLFB173:
	save	%sp, -104, %sp
.LLCFI173:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL346
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL347
	 nop
.LL346:
	sethi	%hi(23552), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL347:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE173:
	.size	bitfield__write_var_BitField_offset3_count26, .-bitfield__write_var_BitField_offset3_count26
	.align 4
	.type	bitfield__read_var_BitField_offset3_count27, #function
	.proc	04
bitfield__read_var_BitField_offset3_count27:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	bitfield__read_var_BitField_offset3_count27, .-bitfield__read_var_BitField_offset3_count27
	.align 4
	.type	bitfield__write_var_BitField_offset3_count27, #function
	.proc	020
bitfield__write_var_BitField_offset3_count27:
.LLFB175:
	save	%sp, -104, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL350
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL351
	 nop
.LL350:
	sethi	%hi(23552), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL351:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE175:
	.size	bitfield__write_var_BitField_offset3_count27, .-bitfield__write_var_BitField_offset3_count27
	.align 4
	.type	bitfield__read_var_BitField_offset3_count28, #function
	.proc	04
bitfield__read_var_BitField_offset3_count28:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 3, %g1
	srl	%g1, 4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	bitfield__read_var_BitField_offset3_count28, .-bitfield__read_var_BitField_offset3_count28
	.align 4
	.type	bitfield__write_var_BitField_offset3_count28, #function
	.proc	020
bitfield__write_var_BitField_offset3_count28:
.LLFB177:
	save	%sp, -104, %sp
.LLCFI177:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL354
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL355
	 nop
.LL354:
	sethi	%hi(23552), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL355:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-536870912), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE177:
	.size	bitfield__write_var_BitField_offset3_count28, .-bitfield__write_var_BitField_offset3_count28
	.align 4
	.type	bitfield__read_var_BitField_offset3_count29, #function
	.proc	04
bitfield__read_var_BitField_offset3_count29:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	bitfield__read_var_BitField_offset3_count29, .-bitfield__read_var_BitField_offset3_count29
	.align 4
	.type	bitfield__write_var_BitField_offset3_count29, #function
	.proc	020
bitfield__write_var_BitField_offset3_count29:
.LLFB179:
	save	%sp, -104, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL358
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL359
	 nop
.LL358:
	sethi	%hi(24576), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL359:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-536870912), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(536869888), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE179:
	.size	bitfield__write_var_BitField_offset3_count29, .-bitfield__write_var_BitField_offset3_count29
	.align 4
	.type	bitfield__read_var_BitField_offset4_count1, #function
	.proc	04
bitfield__read_var_BitField_offset4_count1:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE180:
	.size	bitfield__read_var_BitField_offset4_count1, .-bitfield__read_var_BitField_offset4_count1
	.align 4
	.type	bitfield__write_var_BitField_offset4_count1, #function
	.proc	020
bitfield__write_var_BitField_offset4_count1:
.LLFB181:
	save	%sp, -104, %sp
.LLCFI181:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL362
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL363
	 nop
.LL362:
	sethi	%hi(24576), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL363:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -9, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE181:
	.size	bitfield__write_var_BitField_offset4_count1, .-bitfield__write_var_BitField_offset4_count1
	.align 4
	.type	bitfield__read_var_BitField_offset4_count2, #function
	.proc	04
bitfield__read_var_BitField_offset4_count2:
.LLFB182:
	save	%sp, -96, %sp
.LLCFI182:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	bitfield__read_var_BitField_offset4_count2, .-bitfield__read_var_BitField_offset4_count2
	.align 4
	.type	bitfield__write_var_BitField_offset4_count2, #function
	.proc	020
bitfield__write_var_BitField_offset4_count2:
.LLFB183:
	save	%sp, -104, %sp
.LLCFI183:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL366
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL367
	 nop
.LL366:
	sethi	%hi(24576), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL367:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -13, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE183:
	.size	bitfield__write_var_BitField_offset4_count2, .-bitfield__write_var_BitField_offset4_count2
	.align 4
	.type	bitfield__read_var_BitField_offset4_count3, #function
	.proc	04
bitfield__read_var_BitField_offset4_count3:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	bitfield__read_var_BitField_offset4_count3, .-bitfield__read_var_BitField_offset4_count3
	.align 4
	.type	bitfield__write_var_BitField_offset4_count3, #function
	.proc	020
bitfield__write_var_BitField_offset4_count3:
.LLFB185:
	save	%sp, -104, %sp
.LLCFI185:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL370
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL371
	 nop
.LL370:
	sethi	%hi(24576), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL371:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ldub	[%g1], %g1
	and	%g1, -15, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE185:
	.size	bitfield__write_var_BitField_offset4_count3, .-bitfield__write_var_BitField_offset4_count3
	.align 4
	.type	bitfield__read_var_BitField_offset4_count4, #function
	.proc	04
bitfield__read_var_BitField_offset4_count4:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	bitfield__read_var_BitField_offset4_count4, .-bitfield__read_var_BitField_offset4_count4
	.align 4
	.type	bitfield__write_var_BitField_offset4_count4, #function
	.proc	020
bitfield__write_var_BitField_offset4_count4:
.LLFB187:
	save	%sp, -104, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL374
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL375
	 nop
.LL374:
	sethi	%hi(25600), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL375:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -16, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE187:
	.size	bitfield__write_var_BitField_offset4_count4, .-bitfield__write_var_BitField_offset4_count4
	.align 4
	.type	bitfield__read_var_BitField_offset4_count5, #function
	.proc	04
bitfield__read_var_BitField_offset4_count5:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	bitfield__read_var_BitField_offset4_count5, .-bitfield__read_var_BitField_offset4_count5
	.align 4
	.type	bitfield__write_var_BitField_offset4_count5, #function
	.proc	020
bitfield__write_var_BitField_offset4_count5:
.LLFB189:
	save	%sp, -104, %sp
.LLCFI189:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL378
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL379
	 nop
.LL378:
	sethi	%hi(25600), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL379:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -3969, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE189:
	.size	bitfield__write_var_BitField_offset4_count5, .-bitfield__write_var_BitField_offset4_count5
	.align 4
	.type	bitfield__read_var_BitField_offset4_count6, #function
	.proc	04
bitfield__read_var_BitField_offset4_count6:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	bitfield__read_var_BitField_offset4_count6, .-bitfield__read_var_BitField_offset4_count6
	.align 4
	.type	bitfield__write_var_BitField_offset4_count6, #function
	.proc	020
bitfield__write_var_BitField_offset4_count6:
.LLFB191:
	save	%sp, -104, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL382
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL383
	 nop
.LL382:
	sethi	%hi(25600), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL383:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -4033, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE191:
	.size	bitfield__write_var_BitField_offset4_count6, .-bitfield__write_var_BitField_offset4_count6
	.align 4
	.type	bitfield__read_var_BitField_offset4_count7, #function
	.proc	04
bitfield__read_var_BitField_offset4_count7:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	bitfield__read_var_BitField_offset4_count7, .-bitfield__read_var_BitField_offset4_count7
	.align 4
	.type	bitfield__write_var_BitField_offset4_count7, #function
	.proc	020
bitfield__write_var_BitField_offset4_count7:
.LLFB193:
	save	%sp, -104, %sp
.LLCFI193:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL386
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL387
	 nop
.LL386:
	sethi	%hi(25600), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL387:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -4065, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE193:
	.size	bitfield__write_var_BitField_offset4_count7, .-bitfield__write_var_BitField_offset4_count7
	.align 4
	.type	bitfield__read_var_BitField_offset4_count8, #function
	.proc	04
bitfield__read_var_BitField_offset4_count8:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	bitfield__read_var_BitField_offset4_count8, .-bitfield__read_var_BitField_offset4_count8
	.align 4
	.type	bitfield__write_var_BitField_offset4_count8, #function
	.proc	020
bitfield__write_var_BitField_offset4_count8:
.LLFB195:
	save	%sp, -104, %sp
.LLCFI195:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL390
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL391
	 nop
.LL390:
	sethi	%hi(26624), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL391:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -4081, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE195:
	.size	bitfield__write_var_BitField_offset4_count8, .-bitfield__write_var_BitField_offset4_count8
	.align 4
	.type	bitfield__read_var_BitField_offset4_count9, #function
	.proc	04
bitfield__read_var_BitField_offset4_count9:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE196:
	.size	bitfield__read_var_BitField_offset4_count9, .-bitfield__read_var_BitField_offset4_count9
	.align 4
	.type	bitfield__write_var_BitField_offset4_count9, #function
	.proc	020
bitfield__write_var_BitField_offset4_count9:
.LLFB197:
	save	%sp, -104, %sp
.LLCFI197:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL394
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL395
	 nop
.LL394:
	sethi	%hi(26624), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL395:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -4089, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE197:
	.size	bitfield__write_var_BitField_offset4_count9, .-bitfield__write_var_BitField_offset4_count9
	.align 4
	.type	bitfield__read_var_BitField_offset4_count10, #function
	.proc	04
bitfield__read_var_BitField_offset4_count10:
.LLFB198:
	save	%sp, -96, %sp
.LLCFI198:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	bitfield__read_var_BitField_offset4_count10, .-bitfield__read_var_BitField_offset4_count10
	.align 4
	.type	bitfield__write_var_BitField_offset4_count10, #function
	.proc	020
bitfield__write_var_BitField_offset4_count10:
.LLFB199:
	save	%sp, -104, %sp
.LLCFI199:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL398
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL399
	 nop
.LL398:
	sethi	%hi(26624), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL399:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -4093, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE199:
	.size	bitfield__write_var_BitField_offset4_count10, .-bitfield__write_var_BitField_offset4_count10
	.align 4
	.type	bitfield__read_var_BitField_offset4_count11, #function
	.proc	04
bitfield__read_var_BitField_offset4_count11:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	bitfield__read_var_BitField_offset4_count11, .-bitfield__read_var_BitField_offset4_count11
	.align 4
	.type	bitfield__write_var_BitField_offset4_count11, #function
	.proc	020
bitfield__write_var_BitField_offset4_count11:
.LLFB201:
	save	%sp, -104, %sp
.LLCFI201:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL402
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL403
	 nop
.LL402:
	sethi	%hi(26624), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL403:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	and	%g1, -4095, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 2047, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE201:
	.size	bitfield__write_var_BitField_offset4_count11, .-bitfield__write_var_BitField_offset4_count11
	.align 4
	.type	bitfield__read_var_BitField_offset4_count12, #function
	.proc	04
bitfield__read_var_BitField_offset4_count12:
.LLFB202:
	save	%sp, -96, %sp
.LLCFI202:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE202:
	.size	bitfield__read_var_BitField_offset4_count12, .-bitfield__read_var_BitField_offset4_count12
	.align 4
	.type	bitfield__write_var_BitField_offset4_count12, #function
	.proc	020
bitfield__write_var_BitField_offset4_count12:
.LLFB203:
	save	%sp, -104, %sp
.LLCFI203:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL406
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL407
	 nop
.LL406:
	sethi	%hi(27648), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL407:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -4096, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE203:
	.size	bitfield__write_var_BitField_offset4_count12, .-bitfield__write_var_BitField_offset4_count12
	.align 4
	.type	bitfield__read_var_BitField_offset4_count13, #function
	.proc	04
bitfield__read_var_BitField_offset4_count13:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	bitfield__read_var_BitField_offset4_count13, .-bitfield__read_var_BitField_offset4_count13
	.align 4
	.type	bitfield__write_var_BitField_offset4_count13, #function
	.proc	020
bitfield__write_var_BitField_offset4_count13:
.LLFB205:
	save	%sp, -104, %sp
.LLCFI205:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL410
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL411
	 nop
.LL410:
	sethi	%hi(27648), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL411:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268403712), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE205:
	.size	bitfield__write_var_BitField_offset4_count13, .-bitfield__write_var_BitField_offset4_count13
	.align 4
	.type	bitfield__read_var_BitField_offset4_count14, #function
	.proc	04
bitfield__read_var_BitField_offset4_count14:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	bitfield__read_var_BitField_offset4_count14, .-bitfield__read_var_BitField_offset4_count14
	.align 4
	.type	bitfield__write_var_BitField_offset4_count14, #function
	.proc	020
bitfield__write_var_BitField_offset4_count14:
.LLFB207:
	save	%sp, -104, %sp
.LLCFI207:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL414
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL415
	 nop
.LL414:
	sethi	%hi(27648), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL415:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268420096), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE207:
	.size	bitfield__write_var_BitField_offset4_count14, .-bitfield__write_var_BitField_offset4_count14
	.align 4
	.type	bitfield__read_var_BitField_offset4_count15, #function
	.proc	04
bitfield__read_var_BitField_offset4_count15:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	bitfield__read_var_BitField_offset4_count15, .-bitfield__read_var_BitField_offset4_count15
	.align 4
	.type	bitfield__write_var_BitField_offset4_count15, #function
	.proc	020
bitfield__write_var_BitField_offset4_count15:
.LLFB209:
	save	%sp, -104, %sp
.LLCFI209:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL418
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL419
	 nop
.LL418:
	sethi	%hi(27648), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL419:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268428288), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE209:
	.size	bitfield__write_var_BitField_offset4_count15, .-bitfield__write_var_BitField_offset4_count15
	.align 4
	.type	bitfield__read_var_BitField_offset4_count16, #function
	.proc	04
bitfield__read_var_BitField_offset4_count16:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	bitfield__read_var_BitField_offset4_count16, .-bitfield__read_var_BitField_offset4_count16
	.align 4
	.type	bitfield__write_var_BitField_offset4_count16, #function
	.proc	020
bitfield__write_var_BitField_offset4_count16:
.LLFB211:
	save	%sp, -104, %sp
.LLCFI211:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL422
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL423
	 nop
.LL422:
	sethi	%hi(28672), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL423:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268432384), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE211:
	.size	bitfield__write_var_BitField_offset4_count16, .-bitfield__write_var_BitField_offset4_count16
	.align 4
	.type	bitfield__read_var_BitField_offset4_count17, #function
	.proc	04
bitfield__read_var_BitField_offset4_count17:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	bitfield__read_var_BitField_offset4_count17, .-bitfield__read_var_BitField_offset4_count17
	.align 4
	.type	bitfield__write_var_BitField_offset4_count17, #function
	.proc	020
bitfield__write_var_BitField_offset4_count17:
.LLFB213:
	save	%sp, -104, %sp
.LLCFI213:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL426
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL427
	 nop
.LL426:
	sethi	%hi(28672), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL427:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268434432), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE213:
	.size	bitfield__write_var_BitField_offset4_count17, .-bitfield__write_var_BitField_offset4_count17
	.align 4
	.type	bitfield__read_var_BitField_offset4_count18, #function
	.proc	04
bitfield__read_var_BitField_offset4_count18:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	bitfield__read_var_BitField_offset4_count18, .-bitfield__read_var_BitField_offset4_count18
	.align 4
	.type	bitfield__write_var_BitField_offset4_count18, #function
	.proc	020
bitfield__write_var_BitField_offset4_count18:
.LLFB215:
	save	%sp, -104, %sp
.LLCFI215:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL430
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL431
	 nop
.LL430:
	sethi	%hi(28672), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL431:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE215:
	.size	bitfield__write_var_BitField_offset4_count18, .-bitfield__write_var_BitField_offset4_count18
	.align 4
	.type	bitfield__read_var_BitField_offset4_count19, #function
	.proc	04
bitfield__read_var_BitField_offset4_count19:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE216:
	.size	bitfield__read_var_BitField_offset4_count19, .-bitfield__read_var_BitField_offset4_count19
	.align 4
	.type	bitfield__write_var_BitField_offset4_count19, #function
	.proc	020
bitfield__write_var_BitField_offset4_count19:
.LLFB217:
	save	%sp, -104, %sp
.LLCFI217:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL434
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL435
	 nop
.LL434:
	sethi	%hi(28672), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL435:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE217:
	.size	bitfield__write_var_BitField_offset4_count19, .-bitfield__write_var_BitField_offset4_count19
	.align 4
	.type	bitfield__read_var_BitField_offset4_count20, #function
	.proc	04
bitfield__read_var_BitField_offset4_count20:
.LLFB218:
	save	%sp, -96, %sp
.LLCFI218:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE218:
	.size	bitfield__read_var_BitField_offset4_count20, .-bitfield__read_var_BitField_offset4_count20
	.align 4
	.type	bitfield__write_var_BitField_offset4_count20, #function
	.proc	020
bitfield__write_var_BitField_offset4_count20:
.LLFB219:
	save	%sp, -104, %sp
.LLCFI219:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL438
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL439
	 nop
.LL438:
	sethi	%hi(29696), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL439:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE219:
	.size	bitfield__write_var_BitField_offset4_count20, .-bitfield__write_var_BitField_offset4_count20
	.align 4
	.type	bitfield__read_var_BitField_offset4_count21, #function
	.proc	04
bitfield__read_var_BitField_offset4_count21:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	bitfield__read_var_BitField_offset4_count21, .-bitfield__read_var_BitField_offset4_count21
	.align 4
	.type	bitfield__write_var_BitField_offset4_count21, #function
	.proc	020
bitfield__write_var_BitField_offset4_count21:
.LLFB221:
	save	%sp, -104, %sp
.LLCFI221:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL442
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL443
	 nop
.LL442:
	sethi	%hi(29696), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL443:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE221:
	.size	bitfield__write_var_BitField_offset4_count21, .-bitfield__write_var_BitField_offset4_count21
	.align 4
	.type	bitfield__read_var_BitField_offset4_count22, #function
	.proc	04
bitfield__read_var_BitField_offset4_count22:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	bitfield__read_var_BitField_offset4_count22, .-bitfield__read_var_BitField_offset4_count22
	.align 4
	.type	bitfield__write_var_BitField_offset4_count22, #function
	.proc	020
bitfield__write_var_BitField_offset4_count22:
.LLFB223:
	save	%sp, -104, %sp
.LLCFI223:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL446
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL447
	 nop
.LL446:
	sethi	%hi(29696), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL447:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE223:
	.size	bitfield__write_var_BitField_offset4_count22, .-bitfield__write_var_BitField_offset4_count22
	.align 4
	.type	bitfield__read_var_BitField_offset4_count23, #function
	.proc	04
bitfield__read_var_BitField_offset4_count23:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	bitfield__read_var_BitField_offset4_count23, .-bitfield__read_var_BitField_offset4_count23
	.align 4
	.type	bitfield__write_var_BitField_offset4_count23, #function
	.proc	020
bitfield__write_var_BitField_offset4_count23:
.LLFB225:
	save	%sp, -104, %sp
.LLCFI225:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL450
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL451
	 nop
.LL450:
	sethi	%hi(29696), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL451:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE225:
	.size	bitfield__write_var_BitField_offset4_count23, .-bitfield__write_var_BitField_offset4_count23
	.align 4
	.type	bitfield__read_var_BitField_offset4_count24, #function
	.proc	04
bitfield__read_var_BitField_offset4_count24:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	bitfield__read_var_BitField_offset4_count24, .-bitfield__read_var_BitField_offset4_count24
	.align 4
	.type	bitfield__write_var_BitField_offset4_count24, #function
	.proc	020
bitfield__write_var_BitField_offset4_count24:
.LLFB227:
	save	%sp, -104, %sp
.LLCFI227:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL454
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL455
	 nop
.LL454:
	sethi	%hi(30720), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL455:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE227:
	.size	bitfield__write_var_BitField_offset4_count24, .-bitfield__write_var_BitField_offset4_count24
	.align 4
	.type	bitfield__read_var_BitField_offset4_count25, #function
	.proc	04
bitfield__read_var_BitField_offset4_count25:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE228:
	.size	bitfield__read_var_BitField_offset4_count25, .-bitfield__read_var_BitField_offset4_count25
	.align 4
	.type	bitfield__write_var_BitField_offset4_count25, #function
	.proc	020
bitfield__write_var_BitField_offset4_count25:
.LLFB229:
	save	%sp, -104, %sp
.LLCFI229:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL458
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL459
	 nop
.LL458:
	sethi	%hi(30720), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL459:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE229:
	.size	bitfield__write_var_BitField_offset4_count25, .-bitfield__write_var_BitField_offset4_count25
	.align 4
	.type	bitfield__read_var_BitField_offset4_count26, #function
	.proc	04
bitfield__read_var_BitField_offset4_count26:
.LLFB230:
	save	%sp, -96, %sp
.LLCFI230:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE230:
	.size	bitfield__read_var_BitField_offset4_count26, .-bitfield__read_var_BitField_offset4_count26
	.align 4
	.type	bitfield__write_var_BitField_offset4_count26, #function
	.proc	020
bitfield__write_var_BitField_offset4_count26:
.LLFB231:
	save	%sp, -104, %sp
.LLCFI231:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL462
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL463
	 nop
.LL462:
	sethi	%hi(30720), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL463:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE231:
	.size	bitfield__write_var_BitField_offset4_count26, .-bitfield__write_var_BitField_offset4_count26
	.align 4
	.type	bitfield__read_var_BitField_offset4_count27, #function
	.proc	04
bitfield__read_var_BitField_offset4_count27:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 4, %g1
	srl	%g1, 5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	bitfield__read_var_BitField_offset4_count27, .-bitfield__read_var_BitField_offset4_count27
	.align 4
	.type	bitfield__write_var_BitField_offset4_count27, #function
	.proc	020
bitfield__write_var_BitField_offset4_count27:
.LLFB233:
	save	%sp, -104, %sp
.LLCFI233:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL466
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL467
	 nop
.LL466:
	sethi	%hi(30720), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL467:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-268435456), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE233:
	.size	bitfield__write_var_BitField_offset4_count27, .-bitfield__write_var_BitField_offset4_count27
	.align 4
	.type	bitfield__read_var_BitField_offset4_count28, #function
	.proc	04
bitfield__read_var_BitField_offset4_count28:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	bitfield__read_var_BitField_offset4_count28, .-bitfield__read_var_BitField_offset4_count28
	.align 4
	.type	bitfield__write_var_BitField_offset4_count28, #function
	.proc	020
bitfield__write_var_BitField_offset4_count28:
.LLFB235:
	save	%sp, -104, %sp
.LLCFI235:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL470
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL471
	 nop
.LL470:
	sethi	%hi(31744), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL471:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-268435456), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(268434432), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE235:
	.size	bitfield__write_var_BitField_offset4_count28, .-bitfield__write_var_BitField_offset4_count28
	.align 4
	.type	bitfield__read_var_BitField_offset5_count1, #function
	.proc	04
bitfield__read_var_BitField_offset5_count1:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	bitfield__read_var_BitField_offset5_count1, .-bitfield__read_var_BitField_offset5_count1
	.align 4
	.type	bitfield__write_var_BitField_offset5_count1, #function
	.proc	020
bitfield__write_var_BitField_offset5_count1:
.LLFB237:
	save	%sp, -104, %sp
.LLCFI237:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL474
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL475
	 nop
.LL474:
	sethi	%hi(31744), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL475:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE237:
	.size	bitfield__write_var_BitField_offset5_count1, .-bitfield__write_var_BitField_offset5_count1
	.align 4
	.type	bitfield__read_var_BitField_offset5_count2, #function
	.proc	04
bitfield__read_var_BitField_offset5_count2:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	bitfield__read_var_BitField_offset5_count2, .-bitfield__read_var_BitField_offset5_count2
	.align 4
	.type	bitfield__write_var_BitField_offset5_count2, #function
	.proc	020
bitfield__write_var_BitField_offset5_count2:
.LLFB239:
	save	%sp, -104, %sp
.LLCFI239:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL478
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL479
	 nop
.LL478:
	sethi	%hi(31744), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL479:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ldub	[%g1], %g1
	and	%g1, -7, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE239:
	.size	bitfield__write_var_BitField_offset5_count2, .-bitfield__write_var_BitField_offset5_count2
	.align 4
	.type	bitfield__read_var_BitField_offset5_count3, #function
	.proc	04
bitfield__read_var_BitField_offset5_count3:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	bitfield__read_var_BitField_offset5_count3, .-bitfield__read_var_BitField_offset5_count3
	.align 4
	.type	bitfield__write_var_BitField_offset5_count3, #function
	.proc	020
bitfield__write_var_BitField_offset5_count3:
.LLFB241:
	save	%sp, -104, %sp
.LLCFI241:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL482
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL483
	 nop
.LL482:
	sethi	%hi(31744), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL483:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -8, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE241:
	.size	bitfield__write_var_BitField_offset5_count3, .-bitfield__write_var_BitField_offset5_count3
	.align 4
	.type	bitfield__read_var_BitField_offset5_count4, #function
	.proc	04
bitfield__read_var_BitField_offset5_count4:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	bitfield__read_var_BitField_offset5_count4, .-bitfield__read_var_BitField_offset5_count4
	.align 4
	.type	bitfield__write_var_BitField_offset5_count4, #function
	.proc	020
bitfield__write_var_BitField_offset5_count4:
.LLFB243:
	save	%sp, -104, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL486
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL487
	 nop
.LL486:
	sethi	%hi(32768), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL487:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -1921, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE243:
	.size	bitfield__write_var_BitField_offset5_count4, .-bitfield__write_var_BitField_offset5_count4
	.align 4
	.type	bitfield__read_var_BitField_offset5_count5, #function
	.proc	04
bitfield__read_var_BitField_offset5_count5:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE244:
	.size	bitfield__read_var_BitField_offset5_count5, .-bitfield__read_var_BitField_offset5_count5
	.align 4
	.type	bitfield__write_var_BitField_offset5_count5, #function
	.proc	020
bitfield__write_var_BitField_offset5_count5:
.LLFB245:
	save	%sp, -104, %sp
.LLCFI245:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL490
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL491
	 nop
.LL490:
	sethi	%hi(32768), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL491:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -1985, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE245:
	.size	bitfield__write_var_BitField_offset5_count5, .-bitfield__write_var_BitField_offset5_count5
	.align 4
	.type	bitfield__read_var_BitField_offset5_count6, #function
	.proc	04
bitfield__read_var_BitField_offset5_count6:
.LLFB246:
	save	%sp, -96, %sp
.LLCFI246:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	bitfield__read_var_BitField_offset5_count6, .-bitfield__read_var_BitField_offset5_count6
	.align 4
	.type	bitfield__write_var_BitField_offset5_count6, #function
	.proc	020
bitfield__write_var_BitField_offset5_count6:
.LLFB247:
	save	%sp, -104, %sp
.LLCFI247:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL494
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL495
	 nop
.LL494:
	sethi	%hi(32768), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL495:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -2017, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE247:
	.size	bitfield__write_var_BitField_offset5_count6, .-bitfield__write_var_BitField_offset5_count6
	.align 4
	.type	bitfield__read_var_BitField_offset5_count7, #function
	.proc	04
bitfield__read_var_BitField_offset5_count7:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	bitfield__read_var_BitField_offset5_count7, .-bitfield__read_var_BitField_offset5_count7
	.align 4
	.type	bitfield__write_var_BitField_offset5_count7, #function
	.proc	020
bitfield__write_var_BitField_offset5_count7:
.LLFB249:
	save	%sp, -104, %sp
.LLCFI249:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL498
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL499
	 nop
.LL498:
	sethi	%hi(32768), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL499:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -2033, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE249:
	.size	bitfield__write_var_BitField_offset5_count7, .-bitfield__write_var_BitField_offset5_count7
	.align 4
	.type	bitfield__read_var_BitField_offset5_count8, #function
	.proc	04
bitfield__read_var_BitField_offset5_count8:
.LLFB250:
	save	%sp, -96, %sp
.LLCFI250:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE250:
	.size	bitfield__read_var_BitField_offset5_count8, .-bitfield__read_var_BitField_offset5_count8
	.align 4
	.type	bitfield__write_var_BitField_offset5_count8, #function
	.proc	020
bitfield__write_var_BitField_offset5_count8:
.LLFB251:
	save	%sp, -104, %sp
.LLCFI251:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL502
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL503
	 nop
.LL502:
	sethi	%hi(33792), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL503:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -2041, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE251:
	.size	bitfield__write_var_BitField_offset5_count8, .-bitfield__write_var_BitField_offset5_count8
	.align 4
	.type	bitfield__read_var_BitField_offset5_count9, #function
	.proc	04
bitfield__read_var_BitField_offset5_count9:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	bitfield__read_var_BitField_offset5_count9, .-bitfield__read_var_BitField_offset5_count9
	.align 4
	.type	bitfield__write_var_BitField_offset5_count9, #function
	.proc	020
bitfield__write_var_BitField_offset5_count9:
.LLFB253:
	save	%sp, -104, %sp
.LLCFI253:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL506
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL507
	 nop
.LL506:
	sethi	%hi(33792), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL507:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -2045, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE253:
	.size	bitfield__write_var_BitField_offset5_count9, .-bitfield__write_var_BitField_offset5_count9
	.align 4
	.type	bitfield__read_var_BitField_offset5_count10, #function
	.proc	04
bitfield__read_var_BitField_offset5_count10:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	bitfield__read_var_BitField_offset5_count10, .-bitfield__read_var_BitField_offset5_count10
	.align 4
	.type	bitfield__write_var_BitField_offset5_count10, #function
	.proc	020
bitfield__write_var_BitField_offset5_count10:
.LLFB255:
	save	%sp, -104, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL510
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL511
	 nop
.LL510:
	sethi	%hi(33792), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL511:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	and	%g1, -2047, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 1023, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE255:
	.size	bitfield__write_var_BitField_offset5_count10, .-bitfield__write_var_BitField_offset5_count10
	.align 4
	.type	bitfield__read_var_BitField_offset5_count11, #function
	.proc	04
bitfield__read_var_BitField_offset5_count11:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	bitfield__read_var_BitField_offset5_count11, .-bitfield__read_var_BitField_offset5_count11
	.align 4
	.type	bitfield__write_var_BitField_offset5_count11, #function
	.proc	020
bitfield__write_var_BitField_offset5_count11:
.LLFB257:
	save	%sp, -104, %sp
.LLCFI257:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL514
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL515
	 nop
.LL514:
	sethi	%hi(33792), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL515:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -2048, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE257:
	.size	bitfield__write_var_BitField_offset5_count11, .-bitfield__write_var_BitField_offset5_count11
	.align 4
	.type	bitfield__read_var_BitField_offset5_count12, #function
	.proc	04
bitfield__read_var_BitField_offset5_count12:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	bitfield__read_var_BitField_offset5_count12, .-bitfield__read_var_BitField_offset5_count12
	.align 4
	.type	bitfield__write_var_BitField_offset5_count12, #function
	.proc	020
bitfield__write_var_BitField_offset5_count12:
.LLFB259:
	save	%sp, -104, %sp
.LLCFI259:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL518
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL519
	 nop
.LL518:
	sethi	%hi(34816), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL519:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134185984), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE259:
	.size	bitfield__write_var_BitField_offset5_count12, .-bitfield__write_var_BitField_offset5_count12
	.align 4
	.type	bitfield__read_var_BitField_offset5_count13, #function
	.proc	04
bitfield__read_var_BitField_offset5_count13:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	bitfield__read_var_BitField_offset5_count13, .-bitfield__read_var_BitField_offset5_count13
	.align 4
	.type	bitfield__write_var_BitField_offset5_count13, #function
	.proc	020
bitfield__write_var_BitField_offset5_count13:
.LLFB261:
	save	%sp, -104, %sp
.LLCFI261:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL522
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL523
	 nop
.LL522:
	sethi	%hi(34816), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL523:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134202368), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE261:
	.size	bitfield__write_var_BitField_offset5_count13, .-bitfield__write_var_BitField_offset5_count13
	.align 4
	.type	bitfield__read_var_BitField_offset5_count14, #function
	.proc	04
bitfield__read_var_BitField_offset5_count14:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	bitfield__read_var_BitField_offset5_count14, .-bitfield__read_var_BitField_offset5_count14
	.align 4
	.type	bitfield__write_var_BitField_offset5_count14, #function
	.proc	020
bitfield__write_var_BitField_offset5_count14:
.LLFB263:
	save	%sp, -104, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL526
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL527
	 nop
.LL526:
	sethi	%hi(34816), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL527:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134210560), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE263:
	.size	bitfield__write_var_BitField_offset5_count14, .-bitfield__write_var_BitField_offset5_count14
	.align 4
	.type	bitfield__read_var_BitField_offset5_count15, #function
	.proc	04
bitfield__read_var_BitField_offset5_count15:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	bitfield__read_var_BitField_offset5_count15, .-bitfield__read_var_BitField_offset5_count15
	.align 4
	.type	bitfield__write_var_BitField_offset5_count15, #function
	.proc	020
bitfield__write_var_BitField_offset5_count15:
.LLFB265:
	save	%sp, -104, %sp
.LLCFI265:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL530
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL531
	 nop
.LL530:
	sethi	%hi(34816), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL531:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134214656), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE265:
	.size	bitfield__write_var_BitField_offset5_count15, .-bitfield__write_var_BitField_offset5_count15
	.align 4
	.type	bitfield__read_var_BitField_offset5_count16, #function
	.proc	04
bitfield__read_var_BitField_offset5_count16:
.LLFB266:
	save	%sp, -96, %sp
.LLCFI266:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE266:
	.size	bitfield__read_var_BitField_offset5_count16, .-bitfield__read_var_BitField_offset5_count16
	.align 4
	.type	bitfield__write_var_BitField_offset5_count16, #function
	.proc	020
bitfield__write_var_BitField_offset5_count16:
.LLFB267:
	save	%sp, -104, %sp
.LLCFI267:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL534
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL535
	 nop
.LL534:
	sethi	%hi(35840), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL535:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134216704), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE267:
	.size	bitfield__write_var_BitField_offset5_count16, .-bitfield__write_var_BitField_offset5_count16
	.align 4
	.type	bitfield__read_var_BitField_offset5_count17, #function
	.proc	04
bitfield__read_var_BitField_offset5_count17:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	bitfield__read_var_BitField_offset5_count17, .-bitfield__read_var_BitField_offset5_count17
	.align 4
	.type	bitfield__write_var_BitField_offset5_count17, #function
	.proc	020
bitfield__write_var_BitField_offset5_count17:
.LLFB269:
	save	%sp, -104, %sp
.LLCFI269:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL538
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL539
	 nop
.LL538:
	sethi	%hi(35840), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL539:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE269:
	.size	bitfield__write_var_BitField_offset5_count17, .-bitfield__write_var_BitField_offset5_count17
	.align 4
	.type	bitfield__read_var_BitField_offset5_count18, #function
	.proc	04
bitfield__read_var_BitField_offset5_count18:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	bitfield__read_var_BitField_offset5_count18, .-bitfield__read_var_BitField_offset5_count18
	.align 4
	.type	bitfield__write_var_BitField_offset5_count18, #function
	.proc	020
bitfield__write_var_BitField_offset5_count18:
.LLFB271:
	save	%sp, -104, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL542
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL543
	 nop
.LL542:
	sethi	%hi(35840), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL543:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE271:
	.size	bitfield__write_var_BitField_offset5_count18, .-bitfield__write_var_BitField_offset5_count18
	.align 4
	.type	bitfield__read_var_BitField_offset5_count19, #function
	.proc	04
bitfield__read_var_BitField_offset5_count19:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	bitfield__read_var_BitField_offset5_count19, .-bitfield__read_var_BitField_offset5_count19
	.align 4
	.type	bitfield__write_var_BitField_offset5_count19, #function
	.proc	020
bitfield__write_var_BitField_offset5_count19:
.LLFB273:
	save	%sp, -104, %sp
.LLCFI273:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL546
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL547
	 nop
.LL546:
	sethi	%hi(35840), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL547:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE273:
	.size	bitfield__write_var_BitField_offset5_count19, .-bitfield__write_var_BitField_offset5_count19
	.align 4
	.type	bitfield__read_var_BitField_offset5_count20, #function
	.proc	04
bitfield__read_var_BitField_offset5_count20:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	bitfield__read_var_BitField_offset5_count20, .-bitfield__read_var_BitField_offset5_count20
	.align 4
	.type	bitfield__write_var_BitField_offset5_count20, #function
	.proc	020
bitfield__write_var_BitField_offset5_count20:
.LLFB275:
	save	%sp, -104, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL550
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL551
	 nop
.LL550:
	sethi	%hi(36864), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL551:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE275:
	.size	bitfield__write_var_BitField_offset5_count20, .-bitfield__write_var_BitField_offset5_count20
	.align 4
	.type	bitfield__read_var_BitField_offset5_count21, #function
	.proc	04
bitfield__read_var_BitField_offset5_count21:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE276:
	.size	bitfield__read_var_BitField_offset5_count21, .-bitfield__read_var_BitField_offset5_count21
	.align 4
	.type	bitfield__write_var_BitField_offset5_count21, #function
	.proc	020
bitfield__write_var_BitField_offset5_count21:
.LLFB277:
	save	%sp, -104, %sp
.LLCFI277:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL554
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL555
	 nop
.LL554:
	sethi	%hi(36864), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL555:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE277:
	.size	bitfield__write_var_BitField_offset5_count21, .-bitfield__write_var_BitField_offset5_count21
	.align 4
	.type	bitfield__read_var_BitField_offset5_count22, #function
	.proc	04
bitfield__read_var_BitField_offset5_count22:
.LLFB278:
	save	%sp, -96, %sp
.LLCFI278:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	bitfield__read_var_BitField_offset5_count22, .-bitfield__read_var_BitField_offset5_count22
	.align 4
	.type	bitfield__write_var_BitField_offset5_count22, #function
	.proc	020
bitfield__write_var_BitField_offset5_count22:
.LLFB279:
	save	%sp, -104, %sp
.LLCFI279:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL558
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL559
	 nop
.LL558:
	sethi	%hi(36864), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL559:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE279:
	.size	bitfield__write_var_BitField_offset5_count22, .-bitfield__write_var_BitField_offset5_count22
	.align 4
	.type	bitfield__read_var_BitField_offset5_count23, #function
	.proc	04
bitfield__read_var_BitField_offset5_count23:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	bitfield__read_var_BitField_offset5_count23, .-bitfield__read_var_BitField_offset5_count23
	.align 4
	.type	bitfield__write_var_BitField_offset5_count23, #function
	.proc	020
bitfield__write_var_BitField_offset5_count23:
.LLFB281:
	save	%sp, -104, %sp
.LLCFI281:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL562
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL563
	 nop
.LL562:
	sethi	%hi(36864), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL563:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE281:
	.size	bitfield__write_var_BitField_offset5_count23, .-bitfield__write_var_BitField_offset5_count23
	.align 4
	.type	bitfield__read_var_BitField_offset5_count24, #function
	.proc	04
bitfield__read_var_BitField_offset5_count24:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	bitfield__read_var_BitField_offset5_count24, .-bitfield__read_var_BitField_offset5_count24
	.align 4
	.type	bitfield__write_var_BitField_offset5_count24, #function
	.proc	020
bitfield__write_var_BitField_offset5_count24:
.LLFB283:
	save	%sp, -104, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL566
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL567
	 nop
.LL566:
	sethi	%hi(37888), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL567:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE283:
	.size	bitfield__write_var_BitField_offset5_count24, .-bitfield__write_var_BitField_offset5_count24
	.align 4
	.type	bitfield__read_var_BitField_offset5_count25, #function
	.proc	04
bitfield__read_var_BitField_offset5_count25:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	bitfield__read_var_BitField_offset5_count25, .-bitfield__read_var_BitField_offset5_count25
	.align 4
	.type	bitfield__write_var_BitField_offset5_count25, #function
	.proc	020
bitfield__write_var_BitField_offset5_count25:
.LLFB285:
	save	%sp, -104, %sp
.LLCFI285:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL570
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL571
	 nop
.LL570:
	sethi	%hi(37888), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL571:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE285:
	.size	bitfield__write_var_BitField_offset5_count25, .-bitfield__write_var_BitField_offset5_count25
	.align 4
	.type	bitfield__read_var_BitField_offset5_count26, #function
	.proc	04
bitfield__read_var_BitField_offset5_count26:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 5, %g1
	srl	%g1, 6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	bitfield__read_var_BitField_offset5_count26, .-bitfield__read_var_BitField_offset5_count26
	.align 4
	.type	bitfield__write_var_BitField_offset5_count26, #function
	.proc	020
bitfield__write_var_BitField_offset5_count26:
.LLFB287:
	save	%sp, -104, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL574
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL575
	 nop
.LL574:
	sethi	%hi(37888), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL575:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-134217728), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE287:
	.size	bitfield__write_var_BitField_offset5_count26, .-bitfield__write_var_BitField_offset5_count26
	.align 4
	.type	bitfield__read_var_BitField_offset5_count27, #function
	.proc	04
bitfield__read_var_BitField_offset5_count27:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE288:
	.size	bitfield__read_var_BitField_offset5_count27, .-bitfield__read_var_BitField_offset5_count27
	.align 4
	.type	bitfield__write_var_BitField_offset5_count27, #function
	.proc	020
bitfield__write_var_BitField_offset5_count27:
.LLFB289:
	save	%sp, -104, %sp
.LLCFI289:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL578
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL579
	 nop
.LL578:
	sethi	%hi(37888), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL579:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-134217728), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE289:
	.size	bitfield__write_var_BitField_offset5_count27, .-bitfield__write_var_BitField_offset5_count27
	.align 4
	.type	bitfield__read_var_BitField_offset6_count1, #function
	.proc	04
bitfield__read_var_BitField_offset6_count1:
.LLFB290:
	save	%sp, -96, %sp
.LLCFI290:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE290:
	.size	bitfield__read_var_BitField_offset6_count1, .-bitfield__read_var_BitField_offset6_count1
	.align 4
	.type	bitfield__write_var_BitField_offset6_count1, #function
	.proc	020
bitfield__write_var_BitField_offset6_count1:
.LLFB291:
	save	%sp, -104, %sp
.LLCFI291:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL582
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL583
	 nop
.LL582:
	sethi	%hi(38912), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL583:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ldub	[%g1], %g1
	and	%g1, -3, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE291:
	.size	bitfield__write_var_BitField_offset6_count1, .-bitfield__write_var_BitField_offset6_count1
	.align 4
	.type	bitfield__read_var_BitField_offset6_count2, #function
	.proc	04
bitfield__read_var_BitField_offset6_count2:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE292:
	.size	bitfield__read_var_BitField_offset6_count2, .-bitfield__read_var_BitField_offset6_count2
	.align 4
	.type	bitfield__write_var_BitField_offset6_count2, #function
	.proc	020
bitfield__write_var_BitField_offset6_count2:
.LLFB293:
	save	%sp, -104, %sp
.LLCFI293:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL586
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL587
	 nop
.LL586:
	sethi	%hi(38912), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL587:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -4, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE293:
	.size	bitfield__write_var_BitField_offset6_count2, .-bitfield__write_var_BitField_offset6_count2
	.align 4
	.type	bitfield__read_var_BitField_offset6_count3, #function
	.proc	04
bitfield__read_var_BitField_offset6_count3:
.LLFB294:
	save	%sp, -96, %sp
.LLCFI294:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE294:
	.size	bitfield__read_var_BitField_offset6_count3, .-bitfield__read_var_BitField_offset6_count3
	.align 4
	.type	bitfield__write_var_BitField_offset6_count3, #function
	.proc	020
bitfield__write_var_BitField_offset6_count3:
.LLFB295:
	save	%sp, -104, %sp
.LLCFI295:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL590
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL591
	 nop
.LL590:
	sethi	%hi(38912), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL591:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -897, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE295:
	.size	bitfield__write_var_BitField_offset6_count3, .-bitfield__write_var_BitField_offset6_count3
	.align 4
	.type	bitfield__read_var_BitField_offset6_count4, #function
	.proc	04
bitfield__read_var_BitField_offset6_count4:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE296:
	.size	bitfield__read_var_BitField_offset6_count4, .-bitfield__read_var_BitField_offset6_count4
	.align 4
	.type	bitfield__write_var_BitField_offset6_count4, #function
	.proc	020
bitfield__write_var_BitField_offset6_count4:
.LLFB297:
	save	%sp, -104, %sp
.LLCFI297:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL594
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL595
	 nop
.LL594:
	sethi	%hi(38912), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL595:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -961, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE297:
	.size	bitfield__write_var_BitField_offset6_count4, .-bitfield__write_var_BitField_offset6_count4
	.align 4
	.type	bitfield__read_var_BitField_offset6_count5, #function
	.proc	04
bitfield__read_var_BitField_offset6_count5:
.LLFB298:
	save	%sp, -96, %sp
.LLCFI298:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE298:
	.size	bitfield__read_var_BitField_offset6_count5, .-bitfield__read_var_BitField_offset6_count5
	.align 4
	.type	bitfield__write_var_BitField_offset6_count5, #function
	.proc	020
bitfield__write_var_BitField_offset6_count5:
.LLFB299:
	save	%sp, -104, %sp
.LLCFI299:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL598
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL599
	 nop
.LL598:
	sethi	%hi(39936), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL599:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -993, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE299:
	.size	bitfield__write_var_BitField_offset6_count5, .-bitfield__write_var_BitField_offset6_count5
	.align 4
	.type	bitfield__read_var_BitField_offset6_count6, #function
	.proc	04
bitfield__read_var_BitField_offset6_count6:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE300:
	.size	bitfield__read_var_BitField_offset6_count6, .-bitfield__read_var_BitField_offset6_count6
	.align 4
	.type	bitfield__write_var_BitField_offset6_count6, #function
	.proc	020
bitfield__write_var_BitField_offset6_count6:
.LLFB301:
	save	%sp, -104, %sp
.LLCFI301:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL602
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL603
	 nop
.LL602:
	sethi	%hi(39936), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL603:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -1009, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE301:
	.size	bitfield__write_var_BitField_offset6_count6, .-bitfield__write_var_BitField_offset6_count6
	.align 4
	.type	bitfield__read_var_BitField_offset6_count7, #function
	.proc	04
bitfield__read_var_BitField_offset6_count7:
.LLFB302:
	save	%sp, -96, %sp
.LLCFI302:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE302:
	.size	bitfield__read_var_BitField_offset6_count7, .-bitfield__read_var_BitField_offset6_count7
	.align 4
	.type	bitfield__write_var_BitField_offset6_count7, #function
	.proc	020
bitfield__write_var_BitField_offset6_count7:
.LLFB303:
	save	%sp, -104, %sp
.LLCFI303:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL606
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL607
	 nop
.LL606:
	sethi	%hi(39936), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL607:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -1017, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE303:
	.size	bitfield__write_var_BitField_offset6_count7, .-bitfield__write_var_BitField_offset6_count7
	.align 4
	.type	bitfield__read_var_BitField_offset6_count8, #function
	.proc	04
bitfield__read_var_BitField_offset6_count8:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE304:
	.size	bitfield__read_var_BitField_offset6_count8, .-bitfield__read_var_BitField_offset6_count8
	.align 4
	.type	bitfield__write_var_BitField_offset6_count8, #function
	.proc	020
bitfield__write_var_BitField_offset6_count8:
.LLFB305:
	save	%sp, -104, %sp
.LLCFI305:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL610
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL611
	 nop
.LL610:
	sethi	%hi(39936), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL611:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -1021, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE305:
	.size	bitfield__write_var_BitField_offset6_count8, .-bitfield__write_var_BitField_offset6_count8
	.align 4
	.type	bitfield__read_var_BitField_offset6_count9, #function
	.proc	04
bitfield__read_var_BitField_offset6_count9:
.LLFB306:
	save	%sp, -96, %sp
.LLCFI306:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE306:
	.size	bitfield__read_var_BitField_offset6_count9, .-bitfield__read_var_BitField_offset6_count9
	.align 4
	.type	bitfield__write_var_BitField_offset6_count9, #function
	.proc	020
bitfield__write_var_BitField_offset6_count9:
.LLFB307:
	save	%sp, -104, %sp
.LLCFI307:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL614
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL615
	 nop
.LL614:
	sethi	%hi(40960), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL615:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	and	%g1, -1023, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 511, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE307:
	.size	bitfield__write_var_BitField_offset6_count9, .-bitfield__write_var_BitField_offset6_count9
	.align 4
	.type	bitfield__read_var_BitField_offset6_count10, #function
	.proc	04
bitfield__read_var_BitField_offset6_count10:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE308:
	.size	bitfield__read_var_BitField_offset6_count10, .-bitfield__read_var_BitField_offset6_count10
	.align 4
	.type	bitfield__write_var_BitField_offset6_count10, #function
	.proc	020
bitfield__write_var_BitField_offset6_count10:
.LLFB309:
	save	%sp, -104, %sp
.LLCFI309:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL618
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL619
	 nop
.LL618:
	sethi	%hi(40960), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL619:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -1024, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE309:
	.size	bitfield__write_var_BitField_offset6_count10, .-bitfield__write_var_BitField_offset6_count10
	.align 4
	.type	bitfield__read_var_BitField_offset6_count11, #function
	.proc	04
bitfield__read_var_BitField_offset6_count11:
.LLFB310:
	save	%sp, -96, %sp
.LLCFI310:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE310:
	.size	bitfield__read_var_BitField_offset6_count11, .-bitfield__read_var_BitField_offset6_count11
	.align 4
	.type	bitfield__write_var_BitField_offset6_count11, #function
	.proc	020
bitfield__write_var_BitField_offset6_count11:
.LLFB311:
	save	%sp, -104, %sp
.LLCFI311:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL622
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL623
	 nop
.LL622:
	sethi	%hi(40960), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL623:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67077120), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE311:
	.size	bitfield__write_var_BitField_offset6_count11, .-bitfield__write_var_BitField_offset6_count11
	.align 4
	.type	bitfield__read_var_BitField_offset6_count12, #function
	.proc	04
bitfield__read_var_BitField_offset6_count12:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE312:
	.size	bitfield__read_var_BitField_offset6_count12, .-bitfield__read_var_BitField_offset6_count12
	.align 4
	.type	bitfield__write_var_BitField_offset6_count12, #function
	.proc	020
bitfield__write_var_BitField_offset6_count12:
.LLFB313:
	save	%sp, -104, %sp
.LLCFI313:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL626
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL627
	 nop
.LL626:
	sethi	%hi(40960), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL627:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67093504), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE313:
	.size	bitfield__write_var_BitField_offset6_count12, .-bitfield__write_var_BitField_offset6_count12
	.align 4
	.type	bitfield__read_var_BitField_offset6_count13, #function
	.proc	04
bitfield__read_var_BitField_offset6_count13:
.LLFB314:
	save	%sp, -96, %sp
.LLCFI314:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE314:
	.size	bitfield__read_var_BitField_offset6_count13, .-bitfield__read_var_BitField_offset6_count13
	.align 4
	.type	bitfield__write_var_BitField_offset6_count13, #function
	.proc	020
bitfield__write_var_BitField_offset6_count13:
.LLFB315:
	save	%sp, -104, %sp
.LLCFI315:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL630
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL631
	 nop
.LL630:
	sethi	%hi(41984), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL631:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67101696), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE315:
	.size	bitfield__write_var_BitField_offset6_count13, .-bitfield__write_var_BitField_offset6_count13
	.align 4
	.type	bitfield__read_var_BitField_offset6_count14, #function
	.proc	04
bitfield__read_var_BitField_offset6_count14:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE316:
	.size	bitfield__read_var_BitField_offset6_count14, .-bitfield__read_var_BitField_offset6_count14
	.align 4
	.type	bitfield__write_var_BitField_offset6_count14, #function
	.proc	020
bitfield__write_var_BitField_offset6_count14:
.LLFB317:
	save	%sp, -104, %sp
.LLCFI317:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL634
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL635
	 nop
.LL634:
	sethi	%hi(41984), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL635:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67105792), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE317:
	.size	bitfield__write_var_BitField_offset6_count14, .-bitfield__write_var_BitField_offset6_count14
	.align 4
	.type	bitfield__read_var_BitField_offset6_count15, #function
	.proc	04
bitfield__read_var_BitField_offset6_count15:
.LLFB318:
	save	%sp, -96, %sp
.LLCFI318:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE318:
	.size	bitfield__read_var_BitField_offset6_count15, .-bitfield__read_var_BitField_offset6_count15
	.align 4
	.type	bitfield__write_var_BitField_offset6_count15, #function
	.proc	020
bitfield__write_var_BitField_offset6_count15:
.LLFB319:
	save	%sp, -104, %sp
.LLCFI319:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL638
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL639
	 nop
.LL638:
	sethi	%hi(41984), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL639:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67107840), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE319:
	.size	bitfield__write_var_BitField_offset6_count15, .-bitfield__write_var_BitField_offset6_count15
	.align 4
	.type	bitfield__read_var_BitField_offset6_count16, #function
	.proc	04
bitfield__read_var_BitField_offset6_count16:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE320:
	.size	bitfield__read_var_BitField_offset6_count16, .-bitfield__read_var_BitField_offset6_count16
	.align 4
	.type	bitfield__write_var_BitField_offset6_count16, #function
	.proc	020
bitfield__write_var_BitField_offset6_count16:
.LLFB321:
	save	%sp, -104, %sp
.LLCFI321:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL642
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL643
	 nop
.LL642:
	sethi	%hi(41984), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL643:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE321:
	.size	bitfield__write_var_BitField_offset6_count16, .-bitfield__write_var_BitField_offset6_count16
	.align 4
	.type	bitfield__read_var_BitField_offset6_count17, #function
	.proc	04
bitfield__read_var_BitField_offset6_count17:
.LLFB322:
	save	%sp, -96, %sp
.LLCFI322:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE322:
	.size	bitfield__read_var_BitField_offset6_count17, .-bitfield__read_var_BitField_offset6_count17
	.align 4
	.type	bitfield__write_var_BitField_offset6_count17, #function
	.proc	020
bitfield__write_var_BitField_offset6_count17:
.LLFB323:
	save	%sp, -104, %sp
.LLCFI323:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL646
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL647
	 nop
.LL646:
	sethi	%hi(43008), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL647:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE323:
	.size	bitfield__write_var_BitField_offset6_count17, .-bitfield__write_var_BitField_offset6_count17
	.align 4
	.type	bitfield__read_var_BitField_offset6_count18, #function
	.proc	04
bitfield__read_var_BitField_offset6_count18:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE324:
	.size	bitfield__read_var_BitField_offset6_count18, .-bitfield__read_var_BitField_offset6_count18
	.align 4
	.type	bitfield__write_var_BitField_offset6_count18, #function
	.proc	020
bitfield__write_var_BitField_offset6_count18:
.LLFB325:
	save	%sp, -104, %sp
.LLCFI325:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL650
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL651
	 nop
.LL650:
	sethi	%hi(43008), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL651:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE325:
	.size	bitfield__write_var_BitField_offset6_count18, .-bitfield__write_var_BitField_offset6_count18
	.align 4
	.type	bitfield__read_var_BitField_offset6_count19, #function
	.proc	04
bitfield__read_var_BitField_offset6_count19:
.LLFB326:
	save	%sp, -96, %sp
.LLCFI326:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE326:
	.size	bitfield__read_var_BitField_offset6_count19, .-bitfield__read_var_BitField_offset6_count19
	.align 4
	.type	bitfield__write_var_BitField_offset6_count19, #function
	.proc	020
bitfield__write_var_BitField_offset6_count19:
.LLFB327:
	save	%sp, -104, %sp
.LLCFI327:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL654
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL655
	 nop
.LL654:
	sethi	%hi(43008), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL655:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE327:
	.size	bitfield__write_var_BitField_offset6_count19, .-bitfield__write_var_BitField_offset6_count19
	.align 4
	.type	bitfield__read_var_BitField_offset6_count20, #function
	.proc	04
bitfield__read_var_BitField_offset6_count20:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE328:
	.size	bitfield__read_var_BitField_offset6_count20, .-bitfield__read_var_BitField_offset6_count20
	.align 4
	.type	bitfield__write_var_BitField_offset6_count20, #function
	.proc	020
bitfield__write_var_BitField_offset6_count20:
.LLFB329:
	save	%sp, -104, %sp
.LLCFI329:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL658
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL659
	 nop
.LL658:
	sethi	%hi(43008), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL659:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE329:
	.size	bitfield__write_var_BitField_offset6_count20, .-bitfield__write_var_BitField_offset6_count20
	.align 4
	.type	bitfield__read_var_BitField_offset6_count21, #function
	.proc	04
bitfield__read_var_BitField_offset6_count21:
.LLFB330:
	save	%sp, -96, %sp
.LLCFI330:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE330:
	.size	bitfield__read_var_BitField_offset6_count21, .-bitfield__read_var_BitField_offset6_count21
	.align 4
	.type	bitfield__write_var_BitField_offset6_count21, #function
	.proc	020
bitfield__write_var_BitField_offset6_count21:
.LLFB331:
	save	%sp, -104, %sp
.LLCFI331:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL662
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL663
	 nop
.LL662:
	sethi	%hi(44032), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL663:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE331:
	.size	bitfield__write_var_BitField_offset6_count21, .-bitfield__write_var_BitField_offset6_count21
	.align 4
	.type	bitfield__read_var_BitField_offset6_count22, #function
	.proc	04
bitfield__read_var_BitField_offset6_count22:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE332:
	.size	bitfield__read_var_BitField_offset6_count22, .-bitfield__read_var_BitField_offset6_count22
	.align 4
	.type	bitfield__write_var_BitField_offset6_count22, #function
	.proc	020
bitfield__write_var_BitField_offset6_count22:
.LLFB333:
	save	%sp, -104, %sp
.LLCFI333:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL666
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL667
	 nop
.LL666:
	sethi	%hi(44032), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL667:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE333:
	.size	bitfield__write_var_BitField_offset6_count22, .-bitfield__write_var_BitField_offset6_count22
	.align 4
	.type	bitfield__read_var_BitField_offset6_count23, #function
	.proc	04
bitfield__read_var_BitField_offset6_count23:
.LLFB334:
	save	%sp, -96, %sp
.LLCFI334:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE334:
	.size	bitfield__read_var_BitField_offset6_count23, .-bitfield__read_var_BitField_offset6_count23
	.align 4
	.type	bitfield__write_var_BitField_offset6_count23, #function
	.proc	020
bitfield__write_var_BitField_offset6_count23:
.LLFB335:
	save	%sp, -104, %sp
.LLCFI335:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL670
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL671
	 nop
.LL670:
	sethi	%hi(44032), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL671:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE335:
	.size	bitfield__write_var_BitField_offset6_count23, .-bitfield__write_var_BitField_offset6_count23
	.align 4
	.type	bitfield__read_var_BitField_offset6_count24, #function
	.proc	04
bitfield__read_var_BitField_offset6_count24:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	bitfield__read_var_BitField_offset6_count24, .-bitfield__read_var_BitField_offset6_count24
	.align 4
	.type	bitfield__write_var_BitField_offset6_count24, #function
	.proc	020
bitfield__write_var_BitField_offset6_count24:
.LLFB337:
	save	%sp, -104, %sp
.LLCFI337:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL674
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL675
	 nop
.LL674:
	sethi	%hi(44032), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL675:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE337:
	.size	bitfield__write_var_BitField_offset6_count24, .-bitfield__write_var_BitField_offset6_count24
	.align 4
	.type	bitfield__read_var_BitField_offset6_count25, #function
	.proc	04
bitfield__read_var_BitField_offset6_count25:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 6, %g1
	srl	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	bitfield__read_var_BitField_offset6_count25, .-bitfield__read_var_BitField_offset6_count25
	.align 4
	.type	bitfield__write_var_BitField_offset6_count25, #function
	.proc	020
bitfield__write_var_BitField_offset6_count25:
.LLFB339:
	save	%sp, -104, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL678
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL679
	 nop
.LL678:
	sethi	%hi(45056), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL679:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-67108864), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE339:
	.size	bitfield__write_var_BitField_offset6_count25, .-bitfield__write_var_BitField_offset6_count25
	.align 4
	.type	bitfield__read_var_BitField_offset6_count26, #function
	.proc	04
bitfield__read_var_BitField_offset6_count26:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE340:
	.size	bitfield__read_var_BitField_offset6_count26, .-bitfield__read_var_BitField_offset6_count26
	.align 4
	.type	bitfield__write_var_BitField_offset6_count26, #function
	.proc	020
bitfield__write_var_BitField_offset6_count26:
.LLFB341:
	save	%sp, -104, %sp
.LLCFI341:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL682
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL683
	 nop
.LL682:
	sethi	%hi(45056), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL683:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-67108864), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE341:
	.size	bitfield__write_var_BitField_offset6_count26, .-bitfield__write_var_BitField_offset6_count26
	.align 4
	.type	bitfield__read_var_BitField_offset7_count1, #function
	.proc	04
bitfield__read_var_BitField_offset7_count1:
.LLFB342:
	save	%sp, -96, %sp
.LLCFI342:
	mov	%i0, %g1
	ldub	[%g1], %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE342:
	.size	bitfield__read_var_BitField_offset7_count1, .-bitfield__read_var_BitField_offset7_count1
	.align 4
	.type	bitfield__write_var_BitField_offset7_count1, #function
	.proc	020
bitfield__write_var_BitField_offset7_count1:
.LLFB343:
	save	%sp, -104, %sp
.LLCFI343:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL686
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL687
	 nop
.LL686:
	sethi	%hi(45056), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL687:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ldub	[%o5], %o5
	and	%o5, -2, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE343:
	.size	bitfield__write_var_BitField_offset7_count1, .-bitfield__write_var_BitField_offset7_count1
	.align 4
	.type	bitfield__read_var_BitField_offset7_count2, #function
	.proc	04
bitfield__read_var_BitField_offset7_count2:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE344:
	.size	bitfield__read_var_BitField_offset7_count2, .-bitfield__read_var_BitField_offset7_count2
	.align 4
	.type	bitfield__write_var_BitField_offset7_count2, #function
	.proc	020
bitfield__write_var_BitField_offset7_count2:
.LLFB345:
	save	%sp, -104, %sp
.LLCFI345:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL690
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL691
	 nop
.LL690:
	sethi	%hi(45056), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL691:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -385, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE345:
	.size	bitfield__write_var_BitField_offset7_count2, .-bitfield__write_var_BitField_offset7_count2
	.align 4
	.type	bitfield__read_var_BitField_offset7_count3, #function
	.proc	04
bitfield__read_var_BitField_offset7_count3:
.LLFB346:
	save	%sp, -96, %sp
.LLCFI346:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE346:
	.size	bitfield__read_var_BitField_offset7_count3, .-bitfield__read_var_BitField_offset7_count3
	.align 4
	.type	bitfield__write_var_BitField_offset7_count3, #function
	.proc	020
bitfield__write_var_BitField_offset7_count3:
.LLFB347:
	save	%sp, -104, %sp
.LLCFI347:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL694
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL695
	 nop
.LL694:
	sethi	%hi(46080), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL695:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -449, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE347:
	.size	bitfield__write_var_BitField_offset7_count3, .-bitfield__write_var_BitField_offset7_count3
	.align 4
	.type	bitfield__read_var_BitField_offset7_count4, #function
	.proc	04
bitfield__read_var_BitField_offset7_count4:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	bitfield__read_var_BitField_offset7_count4, .-bitfield__read_var_BitField_offset7_count4
	.align 4
	.type	bitfield__write_var_BitField_offset7_count4, #function
	.proc	020
bitfield__write_var_BitField_offset7_count4:
.LLFB349:
	save	%sp, -104, %sp
.LLCFI349:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL698
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL699
	 nop
.LL698:
	sethi	%hi(46080), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL699:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -481, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE349:
	.size	bitfield__write_var_BitField_offset7_count4, .-bitfield__write_var_BitField_offset7_count4
	.align 4
	.type	bitfield__read_var_BitField_offset7_count5, #function
	.proc	04
bitfield__read_var_BitField_offset7_count5:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	bitfield__read_var_BitField_offset7_count5, .-bitfield__read_var_BitField_offset7_count5
	.align 4
	.type	bitfield__write_var_BitField_offset7_count5, #function
	.proc	020
bitfield__write_var_BitField_offset7_count5:
.LLFB351:
	save	%sp, -104, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL702
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL703
	 nop
.LL702:
	sethi	%hi(46080), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL703:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -497, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE351:
	.size	bitfield__write_var_BitField_offset7_count5, .-bitfield__write_var_BitField_offset7_count5
	.align 4
	.type	bitfield__read_var_BitField_offset7_count6, #function
	.proc	04
bitfield__read_var_BitField_offset7_count6:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	bitfield__read_var_BitField_offset7_count6, .-bitfield__read_var_BitField_offset7_count6
	.align 4
	.type	bitfield__write_var_BitField_offset7_count6, #function
	.proc	020
bitfield__write_var_BitField_offset7_count6:
.LLFB353:
	save	%sp, -104, %sp
.LLCFI353:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL706
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL707
	 nop
.LL706:
	sethi	%hi(46080), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL707:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -505, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE353:
	.size	bitfield__write_var_BitField_offset7_count6, .-bitfield__write_var_BitField_offset7_count6
	.align 4
	.type	bitfield__read_var_BitField_offset7_count7, #function
	.proc	04
bitfield__read_var_BitField_offset7_count7:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	bitfield__read_var_BitField_offset7_count7, .-bitfield__read_var_BitField_offset7_count7
	.align 4
	.type	bitfield__write_var_BitField_offset7_count7, #function
	.proc	020
bitfield__write_var_BitField_offset7_count7:
.LLFB355:
	save	%sp, -104, %sp
.LLCFI355:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL710
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL711
	 nop
.LL710:
	sethi	%hi(47104), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL711:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -509, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE355:
	.size	bitfield__write_var_BitField_offset7_count7, .-bitfield__write_var_BitField_offset7_count7
	.align 4
	.type	bitfield__read_var_BitField_offset7_count8, #function
	.proc	04
bitfield__read_var_BitField_offset7_count8:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	bitfield__read_var_BitField_offset7_count8, .-bitfield__read_var_BitField_offset7_count8
	.align 4
	.type	bitfield__write_var_BitField_offset7_count8, #function
	.proc	020
bitfield__write_var_BitField_offset7_count8:
.LLFB357:
	save	%sp, -104, %sp
.LLCFI357:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL714
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL715
	 nop
.LL714:
	sethi	%hi(47104), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL715:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	lduh	[%g1], %g1
	and	%g1, -511, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 255, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE357:
	.size	bitfield__write_var_BitField_offset7_count8, .-bitfield__write_var_BitField_offset7_count8
	.align 4
	.type	bitfield__read_var_BitField_offset7_count9, #function
	.proc	04
bitfield__read_var_BitField_offset7_count9:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	mov	%i0, %g1
	lduh	[%g1], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	bitfield__read_var_BitField_offset7_count9, .-bitfield__read_var_BitField_offset7_count9
	.align 4
	.type	bitfield__write_var_BitField_offset7_count9, #function
	.proc	020
bitfield__write_var_BitField_offset7_count9:
.LLFB359:
	save	%sp, -104, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL718
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL719
	 nop
.LL718:
	sethi	%hi(47104), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL719:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	lduh	[%o5], %o5
	and	%o5, -512, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE359:
	.size	bitfield__write_var_BitField_offset7_count9, .-bitfield__write_var_BitField_offset7_count9
	.align 4
	.type	bitfield__read_var_BitField_offset7_count10, #function
	.proc	04
bitfield__read_var_BitField_offset7_count10:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE360:
	.size	bitfield__read_var_BitField_offset7_count10, .-bitfield__read_var_BitField_offset7_count10
	.align 4
	.type	bitfield__write_var_BitField_offset7_count10, #function
	.proc	020
bitfield__write_var_BitField_offset7_count10:
.LLFB361:
	save	%sp, -104, %sp
.LLCFI361:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL722
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL723
	 nop
.LL722:
	sethi	%hi(47104), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL723:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33522688), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE361:
	.size	bitfield__write_var_BitField_offset7_count10, .-bitfield__write_var_BitField_offset7_count10
	.align 4
	.type	bitfield__read_var_BitField_offset7_count11, #function
	.proc	04
bitfield__read_var_BitField_offset7_count11:
.LLFB362:
	save	%sp, -96, %sp
.LLCFI362:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE362:
	.size	bitfield__read_var_BitField_offset7_count11, .-bitfield__read_var_BitField_offset7_count11
	.align 4
	.type	bitfield__write_var_BitField_offset7_count11, #function
	.proc	020
bitfield__write_var_BitField_offset7_count11:
.LLFB363:
	save	%sp, -104, %sp
.LLCFI363:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL726
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL727
	 nop
.LL726:
	sethi	%hi(48128), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL727:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33539072), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE363:
	.size	bitfield__write_var_BitField_offset7_count11, .-bitfield__write_var_BitField_offset7_count11
	.align 4
	.type	bitfield__read_var_BitField_offset7_count12, #function
	.proc	04
bitfield__read_var_BitField_offset7_count12:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	bitfield__read_var_BitField_offset7_count12, .-bitfield__read_var_BitField_offset7_count12
	.align 4
	.type	bitfield__write_var_BitField_offset7_count12, #function
	.proc	020
bitfield__write_var_BitField_offset7_count12:
.LLFB365:
	save	%sp, -104, %sp
.LLCFI365:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL730
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL731
	 nop
.LL730:
	sethi	%hi(48128), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL731:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33547264), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE365:
	.size	bitfield__write_var_BitField_offset7_count12, .-bitfield__write_var_BitField_offset7_count12
	.align 4
	.type	bitfield__read_var_BitField_offset7_count13, #function
	.proc	04
bitfield__read_var_BitField_offset7_count13:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	bitfield__read_var_BitField_offset7_count13, .-bitfield__read_var_BitField_offset7_count13
	.align 4
	.type	bitfield__write_var_BitField_offset7_count13, #function
	.proc	020
bitfield__write_var_BitField_offset7_count13:
.LLFB367:
	save	%sp, -104, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL734
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL735
	 nop
.LL734:
	sethi	%hi(48128), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL735:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33551360), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE367:
	.size	bitfield__write_var_BitField_offset7_count13, .-bitfield__write_var_BitField_offset7_count13
	.align 4
	.type	bitfield__read_var_BitField_offset7_count14, #function
	.proc	04
bitfield__read_var_BitField_offset7_count14:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	bitfield__read_var_BitField_offset7_count14, .-bitfield__read_var_BitField_offset7_count14
	.align 4
	.type	bitfield__write_var_BitField_offset7_count14, #function
	.proc	020
bitfield__write_var_BitField_offset7_count14:
.LLFB369:
	save	%sp, -104, %sp
.LLCFI369:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL738
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL739
	 nop
.LL738:
	sethi	%hi(48128), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL739:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33553408), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE369:
	.size	bitfield__write_var_BitField_offset7_count14, .-bitfield__write_var_BitField_offset7_count14
	.align 4
	.type	bitfield__read_var_BitField_offset7_count15, #function
	.proc	04
bitfield__read_var_BitField_offset7_count15:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	bitfield__read_var_BitField_offset7_count15, .-bitfield__read_var_BitField_offset7_count15
	.align 4
	.type	bitfield__write_var_BitField_offset7_count15, #function
	.proc	020
bitfield__write_var_BitField_offset7_count15:
.LLFB371:
	save	%sp, -104, %sp
.LLCFI371:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL742
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL743
	 nop
.LL742:
	sethi	%hi(49152), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL743:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE371:
	.size	bitfield__write_var_BitField_offset7_count15, .-bitfield__write_var_BitField_offset7_count15
	.align 4
	.type	bitfield__read_var_BitField_offset7_count16, #function
	.proc	04
bitfield__read_var_BitField_offset7_count16:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE372:
	.size	bitfield__read_var_BitField_offset7_count16, .-bitfield__read_var_BitField_offset7_count16
	.align 4
	.type	bitfield__write_var_BitField_offset7_count16, #function
	.proc	020
bitfield__write_var_BitField_offset7_count16:
.LLFB373:
	save	%sp, -104, %sp
.LLCFI373:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL746
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL747
	 nop
.LL746:
	sethi	%hi(49152), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL747:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE373:
	.size	bitfield__write_var_BitField_offset7_count16, .-bitfield__write_var_BitField_offset7_count16
	.align 4
	.type	bitfield__read_var_BitField_offset7_count17, #function
	.proc	04
bitfield__read_var_BitField_offset7_count17:
.LLFB374:
	save	%sp, -96, %sp
.LLCFI374:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE374:
	.size	bitfield__read_var_BitField_offset7_count17, .-bitfield__read_var_BitField_offset7_count17
	.align 4
	.type	bitfield__write_var_BitField_offset7_count17, #function
	.proc	020
bitfield__write_var_BitField_offset7_count17:
.LLFB375:
	save	%sp, -104, %sp
.LLCFI375:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL750
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL751
	 nop
.LL750:
	sethi	%hi(49152), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL751:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE375:
	.size	bitfield__write_var_BitField_offset7_count17, .-bitfield__write_var_BitField_offset7_count17
	.align 4
	.type	bitfield__read_var_BitField_offset7_count18, #function
	.proc	04
bitfield__read_var_BitField_offset7_count18:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	bitfield__read_var_BitField_offset7_count18, .-bitfield__read_var_BitField_offset7_count18
	.align 4
	.type	bitfield__write_var_BitField_offset7_count18, #function
	.proc	020
bitfield__write_var_BitField_offset7_count18:
.LLFB377:
	save	%sp, -104, %sp
.LLCFI377:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL754
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL755
	 nop
.LL754:
	sethi	%hi(49152), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL755:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE377:
	.size	bitfield__write_var_BitField_offset7_count18, .-bitfield__write_var_BitField_offset7_count18
	.align 4
	.type	bitfield__read_var_BitField_offset7_count19, #function
	.proc	04
bitfield__read_var_BitField_offset7_count19:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	bitfield__read_var_BitField_offset7_count19, .-bitfield__read_var_BitField_offset7_count19
	.align 4
	.type	bitfield__write_var_BitField_offset7_count19, #function
	.proc	020
bitfield__write_var_BitField_offset7_count19:
.LLFB379:
	save	%sp, -104, %sp
.LLCFI379:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL758
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL759
	 nop
.LL758:
	sethi	%hi(50176), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL759:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE379:
	.size	bitfield__write_var_BitField_offset7_count19, .-bitfield__write_var_BitField_offset7_count19
	.align 4
	.type	bitfield__read_var_BitField_offset7_count20, #function
	.proc	04
bitfield__read_var_BitField_offset7_count20:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	bitfield__read_var_BitField_offset7_count20, .-bitfield__read_var_BitField_offset7_count20
	.align 4
	.type	bitfield__write_var_BitField_offset7_count20, #function
	.proc	020
bitfield__write_var_BitField_offset7_count20:
.LLFB381:
	save	%sp, -104, %sp
.LLCFI381:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL762
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL763
	 nop
.LL762:
	sethi	%hi(50176), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL763:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE381:
	.size	bitfield__write_var_BitField_offset7_count20, .-bitfield__write_var_BitField_offset7_count20
	.align 4
	.type	bitfield__read_var_BitField_offset7_count21, #function
	.proc	04
bitfield__read_var_BitField_offset7_count21:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	bitfield__read_var_BitField_offset7_count21, .-bitfield__read_var_BitField_offset7_count21
	.align 4
	.type	bitfield__write_var_BitField_offset7_count21, #function
	.proc	020
bitfield__write_var_BitField_offset7_count21:
.LLFB383:
	save	%sp, -104, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL766
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL767
	 nop
.LL766:
	sethi	%hi(50176), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL767:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE383:
	.size	bitfield__write_var_BitField_offset7_count21, .-bitfield__write_var_BitField_offset7_count21
	.align 4
	.type	bitfield__read_var_BitField_offset7_count22, #function
	.proc	04
bitfield__read_var_BitField_offset7_count22:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE384:
	.size	bitfield__read_var_BitField_offset7_count22, .-bitfield__read_var_BitField_offset7_count22
	.align 4
	.type	bitfield__write_var_BitField_offset7_count22, #function
	.proc	020
bitfield__write_var_BitField_offset7_count22:
.LLFB385:
	save	%sp, -104, %sp
.LLCFI385:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL770
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL771
	 nop
.LL770:
	sethi	%hi(50176), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL771:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE385:
	.size	bitfield__write_var_BitField_offset7_count22, .-bitfield__write_var_BitField_offset7_count22
	.align 4
	.type	bitfield__read_var_BitField_offset7_count23, #function
	.proc	04
bitfield__read_var_BitField_offset7_count23:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	bitfield__read_var_BitField_offset7_count23, .-bitfield__read_var_BitField_offset7_count23
	.align 4
	.type	bitfield__write_var_BitField_offset7_count23, #function
	.proc	020
bitfield__write_var_BitField_offset7_count23:
.LLFB387:
	save	%sp, -104, %sp
.LLCFI387:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL774
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL775
	 nop
.LL774:
	sethi	%hi(51200), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL775:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE387:
	.size	bitfield__write_var_BitField_offset7_count23, .-bitfield__write_var_BitField_offset7_count23
	.align 4
	.type	bitfield__read_var_BitField_offset7_count24, #function
	.proc	04
bitfield__read_var_BitField_offset7_count24:
.LLFB388:
	save	%sp, -96, %sp
.LLCFI388:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 7, %g1
	srl	%g1, 8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	bitfield__read_var_BitField_offset7_count24, .-bitfield__read_var_BitField_offset7_count24
	.align 4
	.type	bitfield__write_var_BitField_offset7_count24, #function
	.proc	020
bitfield__write_var_BitField_offset7_count24:
.LLFB389:
	save	%sp, -104, %sp
.LLCFI389:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL778
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL779
	 nop
.LL778:
	sethi	%hi(51200), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL779:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-33554432), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE389:
	.size	bitfield__write_var_BitField_offset7_count24, .-bitfield__write_var_BitField_offset7_count24
	.align 4
	.type	bitfield__read_var_BitField_offset7_count25, #function
	.proc	04
bitfield__read_var_BitField_offset7_count25:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE390:
	.size	bitfield__read_var_BitField_offset7_count25, .-bitfield__read_var_BitField_offset7_count25
	.align 4
	.type	bitfield__write_var_BitField_offset7_count25, #function
	.proc	020
bitfield__write_var_BitField_offset7_count25:
.LLFB391:
	save	%sp, -104, %sp
.LLCFI391:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL782
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL783
	 nop
.LL782:
	sethi	%hi(51200), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL783:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-33554432), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE391:
	.size	bitfield__write_var_BitField_offset7_count25, .-bitfield__write_var_BitField_offset7_count25
	.align 4
	.type	bitfield__read_var_BitField_offset8_count1, #function
	.proc	04
bitfield__read_var_BitField_offset8_count1:
.LLFB392:
	save	%sp, -96, %sp
.LLCFI392:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE392:
	.size	bitfield__read_var_BitField_offset8_count1, .-bitfield__read_var_BitField_offset8_count1
	.align 4
	.type	bitfield__write_var_BitField_offset8_count1, #function
	.proc	020
bitfield__write_var_BitField_offset8_count1:
.LLFB393:
	save	%sp, -104, %sp
.LLCFI393:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL786
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL787
	 nop
.LL786:
	sethi	%hi(51200), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL787:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, 127, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE393:
	.size	bitfield__write_var_BitField_offset8_count1, .-bitfield__write_var_BitField_offset8_count1
	.align 4
	.type	bitfield__read_var_BitField_offset8_count2, #function
	.proc	04
bitfield__read_var_BitField_offset8_count2:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE394:
	.size	bitfield__read_var_BitField_offset8_count2, .-bitfield__read_var_BitField_offset8_count2
	.align 4
	.type	bitfield__write_var_BitField_offset8_count2, #function
	.proc	020
bitfield__write_var_BitField_offset8_count2:
.LLFB395:
	save	%sp, -104, %sp
.LLCFI395:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL790
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL791
	 nop
.LL790:
	sethi	%hi(52224), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL791:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, 63, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE395:
	.size	bitfield__write_var_BitField_offset8_count2, .-bitfield__write_var_BitField_offset8_count2
	.align 4
	.type	bitfield__read_var_BitField_offset8_count3, #function
	.proc	04
bitfield__read_var_BitField_offset8_count3:
.LLFB396:
	save	%sp, -96, %sp
.LLCFI396:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE396:
	.size	bitfield__read_var_BitField_offset8_count3, .-bitfield__read_var_BitField_offset8_count3
	.align 4
	.type	bitfield__write_var_BitField_offset8_count3, #function
	.proc	020
bitfield__write_var_BitField_offset8_count3:
.LLFB397:
	save	%sp, -104, %sp
.LLCFI397:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL794
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL795
	 nop
.LL794:
	sethi	%hi(52224), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL795:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, 31, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE397:
	.size	bitfield__write_var_BitField_offset8_count3, .-bitfield__write_var_BitField_offset8_count3
	.align 4
	.type	bitfield__read_var_BitField_offset8_count4, #function
	.proc	04
bitfield__read_var_BitField_offset8_count4:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	bitfield__read_var_BitField_offset8_count4, .-bitfield__read_var_BitField_offset8_count4
	.align 4
	.type	bitfield__write_var_BitField_offset8_count4, #function
	.proc	020
bitfield__write_var_BitField_offset8_count4:
.LLFB399:
	save	%sp, -104, %sp
.LLCFI399:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL798
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL799
	 nop
.LL798:
	sethi	%hi(52224), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL799:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, 15, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE399:
	.size	bitfield__write_var_BitField_offset8_count4, .-bitfield__write_var_BitField_offset8_count4
	.align 4
	.type	bitfield__read_var_BitField_offset8_count5, #function
	.proc	04
bitfield__read_var_BitField_offset8_count5:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	bitfield__read_var_BitField_offset8_count5, .-bitfield__read_var_BitField_offset8_count5
	.align 4
	.type	bitfield__write_var_BitField_offset8_count5, #function
	.proc	020
bitfield__write_var_BitField_offset8_count5:
.LLFB401:
	save	%sp, -104, %sp
.LLCFI401:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL802
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL803
	 nop
.LL802:
	sethi	%hi(52224), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL803:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, 7, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE401:
	.size	bitfield__write_var_BitField_offset8_count5, .-bitfield__write_var_BitField_offset8_count5
	.align 4
	.type	bitfield__read_var_BitField_offset8_count6, #function
	.proc	04
bitfield__read_var_BitField_offset8_count6:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	bitfield__read_var_BitField_offset8_count6, .-bitfield__read_var_BitField_offset8_count6
	.align 4
	.type	bitfield__write_var_BitField_offset8_count6, #function
	.proc	020
bitfield__write_var_BitField_offset8_count6:
.LLFB403:
	save	%sp, -104, %sp
.LLCFI403:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL806
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL807
	 nop
.LL806:
	sethi	%hi(53248), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL807:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, 3, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE403:
	.size	bitfield__write_var_BitField_offset8_count6, .-bitfield__write_var_BitField_offset8_count6
	.align 4
	.type	bitfield__read_var_BitField_offset8_count7, #function
	.proc	04
bitfield__read_var_BitField_offset8_count7:
.LLFB404:
	save	%sp, -96, %sp
.LLCFI404:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	bitfield__read_var_BitField_offset8_count7, .-bitfield__read_var_BitField_offset8_count7
	.align 4
	.type	bitfield__write_var_BitField_offset8_count7, #function
	.proc	020
bitfield__write_var_BitField_offset8_count7:
.LLFB405:
	save	%sp, -104, %sp
.LLCFI405:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL810
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL811
	 nop
.LL810:
	sethi	%hi(53248), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL811:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, 1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE405:
	.size	bitfield__write_var_BitField_offset8_count7, .-bitfield__write_var_BitField_offset8_count7
	.align 4
	.type	bitfield__read_var_BitField_offset8_count8, #function
	.proc	04
bitfield__read_var_BitField_offset8_count8:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	bitfield__read_var_BitField_offset8_count8, .-bitfield__read_var_BitField_offset8_count8
	.align 4
	.type	bitfield__write_var_BitField_offset8_count8, #function
	.proc	020
bitfield__write_var_BitField_offset8_count8:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL814
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL815
	 nop
.LL814:
	sethi	%hi(53248), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL815:
	ld	[%fp+68], %g1
	add	%g1, 1, %g1
	ld	[%fp+72], %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE407:
	.size	bitfield__write_var_BitField_offset8_count8, .-bitfield__write_var_BitField_offset8_count8
	.align 4
	.type	bitfield__read_var_BitField_offset8_count9, #function
	.proc	04
bitfield__read_var_BitField_offset8_count9:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	bitfield__read_var_BitField_offset8_count9, .-bitfield__read_var_BitField_offset8_count9
	.align 4
	.type	bitfield__write_var_BitField_offset8_count9, #function
	.proc	020
bitfield__write_var_BitField_offset8_count9:
.LLFB409:
	save	%sp, -104, %sp
.LLCFI409:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL818
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL819
	 nop
.LL818:
	sethi	%hi(53248), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL819:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16745472), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE409:
	.size	bitfield__write_var_BitField_offset8_count9, .-bitfield__write_var_BitField_offset8_count9
	.align 4
	.type	bitfield__read_var_BitField_offset8_count10, #function
	.proc	04
bitfield__read_var_BitField_offset8_count10:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE410:
	.size	bitfield__read_var_BitField_offset8_count10, .-bitfield__read_var_BitField_offset8_count10
	.align 4
	.type	bitfield__write_var_BitField_offset8_count10, #function
	.proc	020
bitfield__write_var_BitField_offset8_count10:
.LLFB411:
	save	%sp, -104, %sp
.LLCFI411:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL822
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL823
	 nop
.LL822:
	sethi	%hi(54272), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL823:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16761856), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE411:
	.size	bitfield__write_var_BitField_offset8_count10, .-bitfield__write_var_BitField_offset8_count10
	.align 4
	.type	bitfield__read_var_BitField_offset8_count11, #function
	.proc	04
bitfield__read_var_BitField_offset8_count11:
.LLFB412:
	save	%sp, -96, %sp
.LLCFI412:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE412:
	.size	bitfield__read_var_BitField_offset8_count11, .-bitfield__read_var_BitField_offset8_count11
	.align 4
	.type	bitfield__write_var_BitField_offset8_count11, #function
	.proc	020
bitfield__write_var_BitField_offset8_count11:
.LLFB413:
	save	%sp, -104, %sp
.LLCFI413:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL826
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL827
	 nop
.LL826:
	sethi	%hi(54272), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL827:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16770048), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE413:
	.size	bitfield__write_var_BitField_offset8_count11, .-bitfield__write_var_BitField_offset8_count11
	.align 4
	.type	bitfield__read_var_BitField_offset8_count12, #function
	.proc	04
bitfield__read_var_BitField_offset8_count12:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	bitfield__read_var_BitField_offset8_count12, .-bitfield__read_var_BitField_offset8_count12
	.align 4
	.type	bitfield__write_var_BitField_offset8_count12, #function
	.proc	020
bitfield__write_var_BitField_offset8_count12:
.LLFB415:
	save	%sp, -104, %sp
.LLCFI415:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL830
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL831
	 nop
.LL830:
	sethi	%hi(54272), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL831:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16774144), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE415:
	.size	bitfield__write_var_BitField_offset8_count12, .-bitfield__write_var_BitField_offset8_count12
	.align 4
	.type	bitfield__read_var_BitField_offset8_count13, #function
	.proc	04
bitfield__read_var_BitField_offset8_count13:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	bitfield__read_var_BitField_offset8_count13, .-bitfield__read_var_BitField_offset8_count13
	.align 4
	.type	bitfield__write_var_BitField_offset8_count13, #function
	.proc	020
bitfield__write_var_BitField_offset8_count13:
.LLFB417:
	save	%sp, -104, %sp
.LLCFI417:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL834
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL835
	 nop
.LL834:
	sethi	%hi(54272), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL835:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16776192), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE417:
	.size	bitfield__write_var_BitField_offset8_count13, .-bitfield__write_var_BitField_offset8_count13
	.align 4
	.type	bitfield__read_var_BitField_offset8_count14, #function
	.proc	04
bitfield__read_var_BitField_offset8_count14:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	bitfield__read_var_BitField_offset8_count14, .-bitfield__read_var_BitField_offset8_count14
	.align 4
	.type	bitfield__write_var_BitField_offset8_count14, #function
	.proc	020
bitfield__write_var_BitField_offset8_count14:
.LLFB419:
	save	%sp, -104, %sp
.LLCFI419:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL838
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL839
	 nop
.LL838:
	sethi	%hi(55296), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL839:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE419:
	.size	bitfield__write_var_BitField_offset8_count14, .-bitfield__write_var_BitField_offset8_count14
	.align 4
	.type	bitfield__read_var_BitField_offset8_count15, #function
	.proc	04
bitfield__read_var_BitField_offset8_count15:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	bitfield__read_var_BitField_offset8_count15, .-bitfield__read_var_BitField_offset8_count15
	.align 4
	.type	bitfield__write_var_BitField_offset8_count15, #function
	.proc	020
bitfield__write_var_BitField_offset8_count15:
.LLFB421:
	save	%sp, -104, %sp
.LLCFI421:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL842
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL843
	 nop
.LL842:
	sethi	%hi(55296), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL843:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE421:
	.size	bitfield__write_var_BitField_offset8_count15, .-bitfield__write_var_BitField_offset8_count15
	.align 4
	.type	bitfield__read_var_BitField_offset8_count16, #function
	.proc	04
bitfield__read_var_BitField_offset8_count16:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE422:
	.size	bitfield__read_var_BitField_offset8_count16, .-bitfield__read_var_BitField_offset8_count16
	.align 4
	.type	bitfield__write_var_BitField_offset8_count16, #function
	.proc	020
bitfield__write_var_BitField_offset8_count16:
.LLFB423:
	save	%sp, -104, %sp
.LLCFI423:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL846
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL847
	 nop
.LL846:
	sethi	%hi(55296), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL847:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE423:
	.size	bitfield__write_var_BitField_offset8_count16, .-bitfield__write_var_BitField_offset8_count16
	.align 4
	.type	bitfield__read_var_BitField_offset8_count17, #function
	.proc	04
bitfield__read_var_BitField_offset8_count17:
.LLFB424:
	save	%sp, -96, %sp
.LLCFI424:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE424:
	.size	bitfield__read_var_BitField_offset8_count17, .-bitfield__read_var_BitField_offset8_count17
	.align 4
	.type	bitfield__write_var_BitField_offset8_count17, #function
	.proc	020
bitfield__write_var_BitField_offset8_count17:
.LLFB425:
	save	%sp, -104, %sp
.LLCFI425:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL850
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL851
	 nop
.LL850:
	sethi	%hi(55296), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL851:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE425:
	.size	bitfield__write_var_BitField_offset8_count17, .-bitfield__write_var_BitField_offset8_count17
	.align 4
	.type	bitfield__read_var_BitField_offset8_count18, #function
	.proc	04
bitfield__read_var_BitField_offset8_count18:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	bitfield__read_var_BitField_offset8_count18, .-bitfield__read_var_BitField_offset8_count18
	.align 4
	.type	bitfield__write_var_BitField_offset8_count18, #function
	.proc	020
bitfield__write_var_BitField_offset8_count18:
.LLFB427:
	save	%sp, -104, %sp
.LLCFI427:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL854
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL855
	 nop
.LL854:
	sethi	%hi(56320), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL855:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE427:
	.size	bitfield__write_var_BitField_offset8_count18, .-bitfield__write_var_BitField_offset8_count18
	.align 4
	.type	bitfield__read_var_BitField_offset8_count19, #function
	.proc	04
bitfield__read_var_BitField_offset8_count19:
.LLFB428:
	save	%sp, -96, %sp
.LLCFI428:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	bitfield__read_var_BitField_offset8_count19, .-bitfield__read_var_BitField_offset8_count19
	.align 4
	.type	bitfield__write_var_BitField_offset8_count19, #function
	.proc	020
bitfield__write_var_BitField_offset8_count19:
.LLFB429:
	save	%sp, -104, %sp
.LLCFI429:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL858
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL859
	 nop
.LL858:
	sethi	%hi(56320), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL859:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE429:
	.size	bitfield__write_var_BitField_offset8_count19, .-bitfield__write_var_BitField_offset8_count19
	.align 4
	.type	bitfield__read_var_BitField_offset8_count20, #function
	.proc	04
bitfield__read_var_BitField_offset8_count20:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	bitfield__read_var_BitField_offset8_count20, .-bitfield__read_var_BitField_offset8_count20
	.align 4
	.type	bitfield__write_var_BitField_offset8_count20, #function
	.proc	020
bitfield__write_var_BitField_offset8_count20:
.LLFB431:
	save	%sp, -104, %sp
.LLCFI431:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL862
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL863
	 nop
.LL862:
	sethi	%hi(56320), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL863:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE431:
	.size	bitfield__write_var_BitField_offset8_count20, .-bitfield__write_var_BitField_offset8_count20
	.align 4
	.type	bitfield__read_var_BitField_offset8_count21, #function
	.proc	04
bitfield__read_var_BitField_offset8_count21:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	bitfield__read_var_BitField_offset8_count21, .-bitfield__read_var_BitField_offset8_count21
	.align 4
	.type	bitfield__write_var_BitField_offset8_count21, #function
	.proc	020
bitfield__write_var_BitField_offset8_count21:
.LLFB433:
	save	%sp, -104, %sp
.LLCFI433:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL866
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL867
	 nop
.LL866:
	sethi	%hi(56320), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL867:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE433:
	.size	bitfield__write_var_BitField_offset8_count21, .-bitfield__write_var_BitField_offset8_count21
	.align 4
	.type	bitfield__read_var_BitField_offset8_count22, #function
	.proc	04
bitfield__read_var_BitField_offset8_count22:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE434:
	.size	bitfield__read_var_BitField_offset8_count22, .-bitfield__read_var_BitField_offset8_count22
	.align 4
	.type	bitfield__write_var_BitField_offset8_count22, #function
	.proc	020
bitfield__write_var_BitField_offset8_count22:
.LLFB435:
	save	%sp, -104, %sp
.LLCFI435:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL870
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL871
	 nop
.LL870:
	sethi	%hi(57344), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL871:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE435:
	.size	bitfield__write_var_BitField_offset8_count22, .-bitfield__write_var_BitField_offset8_count22
	.align 4
	.type	bitfield__read_var_BitField_offset8_count23, #function
	.proc	04
bitfield__read_var_BitField_offset8_count23:
.LLFB436:
	save	%sp, -96, %sp
.LLCFI436:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 8, %g1
	srl	%g1, 9, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE436:
	.size	bitfield__read_var_BitField_offset8_count23, .-bitfield__read_var_BitField_offset8_count23
	.align 4
	.type	bitfield__write_var_BitField_offset8_count23, #function
	.proc	020
bitfield__write_var_BitField_offset8_count23:
.LLFB437:
	save	%sp, -104, %sp
.LLCFI437:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL874
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL875
	 nop
.LL874:
	sethi	%hi(57344), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL875:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-16777216), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE437:
	.size	bitfield__write_var_BitField_offset8_count23, .-bitfield__write_var_BitField_offset8_count23
	.align 4
	.type	bitfield__read_var_BitField_offset8_count24, #function
	.proc	04
bitfield__read_var_BitField_offset8_count24:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE438:
	.size	bitfield__read_var_BitField_offset8_count24, .-bitfield__read_var_BitField_offset8_count24
	.align 4
	.type	bitfield__write_var_BitField_offset8_count24, #function
	.proc	020
bitfield__write_var_BitField_offset8_count24:
.LLFB439:
	save	%sp, -104, %sp
.LLCFI439:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL878
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL879
	 nop
.LL878:
	sethi	%hi(57344), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL879:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16777216), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE439:
	.size	bitfield__write_var_BitField_offset8_count24, .-bitfield__write_var_BitField_offset8_count24
	.align 4
	.type	bitfield__read_var_BitField_offset9_count1, #function
	.proc	04
bitfield__read_var_BitField_offset9_count1:
.LLFB440:
	save	%sp, -96, %sp
.LLCFI440:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE440:
	.size	bitfield__read_var_BitField_offset9_count1, .-bitfield__read_var_BitField_offset9_count1
	.align 4
	.type	bitfield__write_var_BitField_offset9_count1, #function
	.proc	020
bitfield__write_var_BitField_offset9_count1:
.LLFB441:
	save	%sp, -104, %sp
.LLCFI441:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL882
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL883
	 nop
.LL882:
	sethi	%hi(57344), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL883:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -65, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE441:
	.size	bitfield__write_var_BitField_offset9_count1, .-bitfield__write_var_BitField_offset9_count1
	.align 4
	.type	bitfield__read_var_BitField_offset9_count2, #function
	.proc	04
bitfield__read_var_BitField_offset9_count2:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE442:
	.size	bitfield__read_var_BitField_offset9_count2, .-bitfield__read_var_BitField_offset9_count2
	.align 4
	.type	bitfield__write_var_BitField_offset9_count2, #function
	.proc	020
bitfield__write_var_BitField_offset9_count2:
.LLFB443:
	save	%sp, -104, %sp
.LLCFI443:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL886
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL887
	 nop
.LL886:
	sethi	%hi(58368), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL887:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -97, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE443:
	.size	bitfield__write_var_BitField_offset9_count2, .-bitfield__write_var_BitField_offset9_count2
	.align 4
	.type	bitfield__read_var_BitField_offset9_count3, #function
	.proc	04
bitfield__read_var_BitField_offset9_count3:
.LLFB444:
	save	%sp, -96, %sp
.LLCFI444:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE444:
	.size	bitfield__read_var_BitField_offset9_count3, .-bitfield__read_var_BitField_offset9_count3
	.align 4
	.type	bitfield__write_var_BitField_offset9_count3, #function
	.proc	020
bitfield__write_var_BitField_offset9_count3:
.LLFB445:
	save	%sp, -104, %sp
.LLCFI445:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL890
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL891
	 nop
.LL890:
	sethi	%hi(58368), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL891:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -113, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE445:
	.size	bitfield__write_var_BitField_offset9_count3, .-bitfield__write_var_BitField_offset9_count3
	.align 4
	.type	bitfield__read_var_BitField_offset9_count4, #function
	.proc	04
bitfield__read_var_BitField_offset9_count4:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE446:
	.size	bitfield__read_var_BitField_offset9_count4, .-bitfield__read_var_BitField_offset9_count4
	.align 4
	.type	bitfield__write_var_BitField_offset9_count4, #function
	.proc	020
bitfield__write_var_BitField_offset9_count4:
.LLFB447:
	save	%sp, -104, %sp
.LLCFI447:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL894
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL895
	 nop
.LL894:
	sethi	%hi(58368), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL895:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -121, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE447:
	.size	bitfield__write_var_BitField_offset9_count4, .-bitfield__write_var_BitField_offset9_count4
	.align 4
	.type	bitfield__read_var_BitField_offset9_count5, #function
	.proc	04
bitfield__read_var_BitField_offset9_count5:
.LLFB448:
	save	%sp, -96, %sp
.LLCFI448:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE448:
	.size	bitfield__read_var_BitField_offset9_count5, .-bitfield__read_var_BitField_offset9_count5
	.align 4
	.type	bitfield__write_var_BitField_offset9_count5, #function
	.proc	020
bitfield__write_var_BitField_offset9_count5:
.LLFB449:
	save	%sp, -104, %sp
.LLCFI449:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL898
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL899
	 nop
.LL898:
	sethi	%hi(58368), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL899:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -125, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE449:
	.size	bitfield__write_var_BitField_offset9_count5, .-bitfield__write_var_BitField_offset9_count5
	.align 4
	.type	bitfield__read_var_BitField_offset9_count6, #function
	.proc	04
bitfield__read_var_BitField_offset9_count6:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE450:
	.size	bitfield__read_var_BitField_offset9_count6, .-bitfield__read_var_BitField_offset9_count6
	.align 4
	.type	bitfield__write_var_BitField_offset9_count6, #function
	.proc	020
bitfield__write_var_BitField_offset9_count6:
.LLFB451:
	save	%sp, -104, %sp
.LLCFI451:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL902
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL903
	 nop
.LL902:
	sethi	%hi(59392), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL903:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, -127, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE451:
	.size	bitfield__write_var_BitField_offset9_count6, .-bitfield__write_var_BitField_offset9_count6
	.align 4
	.type	bitfield__read_var_BitField_offset9_count7, #function
	.proc	04
bitfield__read_var_BitField_offset9_count7:
.LLFB452:
	save	%sp, -96, %sp
.LLCFI452:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE452:
	.size	bitfield__read_var_BitField_offset9_count7, .-bitfield__read_var_BitField_offset9_count7
	.align 4
	.type	bitfield__write_var_BitField_offset9_count7, #function
	.proc	020
bitfield__write_var_BitField_offset9_count7:
.LLFB453:
	save	%sp, -104, %sp
.LLCFI453:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL906
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL907
	 nop
.LL906:
	sethi	%hi(59392), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL907:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -128, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE453:
	.size	bitfield__write_var_BitField_offset9_count7, .-bitfield__write_var_BitField_offset9_count7
	.align 4
	.type	bitfield__read_var_BitField_offset9_count8, #function
	.proc	04
bitfield__read_var_BitField_offset9_count8:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE454:
	.size	bitfield__read_var_BitField_offset9_count8, .-bitfield__read_var_BitField_offset9_count8
	.align 4
	.type	bitfield__write_var_BitField_offset9_count8, #function
	.proc	020
bitfield__write_var_BitField_offset9_count8:
.LLFB455:
	save	%sp, -104, %sp
.LLCFI455:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL910
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL911
	 nop
.LL910:
	sethi	%hi(59392), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL911:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8356864), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE455:
	.size	bitfield__write_var_BitField_offset9_count8, .-bitfield__write_var_BitField_offset9_count8
	.align 4
	.type	bitfield__read_var_BitField_offset9_count9, #function
	.proc	04
bitfield__read_var_BitField_offset9_count9:
.LLFB456:
	save	%sp, -96, %sp
.LLCFI456:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE456:
	.size	bitfield__read_var_BitField_offset9_count9, .-bitfield__read_var_BitField_offset9_count9
	.align 4
	.type	bitfield__write_var_BitField_offset9_count9, #function
	.proc	020
bitfield__write_var_BitField_offset9_count9:
.LLFB457:
	save	%sp, -104, %sp
.LLCFI457:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL914
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL915
	 nop
.LL914:
	sethi	%hi(59392), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL915:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8373248), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE457:
	.size	bitfield__write_var_BitField_offset9_count9, .-bitfield__write_var_BitField_offset9_count9
	.align 4
	.type	bitfield__read_var_BitField_offset9_count10, #function
	.proc	04
bitfield__read_var_BitField_offset9_count10:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE458:
	.size	bitfield__read_var_BitField_offset9_count10, .-bitfield__read_var_BitField_offset9_count10
	.align 4
	.type	bitfield__write_var_BitField_offset9_count10, #function
	.proc	020
bitfield__write_var_BitField_offset9_count10:
.LLFB459:
	save	%sp, -104, %sp
.LLCFI459:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL918
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL919
	 nop
.LL918:
	sethi	%hi(60416), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL919:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8381440), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE459:
	.size	bitfield__write_var_BitField_offset9_count10, .-bitfield__write_var_BitField_offset9_count10
	.align 4
	.type	bitfield__read_var_BitField_offset9_count11, #function
	.proc	04
bitfield__read_var_BitField_offset9_count11:
.LLFB460:
	save	%sp, -96, %sp
.LLCFI460:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE460:
	.size	bitfield__read_var_BitField_offset9_count11, .-bitfield__read_var_BitField_offset9_count11
	.align 4
	.type	bitfield__write_var_BitField_offset9_count11, #function
	.proc	020
bitfield__write_var_BitField_offset9_count11:
.LLFB461:
	save	%sp, -104, %sp
.LLCFI461:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL922
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL923
	 nop
.LL922:
	sethi	%hi(60416), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL923:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8385536), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE461:
	.size	bitfield__write_var_BitField_offset9_count11, .-bitfield__write_var_BitField_offset9_count11
	.align 4
	.type	bitfield__read_var_BitField_offset9_count12, #function
	.proc	04
bitfield__read_var_BitField_offset9_count12:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE462:
	.size	bitfield__read_var_BitField_offset9_count12, .-bitfield__read_var_BitField_offset9_count12
	.align 4
	.type	bitfield__write_var_BitField_offset9_count12, #function
	.proc	020
bitfield__write_var_BitField_offset9_count12:
.LLFB463:
	save	%sp, -104, %sp
.LLCFI463:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL926
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL927
	 nop
.LL926:
	sethi	%hi(60416), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL927:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8387584), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE463:
	.size	bitfield__write_var_BitField_offset9_count12, .-bitfield__write_var_BitField_offset9_count12
	.align 4
	.type	bitfield__read_var_BitField_offset9_count13, #function
	.proc	04
bitfield__read_var_BitField_offset9_count13:
.LLFB464:
	save	%sp, -96, %sp
.LLCFI464:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE464:
	.size	bitfield__read_var_BitField_offset9_count13, .-bitfield__read_var_BitField_offset9_count13
	.align 4
	.type	bitfield__write_var_BitField_offset9_count13, #function
	.proc	020
bitfield__write_var_BitField_offset9_count13:
.LLFB465:
	save	%sp, -104, %sp
.LLCFI465:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL930
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL931
	 nop
.LL930:
	sethi	%hi(60416), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL931:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE465:
	.size	bitfield__write_var_BitField_offset9_count13, .-bitfield__write_var_BitField_offset9_count13
	.align 4
	.type	bitfield__read_var_BitField_offset9_count14, #function
	.proc	04
bitfield__read_var_BitField_offset9_count14:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE466:
	.size	bitfield__read_var_BitField_offset9_count14, .-bitfield__read_var_BitField_offset9_count14
	.align 4
	.type	bitfield__write_var_BitField_offset9_count14, #function
	.proc	020
bitfield__write_var_BitField_offset9_count14:
.LLFB467:
	save	%sp, -104, %sp
.LLCFI467:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL934
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL935
	 nop
.LL934:
	sethi	%hi(61440), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL935:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE467:
	.size	bitfield__write_var_BitField_offset9_count14, .-bitfield__write_var_BitField_offset9_count14
	.align 4
	.type	bitfield__read_var_BitField_offset9_count15, #function
	.proc	04
bitfield__read_var_BitField_offset9_count15:
.LLFB468:
	save	%sp, -96, %sp
.LLCFI468:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE468:
	.size	bitfield__read_var_BitField_offset9_count15, .-bitfield__read_var_BitField_offset9_count15
	.align 4
	.type	bitfield__write_var_BitField_offset9_count15, #function
	.proc	020
bitfield__write_var_BitField_offset9_count15:
.LLFB469:
	save	%sp, -104, %sp
.LLCFI469:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL938
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL939
	 nop
.LL938:
	sethi	%hi(61440), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL939:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE469:
	.size	bitfield__write_var_BitField_offset9_count15, .-bitfield__write_var_BitField_offset9_count15
	.align 4
	.type	bitfield__read_var_BitField_offset9_count16, #function
	.proc	04
bitfield__read_var_BitField_offset9_count16:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE470:
	.size	bitfield__read_var_BitField_offset9_count16, .-bitfield__read_var_BitField_offset9_count16
	.align 4
	.type	bitfield__write_var_BitField_offset9_count16, #function
	.proc	020
bitfield__write_var_BitField_offset9_count16:
.LLFB471:
	save	%sp, -104, %sp
.LLCFI471:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL942
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL943
	 nop
.LL942:
	sethi	%hi(61440), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL943:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE471:
	.size	bitfield__write_var_BitField_offset9_count16, .-bitfield__write_var_BitField_offset9_count16
	.align 4
	.type	bitfield__read_var_BitField_offset9_count17, #function
	.proc	04
bitfield__read_var_BitField_offset9_count17:
.LLFB472:
	save	%sp, -96, %sp
.LLCFI472:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE472:
	.size	bitfield__read_var_BitField_offset9_count17, .-bitfield__read_var_BitField_offset9_count17
	.align 4
	.type	bitfield__write_var_BitField_offset9_count17, #function
	.proc	020
bitfield__write_var_BitField_offset9_count17:
.LLFB473:
	save	%sp, -104, %sp
.LLCFI473:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL946
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL947
	 nop
.LL946:
	sethi	%hi(61440), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL947:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE473:
	.size	bitfield__write_var_BitField_offset9_count17, .-bitfield__write_var_BitField_offset9_count17
	.align 4
	.type	bitfield__read_var_BitField_offset9_count18, #function
	.proc	04
bitfield__read_var_BitField_offset9_count18:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE474:
	.size	bitfield__read_var_BitField_offset9_count18, .-bitfield__read_var_BitField_offset9_count18
	.align 4
	.type	bitfield__write_var_BitField_offset9_count18, #function
	.proc	020
bitfield__write_var_BitField_offset9_count18:
.LLFB475:
	save	%sp, -104, %sp
.LLCFI475:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL950
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL951
	 nop
.LL950:
	sethi	%hi(62464), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL951:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE475:
	.size	bitfield__write_var_BitField_offset9_count18, .-bitfield__write_var_BitField_offset9_count18
	.align 4
	.type	bitfield__read_var_BitField_offset9_count19, #function
	.proc	04
bitfield__read_var_BitField_offset9_count19:
.LLFB476:
	save	%sp, -96, %sp
.LLCFI476:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE476:
	.size	bitfield__read_var_BitField_offset9_count19, .-bitfield__read_var_BitField_offset9_count19
	.align 4
	.type	bitfield__write_var_BitField_offset9_count19, #function
	.proc	020
bitfield__write_var_BitField_offset9_count19:
.LLFB477:
	save	%sp, -104, %sp
.LLCFI477:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL954
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL955
	 nop
.LL954:
	sethi	%hi(62464), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL955:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE477:
	.size	bitfield__write_var_BitField_offset9_count19, .-bitfield__write_var_BitField_offset9_count19
	.align 4
	.type	bitfield__read_var_BitField_offset9_count20, #function
	.proc	04
bitfield__read_var_BitField_offset9_count20:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE478:
	.size	bitfield__read_var_BitField_offset9_count20, .-bitfield__read_var_BitField_offset9_count20
	.align 4
	.type	bitfield__write_var_BitField_offset9_count20, #function
	.proc	020
bitfield__write_var_BitField_offset9_count20:
.LLFB479:
	save	%sp, -104, %sp
.LLCFI479:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL958
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL959
	 nop
.LL958:
	sethi	%hi(62464), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL959:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE479:
	.size	bitfield__write_var_BitField_offset9_count20, .-bitfield__write_var_BitField_offset9_count20
	.align 4
	.type	bitfield__read_var_BitField_offset9_count21, #function
	.proc	04
bitfield__read_var_BitField_offset9_count21:
.LLFB480:
	save	%sp, -96, %sp
.LLCFI480:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE480:
	.size	bitfield__read_var_BitField_offset9_count21, .-bitfield__read_var_BitField_offset9_count21
	.align 4
	.type	bitfield__write_var_BitField_offset9_count21, #function
	.proc	020
bitfield__write_var_BitField_offset9_count21:
.LLFB481:
	save	%sp, -104, %sp
.LLCFI481:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL962
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL963
	 nop
.LL962:
	sethi	%hi(62464), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL963:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE481:
	.size	bitfield__write_var_BitField_offset9_count21, .-bitfield__write_var_BitField_offset9_count21
	.align 4
	.type	bitfield__read_var_BitField_offset9_count22, #function
	.proc	04
bitfield__read_var_BitField_offset9_count22:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 9, %g1
	srl	%g1, 10, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	bitfield__read_var_BitField_offset9_count22, .-bitfield__read_var_BitField_offset9_count22
	.align 4
	.type	bitfield__write_var_BitField_offset9_count22, #function
	.proc	020
bitfield__write_var_BitField_offset9_count22:
.LLFB483:
	save	%sp, -104, %sp
.LLCFI483:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL966
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL967
	 nop
.LL966:
	sethi	%hi(63488), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL967:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-8388608), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE483:
	.size	bitfield__write_var_BitField_offset9_count22, .-bitfield__write_var_BitField_offset9_count22
	.align 4
	.type	bitfield__read_var_BitField_offset9_count23, #function
	.proc	04
bitfield__read_var_BitField_offset9_count23:
.LLFB484:
	save	%sp, -96, %sp
.LLCFI484:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	bitfield__read_var_BitField_offset9_count23, .-bitfield__read_var_BitField_offset9_count23
	.align 4
	.type	bitfield__write_var_BitField_offset9_count23, #function
	.proc	020
bitfield__write_var_BitField_offset9_count23:
.LLFB485:
	save	%sp, -104, %sp
.LLCFI485:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL970
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL971
	 nop
.LL970:
	sethi	%hi(63488), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL971:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8388608), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE485:
	.size	bitfield__write_var_BitField_offset9_count23, .-bitfield__write_var_BitField_offset9_count23
	.align 4
	.type	bitfield__read_var_BitField_offset10_count1, #function
	.proc	04
bitfield__read_var_BitField_offset10_count1:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE486:
	.size	bitfield__read_var_BitField_offset10_count1, .-bitfield__read_var_BitField_offset10_count1
	.align 4
	.type	bitfield__write_var_BitField_offset10_count1, #function
	.proc	020
bitfield__write_var_BitField_offset10_count1:
.LLFB487:
	save	%sp, -104, %sp
.LLCFI487:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL974
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL975
	 nop
.LL974:
	sethi	%hi(63488), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL975:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -33, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE487:
	.size	bitfield__write_var_BitField_offset10_count1, .-bitfield__write_var_BitField_offset10_count1
	.align 4
	.type	bitfield__read_var_BitField_offset10_count2, #function
	.proc	04
bitfield__read_var_BitField_offset10_count2:
.LLFB488:
	save	%sp, -96, %sp
.LLCFI488:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE488:
	.size	bitfield__read_var_BitField_offset10_count2, .-bitfield__read_var_BitField_offset10_count2
	.align 4
	.type	bitfield__write_var_BitField_offset10_count2, #function
	.proc	020
bitfield__write_var_BitField_offset10_count2:
.LLFB489:
	save	%sp, -104, %sp
.LLCFI489:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL978
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL979
	 nop
.LL978:
	sethi	%hi(63488), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL979:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -49, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE489:
	.size	bitfield__write_var_BitField_offset10_count2, .-bitfield__write_var_BitField_offset10_count2
	.align 4
	.type	bitfield__read_var_BitField_offset10_count3, #function
	.proc	04
bitfield__read_var_BitField_offset10_count3:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE490:
	.size	bitfield__read_var_BitField_offset10_count3, .-bitfield__read_var_BitField_offset10_count3
	.align 4
	.type	bitfield__write_var_BitField_offset10_count3, #function
	.proc	020
bitfield__write_var_BitField_offset10_count3:
.LLFB491:
	save	%sp, -104, %sp
.LLCFI491:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL982
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL983
	 nop
.LL982:
	sethi	%hi(64512), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL983:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -57, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE491:
	.size	bitfield__write_var_BitField_offset10_count3, .-bitfield__write_var_BitField_offset10_count3
	.align 4
	.type	bitfield__read_var_BitField_offset10_count4, #function
	.proc	04
bitfield__read_var_BitField_offset10_count4:
.LLFB492:
	save	%sp, -96, %sp
.LLCFI492:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE492:
	.size	bitfield__read_var_BitField_offset10_count4, .-bitfield__read_var_BitField_offset10_count4
	.align 4
	.type	bitfield__write_var_BitField_offset10_count4, #function
	.proc	020
bitfield__write_var_BitField_offset10_count4:
.LLFB493:
	save	%sp, -104, %sp
.LLCFI493:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL986
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL987
	 nop
.LL986:
	sethi	%hi(64512), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL987:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -61, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE493:
	.size	bitfield__write_var_BitField_offset10_count4, .-bitfield__write_var_BitField_offset10_count4
	.align 4
	.type	bitfield__read_var_BitField_offset10_count5, #function
	.proc	04
bitfield__read_var_BitField_offset10_count5:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	bitfield__read_var_BitField_offset10_count5, .-bitfield__read_var_BitField_offset10_count5
	.align 4
	.type	bitfield__write_var_BitField_offset10_count5, #function
	.proc	020
bitfield__write_var_BitField_offset10_count5:
.LLFB495:
	save	%sp, -104, %sp
.LLCFI495:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL990
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL991
	 nop
.LL990:
	sethi	%hi(64512), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL991:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, -63, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE495:
	.size	bitfield__write_var_BitField_offset10_count5, .-bitfield__write_var_BitField_offset10_count5
	.align 4
	.type	bitfield__read_var_BitField_offset10_count6, #function
	.proc	04
bitfield__read_var_BitField_offset10_count6:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	bitfield__read_var_BitField_offset10_count6, .-bitfield__read_var_BitField_offset10_count6
	.align 4
	.type	bitfield__write_var_BitField_offset10_count6, #function
	.proc	020
bitfield__write_var_BitField_offset10_count6:
.LLFB497:
	save	%sp, -104, %sp
.LLCFI497:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL994
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL995
	 nop
.LL994:
	sethi	%hi(64512), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL995:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -64, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE497:
	.size	bitfield__write_var_BitField_offset10_count6, .-bitfield__write_var_BitField_offset10_count6
	.align 4
	.type	bitfield__read_var_BitField_offset10_count7, #function
	.proc	04
bitfield__read_var_BitField_offset10_count7:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	bitfield__read_var_BitField_offset10_count7, .-bitfield__read_var_BitField_offset10_count7
	.align 4
	.type	bitfield__write_var_BitField_offset10_count7, #function
	.proc	020
bitfield__write_var_BitField_offset10_count7:
.LLFB499:
	save	%sp, -104, %sp
.LLCFI499:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL998
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL999
	 nop
.LL998:
	sethi	%hi(65536), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL999:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4162560), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE499:
	.size	bitfield__write_var_BitField_offset10_count7, .-bitfield__write_var_BitField_offset10_count7
	.align 4
	.type	bitfield__read_var_BitField_offset10_count8, #function
	.proc	04
bitfield__read_var_BitField_offset10_count8:
.LLFB500:
	save	%sp, -96, %sp
.LLCFI500:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	bitfield__read_var_BitField_offset10_count8, .-bitfield__read_var_BitField_offset10_count8
	.align 4
	.type	bitfield__write_var_BitField_offset10_count8, #function
	.proc	020
bitfield__write_var_BitField_offset10_count8:
.LLFB501:
	save	%sp, -104, %sp
.LLCFI501:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1002
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1003
	 nop
.LL1002:
	sethi	%hi(65536), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1003:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4178944), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE501:
	.size	bitfield__write_var_BitField_offset10_count8, .-bitfield__write_var_BitField_offset10_count8
	.align 4
	.type	bitfield__read_var_BitField_offset10_count9, #function
	.proc	04
bitfield__read_var_BitField_offset10_count9:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	bitfield__read_var_BitField_offset10_count9, .-bitfield__read_var_BitField_offset10_count9
	.align 4
	.type	bitfield__write_var_BitField_offset10_count9, #function
	.proc	020
bitfield__write_var_BitField_offset10_count9:
.LLFB503:
	save	%sp, -104, %sp
.LLCFI503:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1006
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1007
	 nop
.LL1006:
	sethi	%hi(65536), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1007:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4187136), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE503:
	.size	bitfield__write_var_BitField_offset10_count9, .-bitfield__write_var_BitField_offset10_count9
	.align 4
	.type	bitfield__read_var_BitField_offset10_count10, #function
	.proc	04
bitfield__read_var_BitField_offset10_count10:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	bitfield__read_var_BitField_offset10_count10, .-bitfield__read_var_BitField_offset10_count10
	.align 4
	.type	bitfield__write_var_BitField_offset10_count10, #function
	.proc	020
bitfield__write_var_BitField_offset10_count10:
.LLFB505:
	save	%sp, -104, %sp
.LLCFI505:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1010
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1011
	 nop
.LL1010:
	sethi	%hi(65536), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1011:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4191232), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE505:
	.size	bitfield__write_var_BitField_offset10_count10, .-bitfield__write_var_BitField_offset10_count10
	.align 4
	.type	bitfield__read_var_BitField_offset10_count11, #function
	.proc	04
bitfield__read_var_BitField_offset10_count11:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE506:
	.size	bitfield__read_var_BitField_offset10_count11, .-bitfield__read_var_BitField_offset10_count11
	.align 4
	.type	bitfield__write_var_BitField_offset10_count11, #function
	.proc	020
bitfield__write_var_BitField_offset10_count11:
.LLFB507:
	save	%sp, -104, %sp
.LLCFI507:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1014
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1015
	 nop
.LL1014:
	sethi	%hi(66560), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1015:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4193280), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE507:
	.size	bitfield__write_var_BitField_offset10_count11, .-bitfield__write_var_BitField_offset10_count11
	.align 4
	.type	bitfield__read_var_BitField_offset10_count12, #function
	.proc	04
bitfield__read_var_BitField_offset10_count12:
.LLFB508:
	save	%sp, -96, %sp
.LLCFI508:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE508:
	.size	bitfield__read_var_BitField_offset10_count12, .-bitfield__read_var_BitField_offset10_count12
	.align 4
	.type	bitfield__write_var_BitField_offset10_count12, #function
	.proc	020
bitfield__write_var_BitField_offset10_count12:
.LLFB509:
	save	%sp, -104, %sp
.LLCFI509:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1018
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1019
	 nop
.LL1018:
	sethi	%hi(66560), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1019:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE509:
	.size	bitfield__write_var_BitField_offset10_count12, .-bitfield__write_var_BitField_offset10_count12
	.align 4
	.type	bitfield__read_var_BitField_offset10_count13, #function
	.proc	04
bitfield__read_var_BitField_offset10_count13:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	bitfield__read_var_BitField_offset10_count13, .-bitfield__read_var_BitField_offset10_count13
	.align 4
	.type	bitfield__write_var_BitField_offset10_count13, #function
	.proc	020
bitfield__write_var_BitField_offset10_count13:
.LLFB511:
	save	%sp, -104, %sp
.LLCFI511:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1022
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1023
	 nop
.LL1022:
	sethi	%hi(66560), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1023:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE511:
	.size	bitfield__write_var_BitField_offset10_count13, .-bitfield__write_var_BitField_offset10_count13
	.align 4
	.type	bitfield__read_var_BitField_offset10_count14, #function
	.proc	04
bitfield__read_var_BitField_offset10_count14:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	bitfield__read_var_BitField_offset10_count14, .-bitfield__read_var_BitField_offset10_count14
	.align 4
	.type	bitfield__write_var_BitField_offset10_count14, #function
	.proc	020
bitfield__write_var_BitField_offset10_count14:
.LLFB513:
	save	%sp, -104, %sp
.LLCFI513:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1026
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1027
	 nop
.LL1026:
	sethi	%hi(66560), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1027:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE513:
	.size	bitfield__write_var_BitField_offset10_count14, .-bitfield__write_var_BitField_offset10_count14
	.align 4
	.type	bitfield__read_var_BitField_offset10_count15, #function
	.proc	04
bitfield__read_var_BitField_offset10_count15:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	bitfield__read_var_BitField_offset10_count15, .-bitfield__read_var_BitField_offset10_count15
	.align 4
	.type	bitfield__write_var_BitField_offset10_count15, #function
	.proc	020
bitfield__write_var_BitField_offset10_count15:
.LLFB515:
	save	%sp, -104, %sp
.LLCFI515:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1030
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1031
	 nop
.LL1030:
	sethi	%hi(67584), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1031:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE515:
	.size	bitfield__write_var_BitField_offset10_count15, .-bitfield__write_var_BitField_offset10_count15
	.align 4
	.type	bitfield__read_var_BitField_offset10_count16, #function
	.proc	04
bitfield__read_var_BitField_offset10_count16:
.LLFB516:
	save	%sp, -96, %sp
.LLCFI516:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	bitfield__read_var_BitField_offset10_count16, .-bitfield__read_var_BitField_offset10_count16
	.align 4
	.type	bitfield__write_var_BitField_offset10_count16, #function
	.proc	020
bitfield__write_var_BitField_offset10_count16:
.LLFB517:
	save	%sp, -104, %sp
.LLCFI517:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1034
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1035
	 nop
.LL1034:
	sethi	%hi(67584), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1035:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE517:
	.size	bitfield__write_var_BitField_offset10_count16, .-bitfield__write_var_BitField_offset10_count16
	.align 4
	.type	bitfield__read_var_BitField_offset10_count17, #function
	.proc	04
bitfield__read_var_BitField_offset10_count17:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE518:
	.size	bitfield__read_var_BitField_offset10_count17, .-bitfield__read_var_BitField_offset10_count17
	.align 4
	.type	bitfield__write_var_BitField_offset10_count17, #function
	.proc	020
bitfield__write_var_BitField_offset10_count17:
.LLFB519:
	save	%sp, -104, %sp
.LLCFI519:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1038
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1039
	 nop
.LL1038:
	sethi	%hi(67584), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1039:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE519:
	.size	bitfield__write_var_BitField_offset10_count17, .-bitfield__write_var_BitField_offset10_count17
	.align 4
	.type	bitfield__read_var_BitField_offset10_count18, #function
	.proc	04
bitfield__read_var_BitField_offset10_count18:
.LLFB520:
	save	%sp, -96, %sp
.LLCFI520:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE520:
	.size	bitfield__read_var_BitField_offset10_count18, .-bitfield__read_var_BitField_offset10_count18
	.align 4
	.type	bitfield__write_var_BitField_offset10_count18, #function
	.proc	020
bitfield__write_var_BitField_offset10_count18:
.LLFB521:
	save	%sp, -104, %sp
.LLCFI521:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1042
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1043
	 nop
.LL1042:
	sethi	%hi(67584), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1043:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE521:
	.size	bitfield__write_var_BitField_offset10_count18, .-bitfield__write_var_BitField_offset10_count18
	.align 4
	.type	bitfield__read_var_BitField_offset10_count19, #function
	.proc	04
bitfield__read_var_BitField_offset10_count19:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	bitfield__read_var_BitField_offset10_count19, .-bitfield__read_var_BitField_offset10_count19
	.align 4
	.type	bitfield__write_var_BitField_offset10_count19, #function
	.proc	020
bitfield__write_var_BitField_offset10_count19:
.LLFB523:
	save	%sp, -104, %sp
.LLCFI523:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1046
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1047
	 nop
.LL1046:
	sethi	%hi(68608), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1047:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE523:
	.size	bitfield__write_var_BitField_offset10_count19, .-bitfield__write_var_BitField_offset10_count19
	.align 4
	.type	bitfield__read_var_BitField_offset10_count20, #function
	.proc	04
bitfield__read_var_BitField_offset10_count20:
.LLFB524:
	save	%sp, -96, %sp
.LLCFI524:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	bitfield__read_var_BitField_offset10_count20, .-bitfield__read_var_BitField_offset10_count20
	.align 4
	.type	bitfield__write_var_BitField_offset10_count20, #function
	.proc	020
bitfield__write_var_BitField_offset10_count20:
.LLFB525:
	save	%sp, -104, %sp
.LLCFI525:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1050
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1051
	 nop
.LL1050:
	sethi	%hi(68608), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1051:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE525:
	.size	bitfield__write_var_BitField_offset10_count20, .-bitfield__write_var_BitField_offset10_count20
	.align 4
	.type	bitfield__read_var_BitField_offset10_count21, #function
	.proc	04
bitfield__read_var_BitField_offset10_count21:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 10, %g1
	srl	%g1, 11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	bitfield__read_var_BitField_offset10_count21, .-bitfield__read_var_BitField_offset10_count21
	.align 4
	.type	bitfield__write_var_BitField_offset10_count21, #function
	.proc	020
bitfield__write_var_BitField_offset10_count21:
.LLFB527:
	save	%sp, -104, %sp
.LLCFI527:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1054
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1055
	 nop
.LL1054:
	sethi	%hi(68608), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1055:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-4194304), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE527:
	.size	bitfield__write_var_BitField_offset10_count21, .-bitfield__write_var_BitField_offset10_count21
	.align 4
	.type	bitfield__read_var_BitField_offset10_count22, #function
	.proc	04
bitfield__read_var_BitField_offset10_count22:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	bitfield__read_var_BitField_offset10_count22, .-bitfield__read_var_BitField_offset10_count22
	.align 4
	.type	bitfield__write_var_BitField_offset10_count22, #function
	.proc	020
bitfield__write_var_BitField_offset10_count22:
.LLFB529:
	save	%sp, -104, %sp
.LLCFI529:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1058
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1059
	 nop
.LL1058:
	sethi	%hi(68608), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1059:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-4194304), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE529:
	.size	bitfield__write_var_BitField_offset10_count22, .-bitfield__write_var_BitField_offset10_count22
	.align 4
	.type	bitfield__read_var_BitField_offset11_count1, #function
	.proc	04
bitfield__read_var_BitField_offset11_count1:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	bitfield__read_var_BitField_offset11_count1, .-bitfield__read_var_BitField_offset11_count1
	.align 4
	.type	bitfield__write_var_BitField_offset11_count1, #function
	.proc	020
bitfield__write_var_BitField_offset11_count1:
.LLFB531:
	save	%sp, -104, %sp
.LLCFI531:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1062
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1063
	 nop
.LL1062:
	sethi	%hi(69632), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1063:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -17, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE531:
	.size	bitfield__write_var_BitField_offset11_count1, .-bitfield__write_var_BitField_offset11_count1
	.align 4
	.type	bitfield__read_var_BitField_offset11_count2, #function
	.proc	04
bitfield__read_var_BitField_offset11_count2:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	bitfield__read_var_BitField_offset11_count2, .-bitfield__read_var_BitField_offset11_count2
	.align 4
	.type	bitfield__write_var_BitField_offset11_count2, #function
	.proc	020
bitfield__write_var_BitField_offset11_count2:
.LLFB533:
	save	%sp, -104, %sp
.LLCFI533:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1066
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1067
	 nop
.LL1066:
	sethi	%hi(69632), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1067:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -25, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE533:
	.size	bitfield__write_var_BitField_offset11_count2, .-bitfield__write_var_BitField_offset11_count2
	.align 4
	.type	bitfield__read_var_BitField_offset11_count3, #function
	.proc	04
bitfield__read_var_BitField_offset11_count3:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE534:
	.size	bitfield__read_var_BitField_offset11_count3, .-bitfield__read_var_BitField_offset11_count3
	.align 4
	.type	bitfield__write_var_BitField_offset11_count3, #function
	.proc	020
bitfield__write_var_BitField_offset11_count3:
.LLFB535:
	save	%sp, -104, %sp
.LLCFI535:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1070
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1071
	 nop
.LL1070:
	sethi	%hi(69632), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1071:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -29, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE535:
	.size	bitfield__write_var_BitField_offset11_count3, .-bitfield__write_var_BitField_offset11_count3
	.align 4
	.type	bitfield__read_var_BitField_offset11_count4, #function
	.proc	04
bitfield__read_var_BitField_offset11_count4:
.LLFB536:
	save	%sp, -96, %sp
.LLCFI536:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE536:
	.size	bitfield__read_var_BitField_offset11_count4, .-bitfield__read_var_BitField_offset11_count4
	.align 4
	.type	bitfield__write_var_BitField_offset11_count4, #function
	.proc	020
bitfield__write_var_BitField_offset11_count4:
.LLFB537:
	save	%sp, -104, %sp
.LLCFI537:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1074
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1075
	 nop
.LL1074:
	sethi	%hi(69632), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1075:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, -31, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE537:
	.size	bitfield__write_var_BitField_offset11_count4, .-bitfield__write_var_BitField_offset11_count4
	.align 4
	.type	bitfield__read_var_BitField_offset11_count5, #function
	.proc	04
bitfield__read_var_BitField_offset11_count5:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE538:
	.size	bitfield__read_var_BitField_offset11_count5, .-bitfield__read_var_BitField_offset11_count5
	.align 4
	.type	bitfield__write_var_BitField_offset11_count5, #function
	.proc	020
bitfield__write_var_BitField_offset11_count5:
.LLFB539:
	save	%sp, -104, %sp
.LLCFI539:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1078
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1079
	 nop
.LL1078:
	sethi	%hi(70656), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1079:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -32, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE539:
	.size	bitfield__write_var_BitField_offset11_count5, .-bitfield__write_var_BitField_offset11_count5
	.align 4
	.type	bitfield__read_var_BitField_offset11_count6, #function
	.proc	04
bitfield__read_var_BitField_offset11_count6:
.LLFB540:
	save	%sp, -96, %sp
.LLCFI540:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE540:
	.size	bitfield__read_var_BitField_offset11_count6, .-bitfield__read_var_BitField_offset11_count6
	.align 4
	.type	bitfield__write_var_BitField_offset11_count6, #function
	.proc	020
bitfield__write_var_BitField_offset11_count6:
.LLFB541:
	save	%sp, -104, %sp
.LLCFI541:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1082
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1083
	 nop
.LL1082:
	sethi	%hi(70656), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1083:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2065408), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE541:
	.size	bitfield__write_var_BitField_offset11_count6, .-bitfield__write_var_BitField_offset11_count6
	.align 4
	.type	bitfield__read_var_BitField_offset11_count7, #function
	.proc	04
bitfield__read_var_BitField_offset11_count7:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	bitfield__read_var_BitField_offset11_count7, .-bitfield__read_var_BitField_offset11_count7
	.align 4
	.type	bitfield__write_var_BitField_offset11_count7, #function
	.proc	020
bitfield__write_var_BitField_offset11_count7:
.LLFB543:
	save	%sp, -104, %sp
.LLCFI543:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1086
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1087
	 nop
.LL1086:
	sethi	%hi(70656), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1087:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2081792), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE543:
	.size	bitfield__write_var_BitField_offset11_count7, .-bitfield__write_var_BitField_offset11_count7
	.align 4
	.type	bitfield__read_var_BitField_offset11_count8, #function
	.proc	04
bitfield__read_var_BitField_offset11_count8:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	bitfield__read_var_BitField_offset11_count8, .-bitfield__read_var_BitField_offset11_count8
	.align 4
	.type	bitfield__write_var_BitField_offset11_count8, #function
	.proc	020
bitfield__write_var_BitField_offset11_count8:
.LLFB545:
	save	%sp, -104, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1090
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1091
	 nop
.LL1090:
	sethi	%hi(70656), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1091:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2089984), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE545:
	.size	bitfield__write_var_BitField_offset11_count8, .-bitfield__write_var_BitField_offset11_count8
	.align 4
	.type	bitfield__read_var_BitField_offset11_count9, #function
	.proc	04
bitfield__read_var_BitField_offset11_count9:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	bitfield__read_var_BitField_offset11_count9, .-bitfield__read_var_BitField_offset11_count9
	.align 4
	.type	bitfield__write_var_BitField_offset11_count9, #function
	.proc	020
bitfield__write_var_BitField_offset11_count9:
.LLFB547:
	save	%sp, -104, %sp
.LLCFI547:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1094
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1095
	 nop
.LL1094:
	sethi	%hi(71680), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1095:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2094080), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE547:
	.size	bitfield__write_var_BitField_offset11_count9, .-bitfield__write_var_BitField_offset11_count9
	.align 4
	.type	bitfield__read_var_BitField_offset11_count10, #function
	.proc	04
bitfield__read_var_BitField_offset11_count10:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	bitfield__read_var_BitField_offset11_count10, .-bitfield__read_var_BitField_offset11_count10
	.align 4
	.type	bitfield__write_var_BitField_offset11_count10, #function
	.proc	020
bitfield__write_var_BitField_offset11_count10:
.LLFB549:
	save	%sp, -104, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1098
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1099
	 nop
.LL1098:
	sethi	%hi(71680), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1099:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2096128), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE549:
	.size	bitfield__write_var_BitField_offset11_count10, .-bitfield__write_var_BitField_offset11_count10
	.align 4
	.type	bitfield__read_var_BitField_offset11_count11, #function
	.proc	04
bitfield__read_var_BitField_offset11_count11:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	bitfield__read_var_BitField_offset11_count11, .-bitfield__read_var_BitField_offset11_count11
	.align 4
	.type	bitfield__write_var_BitField_offset11_count11, #function
	.proc	020
bitfield__write_var_BitField_offset11_count11:
.LLFB551:
	save	%sp, -104, %sp
.LLCFI551:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1102
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1103
	 nop
.LL1102:
	sethi	%hi(71680), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1103:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE551:
	.size	bitfield__write_var_BitField_offset11_count11, .-bitfield__write_var_BitField_offset11_count11
	.align 4
	.type	bitfield__read_var_BitField_offset11_count12, #function
	.proc	04
bitfield__read_var_BitField_offset11_count12:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	bitfield__read_var_BitField_offset11_count12, .-bitfield__read_var_BitField_offset11_count12
	.align 4
	.type	bitfield__write_var_BitField_offset11_count12, #function
	.proc	020
bitfield__write_var_BitField_offset11_count12:
.LLFB553:
	save	%sp, -104, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1106
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1107
	 nop
.LL1106:
	sethi	%hi(71680), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1107:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE553:
	.size	bitfield__write_var_BitField_offset11_count12, .-bitfield__write_var_BitField_offset11_count12
	.align 4
	.type	bitfield__read_var_BitField_offset11_count13, #function
	.proc	04
bitfield__read_var_BitField_offset11_count13:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE554:
	.size	bitfield__read_var_BitField_offset11_count13, .-bitfield__read_var_BitField_offset11_count13
	.align 4
	.type	bitfield__write_var_BitField_offset11_count13, #function
	.proc	020
bitfield__write_var_BitField_offset11_count13:
.LLFB555:
	save	%sp, -104, %sp
.LLCFI555:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1110
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1111
	 nop
.LL1110:
	sethi	%hi(72704), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1111:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE555:
	.size	bitfield__write_var_BitField_offset11_count13, .-bitfield__write_var_BitField_offset11_count13
	.align 4
	.type	bitfield__read_var_BitField_offset11_count14, #function
	.proc	04
bitfield__read_var_BitField_offset11_count14:
.LLFB556:
	save	%sp, -96, %sp
.LLCFI556:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE556:
	.size	bitfield__read_var_BitField_offset11_count14, .-bitfield__read_var_BitField_offset11_count14
	.align 4
	.type	bitfield__write_var_BitField_offset11_count14, #function
	.proc	020
bitfield__write_var_BitField_offset11_count14:
.LLFB557:
	save	%sp, -104, %sp
.LLCFI557:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1114
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1115
	 nop
.LL1114:
	sethi	%hi(72704), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1115:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE557:
	.size	bitfield__write_var_BitField_offset11_count14, .-bitfield__write_var_BitField_offset11_count14
	.align 4
	.type	bitfield__read_var_BitField_offset11_count15, #function
	.proc	04
bitfield__read_var_BitField_offset11_count15:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	bitfield__read_var_BitField_offset11_count15, .-bitfield__read_var_BitField_offset11_count15
	.align 4
	.type	bitfield__write_var_BitField_offset11_count15, #function
	.proc	020
bitfield__write_var_BitField_offset11_count15:
.LLFB559:
	save	%sp, -104, %sp
.LLCFI559:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1118
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1119
	 nop
.LL1118:
	sethi	%hi(72704), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1119:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE559:
	.size	bitfield__write_var_BitField_offset11_count15, .-bitfield__write_var_BitField_offset11_count15
	.align 4
	.type	bitfield__read_var_BitField_offset11_count16, #function
	.proc	04
bitfield__read_var_BitField_offset11_count16:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	bitfield__read_var_BitField_offset11_count16, .-bitfield__read_var_BitField_offset11_count16
	.align 4
	.type	bitfield__write_var_BitField_offset11_count16, #function
	.proc	020
bitfield__write_var_BitField_offset11_count16:
.LLFB561:
	save	%sp, -104, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1122
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1123
	 nop
.LL1122:
	sethi	%hi(72704), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1123:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE561:
	.size	bitfield__write_var_BitField_offset11_count16, .-bitfield__write_var_BitField_offset11_count16
	.align 4
	.type	bitfield__read_var_BitField_offset11_count17, #function
	.proc	04
bitfield__read_var_BitField_offset11_count17:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	bitfield__read_var_BitField_offset11_count17, .-bitfield__read_var_BitField_offset11_count17
	.align 4
	.type	bitfield__write_var_BitField_offset11_count17, #function
	.proc	020
bitfield__write_var_BitField_offset11_count17:
.LLFB563:
	save	%sp, -104, %sp
.LLCFI563:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1126
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1127
	 nop
.LL1126:
	sethi	%hi(73728), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1127:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE563:
	.size	bitfield__write_var_BitField_offset11_count17, .-bitfield__write_var_BitField_offset11_count17
	.align 4
	.type	bitfield__read_var_BitField_offset11_count18, #function
	.proc	04
bitfield__read_var_BitField_offset11_count18:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	bitfield__read_var_BitField_offset11_count18, .-bitfield__read_var_BitField_offset11_count18
	.align 4
	.type	bitfield__write_var_BitField_offset11_count18, #function
	.proc	020
bitfield__write_var_BitField_offset11_count18:
.LLFB565:
	save	%sp, -104, %sp
.LLCFI565:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1130
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1131
	 nop
.LL1130:
	sethi	%hi(73728), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1131:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE565:
	.size	bitfield__write_var_BitField_offset11_count18, .-bitfield__write_var_BitField_offset11_count18
	.align 4
	.type	bitfield__read_var_BitField_offset11_count19, #function
	.proc	04
bitfield__read_var_BitField_offset11_count19:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE566:
	.size	bitfield__read_var_BitField_offset11_count19, .-bitfield__read_var_BitField_offset11_count19
	.align 4
	.type	bitfield__write_var_BitField_offset11_count19, #function
	.proc	020
bitfield__write_var_BitField_offset11_count19:
.LLFB567:
	save	%sp, -104, %sp
.LLCFI567:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1134
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1135
	 nop
.LL1134:
	sethi	%hi(73728), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1135:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE567:
	.size	bitfield__write_var_BitField_offset11_count19, .-bitfield__write_var_BitField_offset11_count19
	.align 4
	.type	bitfield__read_var_BitField_offset11_count20, #function
	.proc	04
bitfield__read_var_BitField_offset11_count20:
.LLFB568:
	save	%sp, -96, %sp
.LLCFI568:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 11, %g1
	srl	%g1, 12, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE568:
	.size	bitfield__read_var_BitField_offset11_count20, .-bitfield__read_var_BitField_offset11_count20
	.align 4
	.type	bitfield__write_var_BitField_offset11_count20, #function
	.proc	020
bitfield__write_var_BitField_offset11_count20:
.LLFB569:
	save	%sp, -104, %sp
.LLCFI569:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1138
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1139
	 nop
.LL1138:
	sethi	%hi(73728), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1139:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-2097152), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE569:
	.size	bitfield__write_var_BitField_offset11_count20, .-bitfield__write_var_BitField_offset11_count20
	.align 4
	.type	bitfield__read_var_BitField_offset11_count21, #function
	.proc	04
bitfield__read_var_BitField_offset11_count21:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	bitfield__read_var_BitField_offset11_count21, .-bitfield__read_var_BitField_offset11_count21
	.align 4
	.type	bitfield__write_var_BitField_offset11_count21, #function
	.proc	020
bitfield__write_var_BitField_offset11_count21:
.LLFB571:
	save	%sp, -104, %sp
.LLCFI571:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1142
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1143
	 nop
.LL1142:
	sethi	%hi(74752), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1143:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-2097152), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE571:
	.size	bitfield__write_var_BitField_offset11_count21, .-bitfield__write_var_BitField_offset11_count21
	.align 4
	.type	bitfield__read_var_BitField_offset12_count1, #function
	.proc	04
bitfield__read_var_BitField_offset12_count1:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	bitfield__read_var_BitField_offset12_count1, .-bitfield__read_var_BitField_offset12_count1
	.align 4
	.type	bitfield__write_var_BitField_offset12_count1, #function
	.proc	020
bitfield__write_var_BitField_offset12_count1:
.LLFB573:
	save	%sp, -104, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1146
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1147
	 nop
.LL1146:
	sethi	%hi(74752), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1147:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -9, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE573:
	.size	bitfield__write_var_BitField_offset12_count1, .-bitfield__write_var_BitField_offset12_count1
	.align 4
	.type	bitfield__read_var_BitField_offset12_count2, #function
	.proc	04
bitfield__read_var_BitField_offset12_count2:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	bitfield__read_var_BitField_offset12_count2, .-bitfield__read_var_BitField_offset12_count2
	.align 4
	.type	bitfield__write_var_BitField_offset12_count2, #function
	.proc	020
bitfield__write_var_BitField_offset12_count2:
.LLFB575:
	save	%sp, -104, %sp
.LLCFI575:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1150
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1151
	 nop
.LL1150:
	sethi	%hi(74752), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1151:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -13, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE575:
	.size	bitfield__write_var_BitField_offset12_count2, .-bitfield__write_var_BitField_offset12_count2
	.align 4
	.type	bitfield__read_var_BitField_offset12_count3, #function
	.proc	04
bitfield__read_var_BitField_offset12_count3:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	bitfield__read_var_BitField_offset12_count3, .-bitfield__read_var_BitField_offset12_count3
	.align 4
	.type	bitfield__write_var_BitField_offset12_count3, #function
	.proc	020
bitfield__write_var_BitField_offset12_count3:
.LLFB577:
	save	%sp, -104, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1154
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1155
	 nop
.LL1154:
	sethi	%hi(74752), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1155:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, -15, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE577:
	.size	bitfield__write_var_BitField_offset12_count3, .-bitfield__write_var_BitField_offset12_count3
	.align 4
	.type	bitfield__read_var_BitField_offset12_count4, #function
	.proc	04
bitfield__read_var_BitField_offset12_count4:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE578:
	.size	bitfield__read_var_BitField_offset12_count4, .-bitfield__read_var_BitField_offset12_count4
	.align 4
	.type	bitfield__write_var_BitField_offset12_count4, #function
	.proc	020
bitfield__write_var_BitField_offset12_count4:
.LLFB579:
	save	%sp, -104, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1158
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1159
	 nop
.LL1158:
	sethi	%hi(75776), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1159:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -16, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE579:
	.size	bitfield__write_var_BitField_offset12_count4, .-bitfield__write_var_BitField_offset12_count4
	.align 4
	.type	bitfield__read_var_BitField_offset12_count5, #function
	.proc	04
bitfield__read_var_BitField_offset12_count5:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	bitfield__read_var_BitField_offset12_count5, .-bitfield__read_var_BitField_offset12_count5
	.align 4
	.type	bitfield__write_var_BitField_offset12_count5, #function
	.proc	020
bitfield__write_var_BitField_offset12_count5:
.LLFB581:
	save	%sp, -104, %sp
.LLCFI581:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1162
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1163
	 nop
.LL1162:
	sethi	%hi(75776), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1163:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1016832), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE581:
	.size	bitfield__write_var_BitField_offset12_count5, .-bitfield__write_var_BitField_offset12_count5
	.align 4
	.type	bitfield__read_var_BitField_offset12_count6, #function
	.proc	04
bitfield__read_var_BitField_offset12_count6:
.LLFB582:
	save	%sp, -96, %sp
.LLCFI582:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE582:
	.size	bitfield__read_var_BitField_offset12_count6, .-bitfield__read_var_BitField_offset12_count6
	.align 4
	.type	bitfield__write_var_BitField_offset12_count6, #function
	.proc	020
bitfield__write_var_BitField_offset12_count6:
.LLFB583:
	save	%sp, -104, %sp
.LLCFI583:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1166
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1167
	 nop
.LL1166:
	sethi	%hi(75776), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1167:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1033216), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE583:
	.size	bitfield__write_var_BitField_offset12_count6, .-bitfield__write_var_BitField_offset12_count6
	.align 4
	.type	bitfield__read_var_BitField_offset12_count7, #function
	.proc	04
bitfield__read_var_BitField_offset12_count7:
.LLFB584:
	save	%sp, -96, %sp
.LLCFI584:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE584:
	.size	bitfield__read_var_BitField_offset12_count7, .-bitfield__read_var_BitField_offset12_count7
	.align 4
	.type	bitfield__write_var_BitField_offset12_count7, #function
	.proc	020
bitfield__write_var_BitField_offset12_count7:
.LLFB585:
	save	%sp, -104, %sp
.LLCFI585:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1170
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1171
	 nop
.LL1170:
	sethi	%hi(75776), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1171:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1041408), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE585:
	.size	bitfield__write_var_BitField_offset12_count7, .-bitfield__write_var_BitField_offset12_count7
	.align 4
	.type	bitfield__read_var_BitField_offset12_count8, #function
	.proc	04
bitfield__read_var_BitField_offset12_count8:
.LLFB586:
	save	%sp, -96, %sp
.LLCFI586:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE586:
	.size	bitfield__read_var_BitField_offset12_count8, .-bitfield__read_var_BitField_offset12_count8
	.align 4
	.type	bitfield__write_var_BitField_offset12_count8, #function
	.proc	020
bitfield__write_var_BitField_offset12_count8:
.LLFB587:
	save	%sp, -104, %sp
.LLCFI587:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1174
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1175
	 nop
.LL1174:
	sethi	%hi(76800), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1175:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1045504), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE587:
	.size	bitfield__write_var_BitField_offset12_count8, .-bitfield__write_var_BitField_offset12_count8
	.align 4
	.type	bitfield__read_var_BitField_offset12_count9, #function
	.proc	04
bitfield__read_var_BitField_offset12_count9:
.LLFB588:
	save	%sp, -96, %sp
.LLCFI588:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE588:
	.size	bitfield__read_var_BitField_offset12_count9, .-bitfield__read_var_BitField_offset12_count9
	.align 4
	.type	bitfield__write_var_BitField_offset12_count9, #function
	.proc	020
bitfield__write_var_BitField_offset12_count9:
.LLFB589:
	save	%sp, -104, %sp
.LLCFI589:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1178
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1179
	 nop
.LL1178:
	sethi	%hi(76800), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1179:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1047552), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE589:
	.size	bitfield__write_var_BitField_offset12_count9, .-bitfield__write_var_BitField_offset12_count9
	.align 4
	.type	bitfield__read_var_BitField_offset12_count10, #function
	.proc	04
bitfield__read_var_BitField_offset12_count10:
.LLFB590:
	save	%sp, -96, %sp
.LLCFI590:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE590:
	.size	bitfield__read_var_BitField_offset12_count10, .-bitfield__read_var_BitField_offset12_count10
	.align 4
	.type	bitfield__write_var_BitField_offset12_count10, #function
	.proc	020
bitfield__write_var_BitField_offset12_count10:
.LLFB591:
	save	%sp, -104, %sp
.LLCFI591:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1182
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1183
	 nop
.LL1182:
	sethi	%hi(76800), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1183:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE591:
	.size	bitfield__write_var_BitField_offset12_count10, .-bitfield__write_var_BitField_offset12_count10
	.align 4
	.type	bitfield__read_var_BitField_offset12_count11, #function
	.proc	04
bitfield__read_var_BitField_offset12_count11:
.LLFB592:
	save	%sp, -96, %sp
.LLCFI592:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE592:
	.size	bitfield__read_var_BitField_offset12_count11, .-bitfield__read_var_BitField_offset12_count11
	.align 4
	.type	bitfield__write_var_BitField_offset12_count11, #function
	.proc	020
bitfield__write_var_BitField_offset12_count11:
.LLFB593:
	save	%sp, -104, %sp
.LLCFI593:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1186
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1187
	 nop
.LL1186:
	sethi	%hi(76800), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1187:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE593:
	.size	bitfield__write_var_BitField_offset12_count11, .-bitfield__write_var_BitField_offset12_count11
	.align 4
	.type	bitfield__read_var_BitField_offset12_count12, #function
	.proc	04
bitfield__read_var_BitField_offset12_count12:
.LLFB594:
	save	%sp, -96, %sp
.LLCFI594:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE594:
	.size	bitfield__read_var_BitField_offset12_count12, .-bitfield__read_var_BitField_offset12_count12
	.align 4
	.type	bitfield__write_var_BitField_offset12_count12, #function
	.proc	020
bitfield__write_var_BitField_offset12_count12:
.LLFB595:
	save	%sp, -104, %sp
.LLCFI595:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1190
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1191
	 nop
.LL1190:
	sethi	%hi(77824), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1191:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE595:
	.size	bitfield__write_var_BitField_offset12_count12, .-bitfield__write_var_BitField_offset12_count12
	.align 4
	.type	bitfield__read_var_BitField_offset12_count13, #function
	.proc	04
bitfield__read_var_BitField_offset12_count13:
.LLFB596:
	save	%sp, -96, %sp
.LLCFI596:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE596:
	.size	bitfield__read_var_BitField_offset12_count13, .-bitfield__read_var_BitField_offset12_count13
	.align 4
	.type	bitfield__write_var_BitField_offset12_count13, #function
	.proc	020
bitfield__write_var_BitField_offset12_count13:
.LLFB597:
	save	%sp, -104, %sp
.LLCFI597:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1194
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1195
	 nop
.LL1194:
	sethi	%hi(77824), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1195:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE597:
	.size	bitfield__write_var_BitField_offset12_count13, .-bitfield__write_var_BitField_offset12_count13
	.align 4
	.type	bitfield__read_var_BitField_offset12_count14, #function
	.proc	04
bitfield__read_var_BitField_offset12_count14:
.LLFB598:
	save	%sp, -96, %sp
.LLCFI598:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE598:
	.size	bitfield__read_var_BitField_offset12_count14, .-bitfield__read_var_BitField_offset12_count14
	.align 4
	.type	bitfield__write_var_BitField_offset12_count14, #function
	.proc	020
bitfield__write_var_BitField_offset12_count14:
.LLFB599:
	save	%sp, -104, %sp
.LLCFI599:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1198
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1199
	 nop
.LL1198:
	sethi	%hi(77824), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1199:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE599:
	.size	bitfield__write_var_BitField_offset12_count14, .-bitfield__write_var_BitField_offset12_count14
	.align 4
	.type	bitfield__read_var_BitField_offset12_count15, #function
	.proc	04
bitfield__read_var_BitField_offset12_count15:
.LLFB600:
	save	%sp, -96, %sp
.LLCFI600:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE600:
	.size	bitfield__read_var_BitField_offset12_count15, .-bitfield__read_var_BitField_offset12_count15
	.align 4
	.type	bitfield__write_var_BitField_offset12_count15, #function
	.proc	020
bitfield__write_var_BitField_offset12_count15:
.LLFB601:
	save	%sp, -104, %sp
.LLCFI601:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1202
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1203
	 nop
.LL1202:
	sethi	%hi(77824), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1203:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE601:
	.size	bitfield__write_var_BitField_offset12_count15, .-bitfield__write_var_BitField_offset12_count15
	.align 4
	.type	bitfield__read_var_BitField_offset12_count16, #function
	.proc	04
bitfield__read_var_BitField_offset12_count16:
.LLFB602:
	save	%sp, -96, %sp
.LLCFI602:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE602:
	.size	bitfield__read_var_BitField_offset12_count16, .-bitfield__read_var_BitField_offset12_count16
	.align 4
	.type	bitfield__write_var_BitField_offset12_count16, #function
	.proc	020
bitfield__write_var_BitField_offset12_count16:
.LLFB603:
	save	%sp, -104, %sp
.LLCFI603:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1206
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1207
	 nop
.LL1206:
	sethi	%hi(78848), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1207:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE603:
	.size	bitfield__write_var_BitField_offset12_count16, .-bitfield__write_var_BitField_offset12_count16
	.align 4
	.type	bitfield__read_var_BitField_offset12_count17, #function
	.proc	04
bitfield__read_var_BitField_offset12_count17:
.LLFB604:
	save	%sp, -96, %sp
.LLCFI604:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE604:
	.size	bitfield__read_var_BitField_offset12_count17, .-bitfield__read_var_BitField_offset12_count17
	.align 4
	.type	bitfield__write_var_BitField_offset12_count17, #function
	.proc	020
bitfield__write_var_BitField_offset12_count17:
.LLFB605:
	save	%sp, -104, %sp
.LLCFI605:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1210
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1211
	 nop
.LL1210:
	sethi	%hi(78848), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1211:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE605:
	.size	bitfield__write_var_BitField_offset12_count17, .-bitfield__write_var_BitField_offset12_count17
	.align 4
	.type	bitfield__read_var_BitField_offset12_count18, #function
	.proc	04
bitfield__read_var_BitField_offset12_count18:
.LLFB606:
	save	%sp, -96, %sp
.LLCFI606:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE606:
	.size	bitfield__read_var_BitField_offset12_count18, .-bitfield__read_var_BitField_offset12_count18
	.align 4
	.type	bitfield__write_var_BitField_offset12_count18, #function
	.proc	020
bitfield__write_var_BitField_offset12_count18:
.LLFB607:
	save	%sp, -104, %sp
.LLCFI607:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1214
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1215
	 nop
.LL1214:
	sethi	%hi(78848), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1215:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE607:
	.size	bitfield__write_var_BitField_offset12_count18, .-bitfield__write_var_BitField_offset12_count18
	.align 4
	.type	bitfield__read_var_BitField_offset12_count19, #function
	.proc	04
bitfield__read_var_BitField_offset12_count19:
.LLFB608:
	save	%sp, -96, %sp
.LLCFI608:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 12, %g1
	srl	%g1, 13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE608:
	.size	bitfield__read_var_BitField_offset12_count19, .-bitfield__read_var_BitField_offset12_count19
	.align 4
	.type	bitfield__write_var_BitField_offset12_count19, #function
	.proc	020
bitfield__write_var_BitField_offset12_count19:
.LLFB609:
	save	%sp, -104, %sp
.LLCFI609:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1218
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1219
	 nop
.LL1218:
	sethi	%hi(78848), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1219:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-1048576), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE609:
	.size	bitfield__write_var_BitField_offset12_count19, .-bitfield__write_var_BitField_offset12_count19
	.align 4
	.type	bitfield__read_var_BitField_offset12_count20, #function
	.proc	04
bitfield__read_var_BitField_offset12_count20:
.LLFB610:
	save	%sp, -96, %sp
.LLCFI610:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE610:
	.size	bitfield__read_var_BitField_offset12_count20, .-bitfield__read_var_BitField_offset12_count20
	.align 4
	.type	bitfield__write_var_BitField_offset12_count20, #function
	.proc	020
bitfield__write_var_BitField_offset12_count20:
.LLFB611:
	save	%sp, -104, %sp
.LLCFI611:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1222
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1223
	 nop
.LL1222:
	sethi	%hi(79872), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1223:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-1048576), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE611:
	.size	bitfield__write_var_BitField_offset12_count20, .-bitfield__write_var_BitField_offset12_count20
	.align 4
	.type	bitfield__read_var_BitField_offset13_count1, #function
	.proc	04
bitfield__read_var_BitField_offset13_count1:
.LLFB612:
	save	%sp, -96, %sp
.LLCFI612:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE612:
	.size	bitfield__read_var_BitField_offset13_count1, .-bitfield__read_var_BitField_offset13_count1
	.align 4
	.type	bitfield__write_var_BitField_offset13_count1, #function
	.proc	020
bitfield__write_var_BitField_offset13_count1:
.LLFB613:
	save	%sp, -104, %sp
.LLCFI613:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1226
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1227
	 nop
.LL1226:
	sethi	%hi(79872), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1227:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE613:
	.size	bitfield__write_var_BitField_offset13_count1, .-bitfield__write_var_BitField_offset13_count1
	.align 4
	.type	bitfield__read_var_BitField_offset13_count2, #function
	.proc	04
bitfield__read_var_BitField_offset13_count2:
.LLFB614:
	save	%sp, -96, %sp
.LLCFI614:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE614:
	.size	bitfield__read_var_BitField_offset13_count2, .-bitfield__read_var_BitField_offset13_count2
	.align 4
	.type	bitfield__write_var_BitField_offset13_count2, #function
	.proc	020
bitfield__write_var_BitField_offset13_count2:
.LLFB615:
	save	%sp, -104, %sp
.LLCFI615:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1230
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1231
	 nop
.LL1230:
	sethi	%hi(79872), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1231:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, -7, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE615:
	.size	bitfield__write_var_BitField_offset13_count2, .-bitfield__write_var_BitField_offset13_count2
	.align 4
	.type	bitfield__read_var_BitField_offset13_count3, #function
	.proc	04
bitfield__read_var_BitField_offset13_count3:
.LLFB616:
	save	%sp, -96, %sp
.LLCFI616:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE616:
	.size	bitfield__read_var_BitField_offset13_count3, .-bitfield__read_var_BitField_offset13_count3
	.align 4
	.type	bitfield__write_var_BitField_offset13_count3, #function
	.proc	020
bitfield__write_var_BitField_offset13_count3:
.LLFB617:
	save	%sp, -104, %sp
.LLCFI617:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1234
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1235
	 nop
.LL1234:
	sethi	%hi(79872), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1235:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -8, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE617:
	.size	bitfield__write_var_BitField_offset13_count3, .-bitfield__write_var_BitField_offset13_count3
	.align 4
	.type	bitfield__read_var_BitField_offset13_count4, #function
	.proc	04
bitfield__read_var_BitField_offset13_count4:
.LLFB618:
	save	%sp, -96, %sp
.LLCFI618:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE618:
	.size	bitfield__read_var_BitField_offset13_count4, .-bitfield__read_var_BitField_offset13_count4
	.align 4
	.type	bitfield__write_var_BitField_offset13_count4, #function
	.proc	020
bitfield__write_var_BitField_offset13_count4:
.LLFB619:
	save	%sp, -104, %sp
.LLCFI619:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1238
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1239
	 nop
.LL1238:
	sethi	%hi(80896), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1239:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-492544), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE619:
	.size	bitfield__write_var_BitField_offset13_count4, .-bitfield__write_var_BitField_offset13_count4
	.align 4
	.type	bitfield__read_var_BitField_offset13_count5, #function
	.proc	04
bitfield__read_var_BitField_offset13_count5:
.LLFB620:
	save	%sp, -96, %sp
.LLCFI620:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE620:
	.size	bitfield__read_var_BitField_offset13_count5, .-bitfield__read_var_BitField_offset13_count5
	.align 4
	.type	bitfield__write_var_BitField_offset13_count5, #function
	.proc	020
bitfield__write_var_BitField_offset13_count5:
.LLFB621:
	save	%sp, -104, %sp
.LLCFI621:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1242
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1243
	 nop
.LL1242:
	sethi	%hi(80896), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1243:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-508928), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE621:
	.size	bitfield__write_var_BitField_offset13_count5, .-bitfield__write_var_BitField_offset13_count5
	.align 4
	.type	bitfield__read_var_BitField_offset13_count6, #function
	.proc	04
bitfield__read_var_BitField_offset13_count6:
.LLFB622:
	save	%sp, -96, %sp
.LLCFI622:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE622:
	.size	bitfield__read_var_BitField_offset13_count6, .-bitfield__read_var_BitField_offset13_count6
	.align 4
	.type	bitfield__write_var_BitField_offset13_count6, #function
	.proc	020
bitfield__write_var_BitField_offset13_count6:
.LLFB623:
	save	%sp, -104, %sp
.LLCFI623:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1246
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1247
	 nop
.LL1246:
	sethi	%hi(80896), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1247:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-517120), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE623:
	.size	bitfield__write_var_BitField_offset13_count6, .-bitfield__write_var_BitField_offset13_count6
	.align 4
	.type	bitfield__read_var_BitField_offset13_count7, #function
	.proc	04
bitfield__read_var_BitField_offset13_count7:
.LLFB624:
	save	%sp, -96, %sp
.LLCFI624:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE624:
	.size	bitfield__read_var_BitField_offset13_count7, .-bitfield__read_var_BitField_offset13_count7
	.align 4
	.type	bitfield__write_var_BitField_offset13_count7, #function
	.proc	020
bitfield__write_var_BitField_offset13_count7:
.LLFB625:
	save	%sp, -104, %sp
.LLCFI625:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1250
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1251
	 nop
.LL1250:
	sethi	%hi(80896), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1251:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-521216), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE625:
	.size	bitfield__write_var_BitField_offset13_count7, .-bitfield__write_var_BitField_offset13_count7
	.align 4
	.type	bitfield__read_var_BitField_offset13_count8, #function
	.proc	04
bitfield__read_var_BitField_offset13_count8:
.LLFB626:
	save	%sp, -96, %sp
.LLCFI626:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE626:
	.size	bitfield__read_var_BitField_offset13_count8, .-bitfield__read_var_BitField_offset13_count8
	.align 4
	.type	bitfield__write_var_BitField_offset13_count8, #function
	.proc	020
bitfield__write_var_BitField_offset13_count8:
.LLFB627:
	save	%sp, -104, %sp
.LLCFI627:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1254
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1255
	 nop
.LL1254:
	sethi	%hi(81920), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1255:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-523264), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE627:
	.size	bitfield__write_var_BitField_offset13_count8, .-bitfield__write_var_BitField_offset13_count8
	.align 4
	.type	bitfield__read_var_BitField_offset13_count9, #function
	.proc	04
bitfield__read_var_BitField_offset13_count9:
.LLFB628:
	save	%sp, -96, %sp
.LLCFI628:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE628:
	.size	bitfield__read_var_BitField_offset13_count9, .-bitfield__read_var_BitField_offset13_count9
	.align 4
	.type	bitfield__write_var_BitField_offset13_count9, #function
	.proc	020
bitfield__write_var_BitField_offset13_count9:
.LLFB629:
	save	%sp, -104, %sp
.LLCFI629:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1258
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1259
	 nop
.LL1258:
	sethi	%hi(81920), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1259:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE629:
	.size	bitfield__write_var_BitField_offset13_count9, .-bitfield__write_var_BitField_offset13_count9
	.align 4
	.type	bitfield__read_var_BitField_offset13_count10, #function
	.proc	04
bitfield__read_var_BitField_offset13_count10:
.LLFB630:
	save	%sp, -96, %sp
.LLCFI630:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE630:
	.size	bitfield__read_var_BitField_offset13_count10, .-bitfield__read_var_BitField_offset13_count10
	.align 4
	.type	bitfield__write_var_BitField_offset13_count10, #function
	.proc	020
bitfield__write_var_BitField_offset13_count10:
.LLFB631:
	save	%sp, -104, %sp
.LLCFI631:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1262
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1263
	 nop
.LL1262:
	sethi	%hi(81920), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1263:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE631:
	.size	bitfield__write_var_BitField_offset13_count10, .-bitfield__write_var_BitField_offset13_count10
	.align 4
	.type	bitfield__read_var_BitField_offset13_count11, #function
	.proc	04
bitfield__read_var_BitField_offset13_count11:
.LLFB632:
	save	%sp, -96, %sp
.LLCFI632:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE632:
	.size	bitfield__read_var_BitField_offset13_count11, .-bitfield__read_var_BitField_offset13_count11
	.align 4
	.type	bitfield__write_var_BitField_offset13_count11, #function
	.proc	020
bitfield__write_var_BitField_offset13_count11:
.LLFB633:
	save	%sp, -104, %sp
.LLCFI633:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1266
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1267
	 nop
.LL1266:
	sethi	%hi(81920), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1267:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE633:
	.size	bitfield__write_var_BitField_offset13_count11, .-bitfield__write_var_BitField_offset13_count11
	.align 4
	.type	bitfield__read_var_BitField_offset13_count12, #function
	.proc	04
bitfield__read_var_BitField_offset13_count12:
.LLFB634:
	save	%sp, -96, %sp
.LLCFI634:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE634:
	.size	bitfield__read_var_BitField_offset13_count12, .-bitfield__read_var_BitField_offset13_count12
	.align 4
	.type	bitfield__write_var_BitField_offset13_count12, #function
	.proc	020
bitfield__write_var_BitField_offset13_count12:
.LLFB635:
	save	%sp, -104, %sp
.LLCFI635:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1270
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1271
	 nop
.LL1270:
	sethi	%hi(82944), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1271:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE635:
	.size	bitfield__write_var_BitField_offset13_count12, .-bitfield__write_var_BitField_offset13_count12
	.align 4
	.type	bitfield__read_var_BitField_offset13_count13, #function
	.proc	04
bitfield__read_var_BitField_offset13_count13:
.LLFB636:
	save	%sp, -96, %sp
.LLCFI636:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE636:
	.size	bitfield__read_var_BitField_offset13_count13, .-bitfield__read_var_BitField_offset13_count13
	.align 4
	.type	bitfield__write_var_BitField_offset13_count13, #function
	.proc	020
bitfield__write_var_BitField_offset13_count13:
.LLFB637:
	save	%sp, -104, %sp
.LLCFI637:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1274
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1275
	 nop
.LL1274:
	sethi	%hi(82944), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1275:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE637:
	.size	bitfield__write_var_BitField_offset13_count13, .-bitfield__write_var_BitField_offset13_count13
	.align 4
	.type	bitfield__read_var_BitField_offset13_count14, #function
	.proc	04
bitfield__read_var_BitField_offset13_count14:
.LLFB638:
	save	%sp, -96, %sp
.LLCFI638:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE638:
	.size	bitfield__read_var_BitField_offset13_count14, .-bitfield__read_var_BitField_offset13_count14
	.align 4
	.type	bitfield__write_var_BitField_offset13_count14, #function
	.proc	020
bitfield__write_var_BitField_offset13_count14:
.LLFB639:
	save	%sp, -104, %sp
.LLCFI639:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1278
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1279
	 nop
.LL1278:
	sethi	%hi(82944), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1279:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE639:
	.size	bitfield__write_var_BitField_offset13_count14, .-bitfield__write_var_BitField_offset13_count14
	.align 4
	.type	bitfield__read_var_BitField_offset13_count15, #function
	.proc	04
bitfield__read_var_BitField_offset13_count15:
.LLFB640:
	save	%sp, -96, %sp
.LLCFI640:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE640:
	.size	bitfield__read_var_BitField_offset13_count15, .-bitfield__read_var_BitField_offset13_count15
	.align 4
	.type	bitfield__write_var_BitField_offset13_count15, #function
	.proc	020
bitfield__write_var_BitField_offset13_count15:
.LLFB641:
	save	%sp, -104, %sp
.LLCFI641:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1282
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1283
	 nop
.LL1282:
	sethi	%hi(82944), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1283:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE641:
	.size	bitfield__write_var_BitField_offset13_count15, .-bitfield__write_var_BitField_offset13_count15
	.align 4
	.type	bitfield__read_var_BitField_offset13_count16, #function
	.proc	04
bitfield__read_var_BitField_offset13_count16:
.LLFB642:
	save	%sp, -96, %sp
.LLCFI642:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE642:
	.size	bitfield__read_var_BitField_offset13_count16, .-bitfield__read_var_BitField_offset13_count16
	.align 4
	.type	bitfield__write_var_BitField_offset13_count16, #function
	.proc	020
bitfield__write_var_BitField_offset13_count16:
.LLFB643:
	save	%sp, -104, %sp
.LLCFI643:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1286
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1287
	 nop
.LL1286:
	sethi	%hi(83968), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1287:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE643:
	.size	bitfield__write_var_BitField_offset13_count16, .-bitfield__write_var_BitField_offset13_count16
	.align 4
	.type	bitfield__read_var_BitField_offset13_count17, #function
	.proc	04
bitfield__read_var_BitField_offset13_count17:
.LLFB644:
	save	%sp, -96, %sp
.LLCFI644:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE644:
	.size	bitfield__read_var_BitField_offset13_count17, .-bitfield__read_var_BitField_offset13_count17
	.align 4
	.type	bitfield__write_var_BitField_offset13_count17, #function
	.proc	020
bitfield__write_var_BitField_offset13_count17:
.LLFB645:
	save	%sp, -104, %sp
.LLCFI645:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1290
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1291
	 nop
.LL1290:
	sethi	%hi(83968), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1291:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE645:
	.size	bitfield__write_var_BitField_offset13_count17, .-bitfield__write_var_BitField_offset13_count17
	.align 4
	.type	bitfield__read_var_BitField_offset13_count18, #function
	.proc	04
bitfield__read_var_BitField_offset13_count18:
.LLFB646:
	save	%sp, -96, %sp
.LLCFI646:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 13, %g1
	srl	%g1, 14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE646:
	.size	bitfield__read_var_BitField_offset13_count18, .-bitfield__read_var_BitField_offset13_count18
	.align 4
	.type	bitfield__write_var_BitField_offset13_count18, #function
	.proc	020
bitfield__write_var_BitField_offset13_count18:
.LLFB647:
	save	%sp, -104, %sp
.LLCFI647:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1294
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1295
	 nop
.LL1294:
	sethi	%hi(83968), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1295:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-524288), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE647:
	.size	bitfield__write_var_BitField_offset13_count18, .-bitfield__write_var_BitField_offset13_count18
	.align 4
	.type	bitfield__read_var_BitField_offset13_count19, #function
	.proc	04
bitfield__read_var_BitField_offset13_count19:
.LLFB648:
	save	%sp, -96, %sp
.LLCFI648:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE648:
	.size	bitfield__read_var_BitField_offset13_count19, .-bitfield__read_var_BitField_offset13_count19
	.align 4
	.type	bitfield__write_var_BitField_offset13_count19, #function
	.proc	020
bitfield__write_var_BitField_offset13_count19:
.LLFB649:
	save	%sp, -104, %sp
.LLCFI649:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1298
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1299
	 nop
.LL1298:
	sethi	%hi(83968), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1299:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-524288), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE649:
	.size	bitfield__write_var_BitField_offset13_count19, .-bitfield__write_var_BitField_offset13_count19
	.align 4
	.type	bitfield__read_var_BitField_offset14_count1, #function
	.proc	04
bitfield__read_var_BitField_offset14_count1:
.LLFB650:
	save	%sp, -96, %sp
.LLCFI650:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE650:
	.size	bitfield__read_var_BitField_offset14_count1, .-bitfield__read_var_BitField_offset14_count1
	.align 4
	.type	bitfield__write_var_BitField_offset14_count1, #function
	.proc	020
bitfield__write_var_BitField_offset14_count1:
.LLFB651:
	save	%sp, -104, %sp
.LLCFI651:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1302
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1303
	 nop
.LL1302:
	sethi	%hi(84992), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1303:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %o5
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ldub	[%g1], %g1
	and	%g1, -3, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE651:
	.size	bitfield__write_var_BitField_offset14_count1, .-bitfield__write_var_BitField_offset14_count1
	.align 4
	.type	bitfield__read_var_BitField_offset14_count2, #function
	.proc	04
bitfield__read_var_BitField_offset14_count2:
.LLFB652:
	save	%sp, -96, %sp
.LLCFI652:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE652:
	.size	bitfield__read_var_BitField_offset14_count2, .-bitfield__read_var_BitField_offset14_count2
	.align 4
	.type	bitfield__write_var_BitField_offset14_count2, #function
	.proc	020
bitfield__write_var_BitField_offset14_count2:
.LLFB653:
	save	%sp, -104, %sp
.LLCFI653:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1306
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1307
	 nop
.LL1306:
	sethi	%hi(84992), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1307:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -4, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE653:
	.size	bitfield__write_var_BitField_offset14_count2, .-bitfield__write_var_BitField_offset14_count2
	.align 4
	.type	bitfield__read_var_BitField_offset14_count3, #function
	.proc	04
bitfield__read_var_BitField_offset14_count3:
.LLFB654:
	save	%sp, -96, %sp
.LLCFI654:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE654:
	.size	bitfield__read_var_BitField_offset14_count3, .-bitfield__read_var_BitField_offset14_count3
	.align 4
	.type	bitfield__write_var_BitField_offset14_count3, #function
	.proc	020
bitfield__write_var_BitField_offset14_count3:
.LLFB655:
	save	%sp, -104, %sp
.LLCFI655:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1310
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1311
	 nop
.LL1310:
	sethi	%hi(84992), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1311:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-230400), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE655:
	.size	bitfield__write_var_BitField_offset14_count3, .-bitfield__write_var_BitField_offset14_count3
	.align 4
	.type	bitfield__read_var_BitField_offset14_count4, #function
	.proc	04
bitfield__read_var_BitField_offset14_count4:
.LLFB656:
	save	%sp, -96, %sp
.LLCFI656:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE656:
	.size	bitfield__read_var_BitField_offset14_count4, .-bitfield__read_var_BitField_offset14_count4
	.align 4
	.type	bitfield__write_var_BitField_offset14_count4, #function
	.proc	020
bitfield__write_var_BitField_offset14_count4:
.LLFB657:
	save	%sp, -104, %sp
.LLCFI657:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1314
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1315
	 nop
.LL1314:
	sethi	%hi(84992), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1315:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-246784), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE657:
	.size	bitfield__write_var_BitField_offset14_count4, .-bitfield__write_var_BitField_offset14_count4
	.align 4
	.type	bitfield__read_var_BitField_offset14_count5, #function
	.proc	04
bitfield__read_var_BitField_offset14_count5:
.LLFB658:
	save	%sp, -96, %sp
.LLCFI658:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE658:
	.size	bitfield__read_var_BitField_offset14_count5, .-bitfield__read_var_BitField_offset14_count5
	.align 4
	.type	bitfield__write_var_BitField_offset14_count5, #function
	.proc	020
bitfield__write_var_BitField_offset14_count5:
.LLFB659:
	save	%sp, -104, %sp
.LLCFI659:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1318
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1319
	 nop
.LL1318:
	sethi	%hi(86016), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1319:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-254976), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE659:
	.size	bitfield__write_var_BitField_offset14_count5, .-bitfield__write_var_BitField_offset14_count5
	.align 4
	.type	bitfield__read_var_BitField_offset14_count6, #function
	.proc	04
bitfield__read_var_BitField_offset14_count6:
.LLFB660:
	save	%sp, -96, %sp
.LLCFI660:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE660:
	.size	bitfield__read_var_BitField_offset14_count6, .-bitfield__read_var_BitField_offset14_count6
	.align 4
	.type	bitfield__write_var_BitField_offset14_count6, #function
	.proc	020
bitfield__write_var_BitField_offset14_count6:
.LLFB661:
	save	%sp, -104, %sp
.LLCFI661:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1322
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1323
	 nop
.LL1322:
	sethi	%hi(86016), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1323:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-259072), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE661:
	.size	bitfield__write_var_BitField_offset14_count6, .-bitfield__write_var_BitField_offset14_count6
	.align 4
	.type	bitfield__read_var_BitField_offset14_count7, #function
	.proc	04
bitfield__read_var_BitField_offset14_count7:
.LLFB662:
	save	%sp, -96, %sp
.LLCFI662:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE662:
	.size	bitfield__read_var_BitField_offset14_count7, .-bitfield__read_var_BitField_offset14_count7
	.align 4
	.type	bitfield__write_var_BitField_offset14_count7, #function
	.proc	020
bitfield__write_var_BitField_offset14_count7:
.LLFB663:
	save	%sp, -104, %sp
.LLCFI663:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1326
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1327
	 nop
.LL1326:
	sethi	%hi(86016), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1327:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-261120), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE663:
	.size	bitfield__write_var_BitField_offset14_count7, .-bitfield__write_var_BitField_offset14_count7
	.align 4
	.type	bitfield__read_var_BitField_offset14_count8, #function
	.proc	04
bitfield__read_var_BitField_offset14_count8:
.LLFB664:
	save	%sp, -96, %sp
.LLCFI664:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE664:
	.size	bitfield__read_var_BitField_offset14_count8, .-bitfield__read_var_BitField_offset14_count8
	.align 4
	.type	bitfield__write_var_BitField_offset14_count8, #function
	.proc	020
bitfield__write_var_BitField_offset14_count8:
.LLFB665:
	save	%sp, -104, %sp
.LLCFI665:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1330
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1331
	 nop
.LL1330:
	sethi	%hi(86016), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1331:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE665:
	.size	bitfield__write_var_BitField_offset14_count8, .-bitfield__write_var_BitField_offset14_count8
	.align 4
	.type	bitfield__read_var_BitField_offset14_count9, #function
	.proc	04
bitfield__read_var_BitField_offset14_count9:
.LLFB666:
	save	%sp, -96, %sp
.LLCFI666:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE666:
	.size	bitfield__read_var_BitField_offset14_count9, .-bitfield__read_var_BitField_offset14_count9
	.align 4
	.type	bitfield__write_var_BitField_offset14_count9, #function
	.proc	020
bitfield__write_var_BitField_offset14_count9:
.LLFB667:
	save	%sp, -104, %sp
.LLCFI667:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1334
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1335
	 nop
.LL1334:
	sethi	%hi(87040), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1335:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE667:
	.size	bitfield__write_var_BitField_offset14_count9, .-bitfield__write_var_BitField_offset14_count9
	.align 4
	.type	bitfield__read_var_BitField_offset14_count10, #function
	.proc	04
bitfield__read_var_BitField_offset14_count10:
.LLFB668:
	save	%sp, -96, %sp
.LLCFI668:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE668:
	.size	bitfield__read_var_BitField_offset14_count10, .-bitfield__read_var_BitField_offset14_count10
	.align 4
	.type	bitfield__write_var_BitField_offset14_count10, #function
	.proc	020
bitfield__write_var_BitField_offset14_count10:
.LLFB669:
	save	%sp, -104, %sp
.LLCFI669:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1338
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1339
	 nop
.LL1338:
	sethi	%hi(87040), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1339:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE669:
	.size	bitfield__write_var_BitField_offset14_count10, .-bitfield__write_var_BitField_offset14_count10
	.align 4
	.type	bitfield__read_var_BitField_offset14_count11, #function
	.proc	04
bitfield__read_var_BitField_offset14_count11:
.LLFB670:
	save	%sp, -96, %sp
.LLCFI670:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE670:
	.size	bitfield__read_var_BitField_offset14_count11, .-bitfield__read_var_BitField_offset14_count11
	.align 4
	.type	bitfield__write_var_BitField_offset14_count11, #function
	.proc	020
bitfield__write_var_BitField_offset14_count11:
.LLFB671:
	save	%sp, -104, %sp
.LLCFI671:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1342
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1343
	 nop
.LL1342:
	sethi	%hi(87040), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1343:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE671:
	.size	bitfield__write_var_BitField_offset14_count11, .-bitfield__write_var_BitField_offset14_count11
	.align 4
	.type	bitfield__read_var_BitField_offset14_count12, #function
	.proc	04
bitfield__read_var_BitField_offset14_count12:
.LLFB672:
	save	%sp, -96, %sp
.LLCFI672:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE672:
	.size	bitfield__read_var_BitField_offset14_count12, .-bitfield__read_var_BitField_offset14_count12
	.align 4
	.type	bitfield__write_var_BitField_offset14_count12, #function
	.proc	020
bitfield__write_var_BitField_offset14_count12:
.LLFB673:
	save	%sp, -104, %sp
.LLCFI673:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1346
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1347
	 nop
.LL1346:
	sethi	%hi(87040), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1347:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE673:
	.size	bitfield__write_var_BitField_offset14_count12, .-bitfield__write_var_BitField_offset14_count12
	.align 4
	.type	bitfield__read_var_BitField_offset14_count13, #function
	.proc	04
bitfield__read_var_BitField_offset14_count13:
.LLFB674:
	save	%sp, -96, %sp
.LLCFI674:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE674:
	.size	bitfield__read_var_BitField_offset14_count13, .-bitfield__read_var_BitField_offset14_count13
	.align 4
	.type	bitfield__write_var_BitField_offset14_count13, #function
	.proc	020
bitfield__write_var_BitField_offset14_count13:
.LLFB675:
	save	%sp, -104, %sp
.LLCFI675:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1350
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1351
	 nop
.LL1350:
	sethi	%hi(88064), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1351:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE675:
	.size	bitfield__write_var_BitField_offset14_count13, .-bitfield__write_var_BitField_offset14_count13
	.align 4
	.type	bitfield__read_var_BitField_offset14_count14, #function
	.proc	04
bitfield__read_var_BitField_offset14_count14:
.LLFB676:
	save	%sp, -96, %sp
.LLCFI676:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE676:
	.size	bitfield__read_var_BitField_offset14_count14, .-bitfield__read_var_BitField_offset14_count14
	.align 4
	.type	bitfield__write_var_BitField_offset14_count14, #function
	.proc	020
bitfield__write_var_BitField_offset14_count14:
.LLFB677:
	save	%sp, -104, %sp
.LLCFI677:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1354
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1355
	 nop
.LL1354:
	sethi	%hi(88064), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1355:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE677:
	.size	bitfield__write_var_BitField_offset14_count14, .-bitfield__write_var_BitField_offset14_count14
	.align 4
	.type	bitfield__read_var_BitField_offset14_count15, #function
	.proc	04
bitfield__read_var_BitField_offset14_count15:
.LLFB678:
	save	%sp, -96, %sp
.LLCFI678:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE678:
	.size	bitfield__read_var_BitField_offset14_count15, .-bitfield__read_var_BitField_offset14_count15
	.align 4
	.type	bitfield__write_var_BitField_offset14_count15, #function
	.proc	020
bitfield__write_var_BitField_offset14_count15:
.LLFB679:
	save	%sp, -104, %sp
.LLCFI679:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1358
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1359
	 nop
.LL1358:
	sethi	%hi(88064), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1359:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE679:
	.size	bitfield__write_var_BitField_offset14_count15, .-bitfield__write_var_BitField_offset14_count15
	.align 4
	.type	bitfield__read_var_BitField_offset14_count16, #function
	.proc	04
bitfield__read_var_BitField_offset14_count16:
.LLFB680:
	save	%sp, -96, %sp
.LLCFI680:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE680:
	.size	bitfield__read_var_BitField_offset14_count16, .-bitfield__read_var_BitField_offset14_count16
	.align 4
	.type	bitfield__write_var_BitField_offset14_count16, #function
	.proc	020
bitfield__write_var_BitField_offset14_count16:
.LLFB681:
	save	%sp, -104, %sp
.LLCFI681:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1362
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1363
	 nop
.LL1362:
	sethi	%hi(88064), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1363:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE681:
	.size	bitfield__write_var_BitField_offset14_count16, .-bitfield__write_var_BitField_offset14_count16
	.align 4
	.type	bitfield__read_var_BitField_offset14_count17, #function
	.proc	04
bitfield__read_var_BitField_offset14_count17:
.LLFB682:
	save	%sp, -96, %sp
.LLCFI682:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 14, %g1
	srl	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE682:
	.size	bitfield__read_var_BitField_offset14_count17, .-bitfield__read_var_BitField_offset14_count17
	.align 4
	.type	bitfield__write_var_BitField_offset14_count17, #function
	.proc	020
bitfield__write_var_BitField_offset14_count17:
.LLFB683:
	save	%sp, -104, %sp
.LLCFI683:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1366
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1367
	 nop
.LL1366:
	sethi	%hi(89088), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1367:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-262144), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE683:
	.size	bitfield__write_var_BitField_offset14_count17, .-bitfield__write_var_BitField_offset14_count17
	.align 4
	.type	bitfield__read_var_BitField_offset14_count18, #function
	.proc	04
bitfield__read_var_BitField_offset14_count18:
.LLFB684:
	save	%sp, -96, %sp
.LLCFI684:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE684:
	.size	bitfield__read_var_BitField_offset14_count18, .-bitfield__read_var_BitField_offset14_count18
	.align 4
	.type	bitfield__write_var_BitField_offset14_count18, #function
	.proc	020
bitfield__write_var_BitField_offset14_count18:
.LLFB685:
	save	%sp, -104, %sp
.LLCFI685:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1370
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1371
	 nop
.LL1370:
	sethi	%hi(89088), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1371:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-262144), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE685:
	.size	bitfield__write_var_BitField_offset14_count18, .-bitfield__write_var_BitField_offset14_count18
	.align 4
	.type	bitfield__read_var_BitField_offset15_count1, #function
	.proc	04
bitfield__read_var_BitField_offset15_count1:
.LLFB686:
	save	%sp, -96, %sp
.LLCFI686:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE686:
	.size	bitfield__read_var_BitField_offset15_count1, .-bitfield__read_var_BitField_offset15_count1
	.align 4
	.type	bitfield__write_var_BitField_offset15_count1, #function
	.proc	020
bitfield__write_var_BitField_offset15_count1:
.LLFB687:
	save	%sp, -104, %sp
.LLCFI687:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1374
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1375
	 nop
.LL1374:
	sethi	%hi(89088), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1375:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 1, %g1
	ld	[%fp-4], %o5
	add	%o5, 1, %o5
	ldub	[%o5], %o5
	and	%o5, -2, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE687:
	.size	bitfield__write_var_BitField_offset15_count1, .-bitfield__write_var_BitField_offset15_count1
	.align 4
	.type	bitfield__read_var_BitField_offset15_count2, #function
	.proc	04
bitfield__read_var_BitField_offset15_count2:
.LLFB688:
	save	%sp, -96, %sp
.LLCFI688:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE688:
	.size	bitfield__read_var_BitField_offset15_count2, .-bitfield__read_var_BitField_offset15_count2
	.align 4
	.type	bitfield__write_var_BitField_offset15_count2, #function
	.proc	020
bitfield__write_var_BitField_offset15_count2:
.LLFB689:
	save	%sp, -104, %sp
.LLCFI689:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1378
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1379
	 nop
.LL1378:
	sethi	%hi(89088), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1379:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-99328), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 15, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE689:
	.size	bitfield__write_var_BitField_offset15_count2, .-bitfield__write_var_BitField_offset15_count2
	.align 4
	.type	bitfield__read_var_BitField_offset15_count3, #function
	.proc	04
bitfield__read_var_BitField_offset15_count3:
.LLFB690:
	save	%sp, -96, %sp
.LLCFI690:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE690:
	.size	bitfield__read_var_BitField_offset15_count3, .-bitfield__read_var_BitField_offset15_count3
	.align 4
	.type	bitfield__write_var_BitField_offset15_count3, #function
	.proc	020
bitfield__write_var_BitField_offset15_count3:
.LLFB691:
	save	%sp, -104, %sp
.LLCFI691:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1382
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1383
	 nop
.LL1382:
	sethi	%hi(90112), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1383:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-115712), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 14, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE691:
	.size	bitfield__write_var_BitField_offset15_count3, .-bitfield__write_var_BitField_offset15_count3
	.align 4
	.type	bitfield__read_var_BitField_offset15_count4, #function
	.proc	04
bitfield__read_var_BitField_offset15_count4:
.LLFB692:
	save	%sp, -96, %sp
.LLCFI692:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE692:
	.size	bitfield__read_var_BitField_offset15_count4, .-bitfield__read_var_BitField_offset15_count4
	.align 4
	.type	bitfield__write_var_BitField_offset15_count4, #function
	.proc	020
bitfield__write_var_BitField_offset15_count4:
.LLFB693:
	save	%sp, -104, %sp
.LLCFI693:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1386
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1387
	 nop
.LL1386:
	sethi	%hi(90112), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1387:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-123904), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 13, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE693:
	.size	bitfield__write_var_BitField_offset15_count4, .-bitfield__write_var_BitField_offset15_count4
	.align 4
	.type	bitfield__read_var_BitField_offset15_count5, #function
	.proc	04
bitfield__read_var_BitField_offset15_count5:
.LLFB694:
	save	%sp, -96, %sp
.LLCFI694:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE694:
	.size	bitfield__read_var_BitField_offset15_count5, .-bitfield__read_var_BitField_offset15_count5
	.align 4
	.type	bitfield__write_var_BitField_offset15_count5, #function
	.proc	020
bitfield__write_var_BitField_offset15_count5:
.LLFB695:
	save	%sp, -104, %sp
.LLCFI695:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1390
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1391
	 nop
.LL1390:
	sethi	%hi(90112), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1391:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-128000), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 12, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE695:
	.size	bitfield__write_var_BitField_offset15_count5, .-bitfield__write_var_BitField_offset15_count5
	.align 4
	.type	bitfield__read_var_BitField_offset15_count6, #function
	.proc	04
bitfield__read_var_BitField_offset15_count6:
.LLFB696:
	save	%sp, -96, %sp
.LLCFI696:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE696:
	.size	bitfield__read_var_BitField_offset15_count6, .-bitfield__read_var_BitField_offset15_count6
	.align 4
	.type	bitfield__write_var_BitField_offset15_count6, #function
	.proc	020
bitfield__write_var_BitField_offset15_count6:
.LLFB697:
	save	%sp, -104, %sp
.LLCFI697:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1394
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1395
	 nop
.LL1394:
	sethi	%hi(90112), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1395:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-130048), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 11, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE697:
	.size	bitfield__write_var_BitField_offset15_count6, .-bitfield__write_var_BitField_offset15_count6
	.align 4
	.type	bitfield__read_var_BitField_offset15_count7, #function
	.proc	04
bitfield__read_var_BitField_offset15_count7:
.LLFB698:
	save	%sp, -96, %sp
.LLCFI698:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE698:
	.size	bitfield__read_var_BitField_offset15_count7, .-bitfield__read_var_BitField_offset15_count7
	.align 4
	.type	bitfield__write_var_BitField_offset15_count7, #function
	.proc	020
bitfield__write_var_BitField_offset15_count7:
.LLFB699:
	save	%sp, -104, %sp
.LLCFI699:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1398
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1399
	 nop
.LL1398:
	sethi	%hi(91136), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1399:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 1023, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 10, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE699:
	.size	bitfield__write_var_BitField_offset15_count7, .-bitfield__write_var_BitField_offset15_count7
	.align 4
	.type	bitfield__read_var_BitField_offset15_count8, #function
	.proc	04
bitfield__read_var_BitField_offset15_count8:
.LLFB700:
	save	%sp, -96, %sp
.LLCFI700:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE700:
	.size	bitfield__read_var_BitField_offset15_count8, .-bitfield__read_var_BitField_offset15_count8
	.align 4
	.type	bitfield__write_var_BitField_offset15_count8, #function
	.proc	020
bitfield__write_var_BitField_offset15_count8:
.LLFB701:
	save	%sp, -104, %sp
.LLCFI701:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1402
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1403
	 nop
.LL1402:
	sethi	%hi(91136), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1403:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 511, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 9, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE701:
	.size	bitfield__write_var_BitField_offset15_count8, .-bitfield__write_var_BitField_offset15_count8
	.align 4
	.type	bitfield__read_var_BitField_offset15_count9, #function
	.proc	04
bitfield__read_var_BitField_offset15_count9:
.LLFB702:
	save	%sp, -96, %sp
.LLCFI702:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE702:
	.size	bitfield__read_var_BitField_offset15_count9, .-bitfield__read_var_BitField_offset15_count9
	.align 4
	.type	bitfield__write_var_BitField_offset15_count9, #function
	.proc	020
bitfield__write_var_BitField_offset15_count9:
.LLFB703:
	save	%sp, -104, %sp
.LLCFI703:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1406
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1407
	 nop
.LL1406:
	sethi	%hi(91136), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1407:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 255, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 8, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE703:
	.size	bitfield__write_var_BitField_offset15_count9, .-bitfield__write_var_BitField_offset15_count9
	.align 4
	.type	bitfield__read_var_BitField_offset15_count10, #function
	.proc	04
bitfield__read_var_BitField_offset15_count10:
.LLFB704:
	save	%sp, -96, %sp
.LLCFI704:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE704:
	.size	bitfield__read_var_BitField_offset15_count10, .-bitfield__read_var_BitField_offset15_count10
	.align 4
	.type	bitfield__write_var_BitField_offset15_count10, #function
	.proc	020
bitfield__write_var_BitField_offset15_count10:
.LLFB705:
	save	%sp, -104, %sp
.LLCFI705:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1410
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1411
	 nop
.LL1410:
	sethi	%hi(91136), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1411:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE705:
	.size	bitfield__write_var_BitField_offset15_count10, .-bitfield__write_var_BitField_offset15_count10
	.align 4
	.type	bitfield__read_var_BitField_offset15_count11, #function
	.proc	04
bitfield__read_var_BitField_offset15_count11:
.LLFB706:
	save	%sp, -96, %sp
.LLCFI706:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE706:
	.size	bitfield__read_var_BitField_offset15_count11, .-bitfield__read_var_BitField_offset15_count11
	.align 4
	.type	bitfield__write_var_BitField_offset15_count11, #function
	.proc	020
bitfield__write_var_BitField_offset15_count11:
.LLFB707:
	save	%sp, -104, %sp
.LLCFI707:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1414
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1415
	 nop
.LL1414:
	sethi	%hi(92160), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1415:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE707:
	.size	bitfield__write_var_BitField_offset15_count11, .-bitfield__write_var_BitField_offset15_count11
	.align 4
	.type	bitfield__read_var_BitField_offset15_count12, #function
	.proc	04
bitfield__read_var_BitField_offset15_count12:
.LLFB708:
	save	%sp, -96, %sp
.LLCFI708:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE708:
	.size	bitfield__read_var_BitField_offset15_count12, .-bitfield__read_var_BitField_offset15_count12
	.align 4
	.type	bitfield__write_var_BitField_offset15_count12, #function
	.proc	020
bitfield__write_var_BitField_offset15_count12:
.LLFB709:
	save	%sp, -104, %sp
.LLCFI709:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1418
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1419
	 nop
.LL1418:
	sethi	%hi(92160), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1419:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE709:
	.size	bitfield__write_var_BitField_offset15_count12, .-bitfield__write_var_BitField_offset15_count12
	.align 4
	.type	bitfield__read_var_BitField_offset15_count13, #function
	.proc	04
bitfield__read_var_BitField_offset15_count13:
.LLFB710:
	save	%sp, -96, %sp
.LLCFI710:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE710:
	.size	bitfield__read_var_BitField_offset15_count13, .-bitfield__read_var_BitField_offset15_count13
	.align 4
	.type	bitfield__write_var_BitField_offset15_count13, #function
	.proc	020
bitfield__write_var_BitField_offset15_count13:
.LLFB711:
	save	%sp, -104, %sp
.LLCFI711:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1422
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1423
	 nop
.LL1422:
	sethi	%hi(92160), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1423:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE711:
	.size	bitfield__write_var_BitField_offset15_count13, .-bitfield__write_var_BitField_offset15_count13
	.align 4
	.type	bitfield__read_var_BitField_offset15_count14, #function
	.proc	04
bitfield__read_var_BitField_offset15_count14:
.LLFB712:
	save	%sp, -96, %sp
.LLCFI712:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE712:
	.size	bitfield__read_var_BitField_offset15_count14, .-bitfield__read_var_BitField_offset15_count14
	.align 4
	.type	bitfield__write_var_BitField_offset15_count14, #function
	.proc	020
bitfield__write_var_BitField_offset15_count14:
.LLFB713:
	save	%sp, -104, %sp
.LLCFI713:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1426
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1427
	 nop
.LL1426:
	sethi	%hi(92160), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1427:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE713:
	.size	bitfield__write_var_BitField_offset15_count14, .-bitfield__write_var_BitField_offset15_count14
	.align 4
	.type	bitfield__read_var_BitField_offset15_count15, #function
	.proc	04
bitfield__read_var_BitField_offset15_count15:
.LLFB714:
	save	%sp, -96, %sp
.LLCFI714:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE714:
	.size	bitfield__read_var_BitField_offset15_count15, .-bitfield__read_var_BitField_offset15_count15
	.align 4
	.type	bitfield__write_var_BitField_offset15_count15, #function
	.proc	020
bitfield__write_var_BitField_offset15_count15:
.LLFB715:
	save	%sp, -104, %sp
.LLCFI715:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1430
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1431
	 nop
.LL1430:
	sethi	%hi(93184), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1431:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE715:
	.size	bitfield__write_var_BitField_offset15_count15, .-bitfield__write_var_BitField_offset15_count15
	.align 4
	.type	bitfield__read_var_BitField_offset15_count16, #function
	.proc	04
bitfield__read_var_BitField_offset15_count16:
.LLFB716:
	save	%sp, -96, %sp
.LLCFI716:
	mov	%i0, %g1
	ld	[%g1], %g1
	sll	%g1, 15, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE716:
	.size	bitfield__read_var_BitField_offset15_count16, .-bitfield__read_var_BitField_offset15_count16
	.align 4
	.type	bitfield__write_var_BitField_offset15_count16, #function
	.proc	020
bitfield__write_var_BitField_offset15_count16:
.LLFB717:
	save	%sp, -104, %sp
.LLCFI717:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1434
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1435
	 nop
.LL1434:
	sethi	%hi(93184), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1435:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	mov	%g1, %o5
	ld	[%fp-4], %g1
	ld	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-131072), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	st	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE717:
	.size	bitfield__write_var_BitField_offset15_count16, .-bitfield__write_var_BitField_offset15_count16
	.align 4
	.type	bitfield__read_var_BitField_offset15_count17, #function
	.proc	04
bitfield__read_var_BitField_offset15_count17:
.LLFB718:
	save	%sp, -96, %sp
.LLCFI718:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE718:
	.size	bitfield__read_var_BitField_offset15_count17, .-bitfield__read_var_BitField_offset15_count17
	.align 4
	.type	bitfield__write_var_BitField_offset15_count17, #function
	.proc	020
bitfield__write_var_BitField_offset15_count17:
.LLFB719:
	save	%sp, -104, %sp
.LLCFI719:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1438
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1439
	 nop
.LL1438:
	sethi	%hi(93184), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1439:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	ld	[%fp-4], %o5
	ld	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-131072), %o5
	and	%o4, %o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	st	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE719:
	.size	bitfield__write_var_BitField_offset15_count17, .-bitfield__write_var_BitField_offset15_count17
	.align 4
	.type	bitfield__read_var_BitField_offset16_count1, #function
	.proc	04
bitfield__read_var_BitField_offset16_count1:
.LLFB720:
	save	%sp, -96, %sp
.LLCFI720:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE720:
	.size	bitfield__read_var_BitField_offset16_count1, .-bitfield__read_var_BitField_offset16_count1
	.align 4
	.type	bitfield__write_var_BitField_offset16_count1, #function
	.proc	020
bitfield__write_var_BitField_offset16_count1:
.LLFB721:
	save	%sp, -104, %sp
.LLCFI721:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1442
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1443
	 nop
.LL1442:
	sethi	%hi(93184), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1443:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, 127, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE721:
	.size	bitfield__write_var_BitField_offset16_count1, .-bitfield__write_var_BitField_offset16_count1
	.align 4
	.type	bitfield__read_var_BitField_offset16_count2, #function
	.proc	04
bitfield__read_var_BitField_offset16_count2:
.LLFB722:
	save	%sp, -96, %sp
.LLCFI722:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE722:
	.size	bitfield__read_var_BitField_offset16_count2, .-bitfield__read_var_BitField_offset16_count2
	.align 4
	.type	bitfield__write_var_BitField_offset16_count2, #function
	.proc	020
bitfield__write_var_BitField_offset16_count2:
.LLFB723:
	save	%sp, -104, %sp
.LLCFI723:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1446
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1447
	 nop
.LL1446:
	sethi	%hi(94208), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1447:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, 63, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE723:
	.size	bitfield__write_var_BitField_offset16_count2, .-bitfield__write_var_BitField_offset16_count2
	.align 4
	.type	bitfield__read_var_BitField_offset16_count3, #function
	.proc	04
bitfield__read_var_BitField_offset16_count3:
.LLFB724:
	save	%sp, -96, %sp
.LLCFI724:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE724:
	.size	bitfield__read_var_BitField_offset16_count3, .-bitfield__read_var_BitField_offset16_count3
	.align 4
	.type	bitfield__write_var_BitField_offset16_count3, #function
	.proc	020
bitfield__write_var_BitField_offset16_count3:
.LLFB725:
	save	%sp, -104, %sp
.LLCFI725:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1450
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1451
	 nop
.LL1450:
	sethi	%hi(94208), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1451:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, 31, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE725:
	.size	bitfield__write_var_BitField_offset16_count3, .-bitfield__write_var_BitField_offset16_count3
	.align 4
	.type	bitfield__read_var_BitField_offset16_count4, #function
	.proc	04
bitfield__read_var_BitField_offset16_count4:
.LLFB726:
	save	%sp, -96, %sp
.LLCFI726:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE726:
	.size	bitfield__read_var_BitField_offset16_count4, .-bitfield__read_var_BitField_offset16_count4
	.align 4
	.type	bitfield__write_var_BitField_offset16_count4, #function
	.proc	020
bitfield__write_var_BitField_offset16_count4:
.LLFB727:
	save	%sp, -104, %sp
.LLCFI727:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1454
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1455
	 nop
.LL1454:
	sethi	%hi(94208), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1455:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, 15, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE727:
	.size	bitfield__write_var_BitField_offset16_count4, .-bitfield__write_var_BitField_offset16_count4
	.align 4
	.type	bitfield__read_var_BitField_offset16_count5, #function
	.proc	04
bitfield__read_var_BitField_offset16_count5:
.LLFB728:
	save	%sp, -96, %sp
.LLCFI728:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE728:
	.size	bitfield__read_var_BitField_offset16_count5, .-bitfield__read_var_BitField_offset16_count5
	.align 4
	.type	bitfield__write_var_BitField_offset16_count5, #function
	.proc	020
bitfield__write_var_BitField_offset16_count5:
.LLFB729:
	save	%sp, -104, %sp
.LLCFI729:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1458
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1459
	 nop
.LL1458:
	sethi	%hi(94208), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1459:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, 7, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE729:
	.size	bitfield__write_var_BitField_offset16_count5, .-bitfield__write_var_BitField_offset16_count5
	.align 4
	.type	bitfield__read_var_BitField_offset16_count6, #function
	.proc	04
bitfield__read_var_BitField_offset16_count6:
.LLFB730:
	save	%sp, -96, %sp
.LLCFI730:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE730:
	.size	bitfield__read_var_BitField_offset16_count6, .-bitfield__read_var_BitField_offset16_count6
	.align 4
	.type	bitfield__write_var_BitField_offset16_count6, #function
	.proc	020
bitfield__write_var_BitField_offset16_count6:
.LLFB731:
	save	%sp, -104, %sp
.LLCFI731:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1462
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1463
	 nop
.LL1462:
	sethi	%hi(95232), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1463:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, 3, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE731:
	.size	bitfield__write_var_BitField_offset16_count6, .-bitfield__write_var_BitField_offset16_count6
	.align 4
	.type	bitfield__read_var_BitField_offset16_count7, #function
	.proc	04
bitfield__read_var_BitField_offset16_count7:
.LLFB732:
	save	%sp, -96, %sp
.LLCFI732:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE732:
	.size	bitfield__read_var_BitField_offset16_count7, .-bitfield__read_var_BitField_offset16_count7
	.align 4
	.type	bitfield__write_var_BitField_offset16_count7, #function
	.proc	020
bitfield__write_var_BitField_offset16_count7:
.LLFB733:
	save	%sp, -104, %sp
.LLCFI733:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1466
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1467
	 nop
.LL1466:
	sethi	%hi(95232), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1467:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, 1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE733:
	.size	bitfield__write_var_BitField_offset16_count7, .-bitfield__write_var_BitField_offset16_count7
	.align 4
	.type	bitfield__read_var_BitField_offset16_count8, #function
	.proc	04
bitfield__read_var_BitField_offset16_count8:
.LLFB734:
	save	%sp, -96, %sp
.LLCFI734:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE734:
	.size	bitfield__read_var_BitField_offset16_count8, .-bitfield__read_var_BitField_offset16_count8
	.align 4
	.type	bitfield__write_var_BitField_offset16_count8, #function
	.proc	020
bitfield__write_var_BitField_offset16_count8:
.LLFB735:
	save	%sp, -96, %sp
.LLCFI735:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1470
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1471
	 nop
.LL1470:
	sethi	%hi(95232), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1471:
	ld	[%fp+68], %g1
	add	%g1, 2, %g1
	ld	[%fp+72], %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE735:
	.size	bitfield__write_var_BitField_offset16_count8, .-bitfield__write_var_BitField_offset16_count8
	.align 4
	.type	bitfield__read_var_BitField_offset16_count9, #function
	.proc	04
bitfield__read_var_BitField_offset16_count9:
.LLFB736:
	save	%sp, -96, %sp
.LLCFI736:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE736:
	.size	bitfield__read_var_BitField_offset16_count9, .-bitfield__read_var_BitField_offset16_count9
	.align 4
	.type	bitfield__write_var_BitField_offset16_count9, #function
	.proc	020
bitfield__write_var_BitField_offset16_count9:
.LLFB737:
	save	%sp, -104, %sp
.LLCFI737:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1474
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1475
	 nop
.LL1474:
	sethi	%hi(95232), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1475:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, 127, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE737:
	.size	bitfield__write_var_BitField_offset16_count9, .-bitfield__write_var_BitField_offset16_count9
	.align 4
	.type	bitfield__read_var_BitField_offset16_count10, #function
	.proc	04
bitfield__read_var_BitField_offset16_count10:
.LLFB738:
	save	%sp, -96, %sp
.LLCFI738:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE738:
	.size	bitfield__read_var_BitField_offset16_count10, .-bitfield__read_var_BitField_offset16_count10
	.align 4
	.type	bitfield__write_var_BitField_offset16_count10, #function
	.proc	020
bitfield__write_var_BitField_offset16_count10:
.LLFB739:
	save	%sp, -104, %sp
.LLCFI739:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1478
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1479
	 nop
.LL1478:
	sethi	%hi(96256), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1479:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, 63, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE739:
	.size	bitfield__write_var_BitField_offset16_count10, .-bitfield__write_var_BitField_offset16_count10
	.align 4
	.type	bitfield__read_var_BitField_offset16_count11, #function
	.proc	04
bitfield__read_var_BitField_offset16_count11:
.LLFB740:
	save	%sp, -96, %sp
.LLCFI740:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE740:
	.size	bitfield__read_var_BitField_offset16_count11, .-bitfield__read_var_BitField_offset16_count11
	.align 4
	.type	bitfield__write_var_BitField_offset16_count11, #function
	.proc	020
bitfield__write_var_BitField_offset16_count11:
.LLFB741:
	save	%sp, -104, %sp
.LLCFI741:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1482
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1483
	 nop
.LL1482:
	sethi	%hi(96256), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1483:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, 31, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE741:
	.size	bitfield__write_var_BitField_offset16_count11, .-bitfield__write_var_BitField_offset16_count11
	.align 4
	.type	bitfield__read_var_BitField_offset16_count12, #function
	.proc	04
bitfield__read_var_BitField_offset16_count12:
.LLFB742:
	save	%sp, -96, %sp
.LLCFI742:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE742:
	.size	bitfield__read_var_BitField_offset16_count12, .-bitfield__read_var_BitField_offset16_count12
	.align 4
	.type	bitfield__write_var_BitField_offset16_count12, #function
	.proc	020
bitfield__write_var_BitField_offset16_count12:
.LLFB743:
	save	%sp, -104, %sp
.LLCFI743:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1486
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1487
	 nop
.LL1486:
	sethi	%hi(96256), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1487:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, 15, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE743:
	.size	bitfield__write_var_BitField_offset16_count12, .-bitfield__write_var_BitField_offset16_count12
	.align 4
	.type	bitfield__read_var_BitField_offset16_count13, #function
	.proc	04
bitfield__read_var_BitField_offset16_count13:
.LLFB744:
	save	%sp, -96, %sp
.LLCFI744:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE744:
	.size	bitfield__read_var_BitField_offset16_count13, .-bitfield__read_var_BitField_offset16_count13
	.align 4
	.type	bitfield__write_var_BitField_offset16_count13, #function
	.proc	020
bitfield__write_var_BitField_offset16_count13:
.LLFB745:
	save	%sp, -104, %sp
.LLCFI745:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1490
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1491
	 nop
.LL1490:
	sethi	%hi(96256), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1491:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, 7, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE745:
	.size	bitfield__write_var_BitField_offset16_count13, .-bitfield__write_var_BitField_offset16_count13
	.align 4
	.type	bitfield__read_var_BitField_offset16_count14, #function
	.proc	04
bitfield__read_var_BitField_offset16_count14:
.LLFB746:
	save	%sp, -96, %sp
.LLCFI746:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE746:
	.size	bitfield__read_var_BitField_offset16_count14, .-bitfield__read_var_BitField_offset16_count14
	.align 4
	.type	bitfield__write_var_BitField_offset16_count14, #function
	.proc	020
bitfield__write_var_BitField_offset16_count14:
.LLFB747:
	save	%sp, -104, %sp
.LLCFI747:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1494
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1495
	 nop
.LL1494:
	sethi	%hi(97280), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1495:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, 3, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE747:
	.size	bitfield__write_var_BitField_offset16_count14, .-bitfield__write_var_BitField_offset16_count14
	.align 4
	.type	bitfield__read_var_BitField_offset16_count15, #function
	.proc	04
bitfield__read_var_BitField_offset16_count15:
.LLFB748:
	save	%sp, -96, %sp
.LLCFI748:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 17, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE748:
	.size	bitfield__read_var_BitField_offset16_count15, .-bitfield__read_var_BitField_offset16_count15
	.align 4
	.type	bitfield__write_var_BitField_offset16_count15, #function
	.proc	020
bitfield__write_var_BitField_offset16_count15:
.LLFB749:
	save	%sp, -104, %sp
.LLCFI749:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1498
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1499
	 nop
.LL1498:
	sethi	%hi(97280), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1499:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	and	%g1, 1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE749:
	.size	bitfield__write_var_BitField_offset16_count15, .-bitfield__write_var_BitField_offset16_count15
	.align 4
	.type	bitfield__read_var_BitField_offset16_count16, #function
	.proc	04
bitfield__read_var_BitField_offset16_count16:
.LLFB750:
	save	%sp, -96, %sp
.LLCFI750:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE750:
	.size	bitfield__read_var_BitField_offset16_count16, .-bitfield__read_var_BitField_offset16_count16
	.align 4
	.type	bitfield__write_var_BitField_offset16_count16, #function
	.proc	020
bitfield__write_var_BitField_offset16_count16:
.LLFB751:
	save	%sp, -96, %sp
.LLCFI751:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1502
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1503
	 nop
.LL1502:
	sethi	%hi(97280), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1503:
	ld	[%fp+68], %g1
	add	%g1, 2, %g1
	ld	[%fp+72], %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE751:
	.size	bitfield__write_var_BitField_offset16_count16, .-bitfield__write_var_BitField_offset16_count16
	.align 4
	.type	bitfield__read_var_BitField_offset17_count1, #function
	.proc	04
bitfield__read_var_BitField_offset17_count1:
.LLFB752:
	save	%sp, -96, %sp
.LLCFI752:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE752:
	.size	bitfield__read_var_BitField_offset17_count1, .-bitfield__read_var_BitField_offset17_count1
	.align 4
	.type	bitfield__write_var_BitField_offset17_count1, #function
	.proc	020
bitfield__write_var_BitField_offset17_count1:
.LLFB753:
	save	%sp, -104, %sp
.LLCFI753:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1506
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1507
	 nop
.LL1506:
	sethi	%hi(97280), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1507:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -65, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE753:
	.size	bitfield__write_var_BitField_offset17_count1, .-bitfield__write_var_BitField_offset17_count1
	.align 4
	.type	bitfield__read_var_BitField_offset17_count2, #function
	.proc	04
bitfield__read_var_BitField_offset17_count2:
.LLFB754:
	save	%sp, -96, %sp
.LLCFI754:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE754:
	.size	bitfield__read_var_BitField_offset17_count2, .-bitfield__read_var_BitField_offset17_count2
	.align 4
	.type	bitfield__write_var_BitField_offset17_count2, #function
	.proc	020
bitfield__write_var_BitField_offset17_count2:
.LLFB755:
	save	%sp, -104, %sp
.LLCFI755:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1510
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1511
	 nop
.LL1510:
	sethi	%hi(98304), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1511:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -97, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE755:
	.size	bitfield__write_var_BitField_offset17_count2, .-bitfield__write_var_BitField_offset17_count2
	.align 4
	.type	bitfield__read_var_BitField_offset17_count3, #function
	.proc	04
bitfield__read_var_BitField_offset17_count3:
.LLFB756:
	save	%sp, -96, %sp
.LLCFI756:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE756:
	.size	bitfield__read_var_BitField_offset17_count3, .-bitfield__read_var_BitField_offset17_count3
	.align 4
	.type	bitfield__write_var_BitField_offset17_count3, #function
	.proc	020
bitfield__write_var_BitField_offset17_count3:
.LLFB757:
	save	%sp, -104, %sp
.LLCFI757:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1514
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1515
	 nop
.LL1514:
	sethi	%hi(98304), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1515:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -113, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE757:
	.size	bitfield__write_var_BitField_offset17_count3, .-bitfield__write_var_BitField_offset17_count3
	.align 4
	.type	bitfield__read_var_BitField_offset17_count4, #function
	.proc	04
bitfield__read_var_BitField_offset17_count4:
.LLFB758:
	save	%sp, -96, %sp
.LLCFI758:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE758:
	.size	bitfield__read_var_BitField_offset17_count4, .-bitfield__read_var_BitField_offset17_count4
	.align 4
	.type	bitfield__write_var_BitField_offset17_count4, #function
	.proc	020
bitfield__write_var_BitField_offset17_count4:
.LLFB759:
	save	%sp, -104, %sp
.LLCFI759:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1518
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1519
	 nop
.LL1518:
	sethi	%hi(98304), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1519:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -121, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE759:
	.size	bitfield__write_var_BitField_offset17_count4, .-bitfield__write_var_BitField_offset17_count4
	.align 4
	.type	bitfield__read_var_BitField_offset17_count5, #function
	.proc	04
bitfield__read_var_BitField_offset17_count5:
.LLFB760:
	save	%sp, -96, %sp
.LLCFI760:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE760:
	.size	bitfield__read_var_BitField_offset17_count5, .-bitfield__read_var_BitField_offset17_count5
	.align 4
	.type	bitfield__write_var_BitField_offset17_count5, #function
	.proc	020
bitfield__write_var_BitField_offset17_count5:
.LLFB761:
	save	%sp, -104, %sp
.LLCFI761:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1522
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1523
	 nop
.LL1522:
	sethi	%hi(98304), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1523:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -125, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE761:
	.size	bitfield__write_var_BitField_offset17_count5, .-bitfield__write_var_BitField_offset17_count5
	.align 4
	.type	bitfield__read_var_BitField_offset17_count6, #function
	.proc	04
bitfield__read_var_BitField_offset17_count6:
.LLFB762:
	save	%sp, -96, %sp
.LLCFI762:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE762:
	.size	bitfield__read_var_BitField_offset17_count6, .-bitfield__read_var_BitField_offset17_count6
	.align 4
	.type	bitfield__write_var_BitField_offset17_count6, #function
	.proc	020
bitfield__write_var_BitField_offset17_count6:
.LLFB763:
	save	%sp, -104, %sp
.LLCFI763:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1526
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1527
	 nop
.LL1526:
	sethi	%hi(99328), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1527:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, -127, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE763:
	.size	bitfield__write_var_BitField_offset17_count6, .-bitfield__write_var_BitField_offset17_count6
	.align 4
	.type	bitfield__read_var_BitField_offset17_count7, #function
	.proc	04
bitfield__read_var_BitField_offset17_count7:
.LLFB764:
	save	%sp, -96, %sp
.LLCFI764:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE764:
	.size	bitfield__read_var_BitField_offset17_count7, .-bitfield__read_var_BitField_offset17_count7
	.align 4
	.type	bitfield__write_var_BitField_offset17_count7, #function
	.proc	020
bitfield__write_var_BitField_offset17_count7:
.LLFB765:
	save	%sp, -104, %sp
.LLCFI765:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1530
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1531
	 nop
.LL1530:
	sethi	%hi(99328), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1531:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -128, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE765:
	.size	bitfield__write_var_BitField_offset17_count7, .-bitfield__write_var_BitField_offset17_count7
	.align 4
	.type	bitfield__read_var_BitField_offset17_count8, #function
	.proc	04
bitfield__read_var_BitField_offset17_count8:
.LLFB766:
	save	%sp, -96, %sp
.LLCFI766:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE766:
	.size	bitfield__read_var_BitField_offset17_count8, .-bitfield__read_var_BitField_offset17_count8
	.align 4
	.type	bitfield__write_var_BitField_offset17_count8, #function
	.proc	020
bitfield__write_var_BitField_offset17_count8:
.LLFB767:
	save	%sp, -104, %sp
.LLCFI767:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1534
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1535
	 nop
.LL1534:
	sethi	%hi(99328), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1535:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE767:
	.size	bitfield__write_var_BitField_offset17_count8, .-bitfield__write_var_BitField_offset17_count8
	.align 4
	.type	bitfield__read_var_BitField_offset17_count9, #function
	.proc	04
bitfield__read_var_BitField_offset17_count9:
.LLFB768:
	save	%sp, -96, %sp
.LLCFI768:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE768:
	.size	bitfield__read_var_BitField_offset17_count9, .-bitfield__read_var_BitField_offset17_count9
	.align 4
	.type	bitfield__write_var_BitField_offset17_count9, #function
	.proc	020
bitfield__write_var_BitField_offset17_count9:
.LLFB769:
	save	%sp, -104, %sp
.LLCFI769:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1538
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1539
	 nop
.LL1538:
	sethi	%hi(99328), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1539:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE769:
	.size	bitfield__write_var_BitField_offset17_count9, .-bitfield__write_var_BitField_offset17_count9
	.align 4
	.type	bitfield__read_var_BitField_offset17_count10, #function
	.proc	04
bitfield__read_var_BitField_offset17_count10:
.LLFB770:
	save	%sp, -96, %sp
.LLCFI770:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE770:
	.size	bitfield__read_var_BitField_offset17_count10, .-bitfield__read_var_BitField_offset17_count10
	.align 4
	.type	bitfield__write_var_BitField_offset17_count10, #function
	.proc	020
bitfield__write_var_BitField_offset17_count10:
.LLFB771:
	save	%sp, -104, %sp
.LLCFI771:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1542
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1543
	 nop
.LL1542:
	sethi	%hi(100352), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1543:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE771:
	.size	bitfield__write_var_BitField_offset17_count10, .-bitfield__write_var_BitField_offset17_count10
	.align 4
	.type	bitfield__read_var_BitField_offset17_count11, #function
	.proc	04
bitfield__read_var_BitField_offset17_count11:
.LLFB772:
	save	%sp, -96, %sp
.LLCFI772:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE772:
	.size	bitfield__read_var_BitField_offset17_count11, .-bitfield__read_var_BitField_offset17_count11
	.align 4
	.type	bitfield__write_var_BitField_offset17_count11, #function
	.proc	020
bitfield__write_var_BitField_offset17_count11:
.LLFB773:
	save	%sp, -104, %sp
.LLCFI773:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1546
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1547
	 nop
.LL1546:
	sethi	%hi(100352), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1547:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE773:
	.size	bitfield__write_var_BitField_offset17_count11, .-bitfield__write_var_BitField_offset17_count11
	.align 4
	.type	bitfield__read_var_BitField_offset17_count12, #function
	.proc	04
bitfield__read_var_BitField_offset17_count12:
.LLFB774:
	save	%sp, -96, %sp
.LLCFI774:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE774:
	.size	bitfield__read_var_BitField_offset17_count12, .-bitfield__read_var_BitField_offset17_count12
	.align 4
	.type	bitfield__write_var_BitField_offset17_count12, #function
	.proc	020
bitfield__write_var_BitField_offset17_count12:
.LLFB775:
	save	%sp, -104, %sp
.LLCFI775:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1550
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1551
	 nop
.LL1550:
	sethi	%hi(100352), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1551:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE775:
	.size	bitfield__write_var_BitField_offset17_count12, .-bitfield__write_var_BitField_offset17_count12
	.align 4
	.type	bitfield__read_var_BitField_offset17_count13, #function
	.proc	04
bitfield__read_var_BitField_offset17_count13:
.LLFB776:
	save	%sp, -96, %sp
.LLCFI776:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE776:
	.size	bitfield__read_var_BitField_offset17_count13, .-bitfield__read_var_BitField_offset17_count13
	.align 4
	.type	bitfield__write_var_BitField_offset17_count13, #function
	.proc	020
bitfield__write_var_BitField_offset17_count13:
.LLFB777:
	save	%sp, -104, %sp
.LLCFI777:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1554
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1555
	 nop
.LL1554:
	sethi	%hi(100352), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1555:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE777:
	.size	bitfield__write_var_BitField_offset17_count13, .-bitfield__write_var_BitField_offset17_count13
	.align 4
	.type	bitfield__read_var_BitField_offset17_count14, #function
	.proc	04
bitfield__read_var_BitField_offset17_count14:
.LLFB778:
	save	%sp, -96, %sp
.LLCFI778:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 17, %g1
	srl	%g1, 18, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE778:
	.size	bitfield__read_var_BitField_offset17_count14, .-bitfield__read_var_BitField_offset17_count14
	.align 4
	.type	bitfield__write_var_BitField_offset17_count14, #function
	.proc	020
bitfield__write_var_BitField_offset17_count14:
.LLFB779:
	save	%sp, -104, %sp
.LLCFI779:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1558
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1559
	 nop
.LL1558:
	sethi	%hi(101376), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1559:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-32768), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE779:
	.size	bitfield__write_var_BitField_offset17_count14, .-bitfield__write_var_BitField_offset17_count14
	.align 4
	.type	bitfield__read_var_BitField_offset17_count15, #function
	.proc	04
bitfield__read_var_BitField_offset17_count15:
.LLFB780:
	save	%sp, -96, %sp
.LLCFI780:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE780:
	.size	bitfield__read_var_BitField_offset17_count15, .-bitfield__read_var_BitField_offset17_count15
	.align 4
	.type	bitfield__write_var_BitField_offset17_count15, #function
	.proc	020
bitfield__write_var_BitField_offset17_count15:
.LLFB781:
	save	%sp, -104, %sp
.LLCFI781:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1562
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1563
	 nop
.LL1562:
	sethi	%hi(101376), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1563:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-32768), %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	mov	%o5, %o3
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE781:
	.size	bitfield__write_var_BitField_offset17_count15, .-bitfield__write_var_BitField_offset17_count15
	.align 4
	.type	bitfield__read_var_BitField_offset18_count1, #function
	.proc	04
bitfield__read_var_BitField_offset18_count1:
.LLFB782:
	save	%sp, -96, %sp
.LLCFI782:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE782:
	.size	bitfield__read_var_BitField_offset18_count1, .-bitfield__read_var_BitField_offset18_count1
	.align 4
	.type	bitfield__write_var_BitField_offset18_count1, #function
	.proc	020
bitfield__write_var_BitField_offset18_count1:
.LLFB783:
	save	%sp, -104, %sp
.LLCFI783:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1566
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1567
	 nop
.LL1566:
	sethi	%hi(101376), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1567:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -33, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE783:
	.size	bitfield__write_var_BitField_offset18_count1, .-bitfield__write_var_BitField_offset18_count1
	.align 4
	.type	bitfield__read_var_BitField_offset18_count2, #function
	.proc	04
bitfield__read_var_BitField_offset18_count2:
.LLFB784:
	save	%sp, -96, %sp
.LLCFI784:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE784:
	.size	bitfield__read_var_BitField_offset18_count2, .-bitfield__read_var_BitField_offset18_count2
	.align 4
	.type	bitfield__write_var_BitField_offset18_count2, #function
	.proc	020
bitfield__write_var_BitField_offset18_count2:
.LLFB785:
	save	%sp, -104, %sp
.LLCFI785:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1570
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1571
	 nop
.LL1570:
	sethi	%hi(101376), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1571:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -49, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE785:
	.size	bitfield__write_var_BitField_offset18_count2, .-bitfield__write_var_BitField_offset18_count2
	.align 4
	.type	bitfield__read_var_BitField_offset18_count3, #function
	.proc	04
bitfield__read_var_BitField_offset18_count3:
.LLFB786:
	save	%sp, -96, %sp
.LLCFI786:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE786:
	.size	bitfield__read_var_BitField_offset18_count3, .-bitfield__read_var_BitField_offset18_count3
	.align 4
	.type	bitfield__write_var_BitField_offset18_count3, #function
	.proc	020
bitfield__write_var_BitField_offset18_count3:
.LLFB787:
	save	%sp, -104, %sp
.LLCFI787:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1574
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1575
	 nop
.LL1574:
	sethi	%hi(102400), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1575:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -57, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE787:
	.size	bitfield__write_var_BitField_offset18_count3, .-bitfield__write_var_BitField_offset18_count3
	.align 4
	.type	bitfield__read_var_BitField_offset18_count4, #function
	.proc	04
bitfield__read_var_BitField_offset18_count4:
.LLFB788:
	save	%sp, -96, %sp
.LLCFI788:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE788:
	.size	bitfield__read_var_BitField_offset18_count4, .-bitfield__read_var_BitField_offset18_count4
	.align 4
	.type	bitfield__write_var_BitField_offset18_count4, #function
	.proc	020
bitfield__write_var_BitField_offset18_count4:
.LLFB789:
	save	%sp, -104, %sp
.LLCFI789:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1578
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1579
	 nop
.LL1578:
	sethi	%hi(102400), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1579:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -61, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE789:
	.size	bitfield__write_var_BitField_offset18_count4, .-bitfield__write_var_BitField_offset18_count4
	.align 4
	.type	bitfield__read_var_BitField_offset18_count5, #function
	.proc	04
bitfield__read_var_BitField_offset18_count5:
.LLFB790:
	save	%sp, -96, %sp
.LLCFI790:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE790:
	.size	bitfield__read_var_BitField_offset18_count5, .-bitfield__read_var_BitField_offset18_count5
	.align 4
	.type	bitfield__write_var_BitField_offset18_count5, #function
	.proc	020
bitfield__write_var_BitField_offset18_count5:
.LLFB791:
	save	%sp, -104, %sp
.LLCFI791:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1582
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1583
	 nop
.LL1582:
	sethi	%hi(102400), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1583:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, -63, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE791:
	.size	bitfield__write_var_BitField_offset18_count5, .-bitfield__write_var_BitField_offset18_count5
	.align 4
	.type	bitfield__read_var_BitField_offset18_count6, #function
	.proc	04
bitfield__read_var_BitField_offset18_count6:
.LLFB792:
	save	%sp, -96, %sp
.LLCFI792:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE792:
	.size	bitfield__read_var_BitField_offset18_count6, .-bitfield__read_var_BitField_offset18_count6
	.align 4
	.type	bitfield__write_var_BitField_offset18_count6, #function
	.proc	020
bitfield__write_var_BitField_offset18_count6:
.LLFB793:
	save	%sp, -104, %sp
.LLCFI793:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1586
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1587
	 nop
.LL1586:
	sethi	%hi(102400), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1587:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -64, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE793:
	.size	bitfield__write_var_BitField_offset18_count6, .-bitfield__write_var_BitField_offset18_count6
	.align 4
	.type	bitfield__read_var_BitField_offset18_count7, #function
	.proc	04
bitfield__read_var_BitField_offset18_count7:
.LLFB794:
	save	%sp, -96, %sp
.LLCFI794:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE794:
	.size	bitfield__read_var_BitField_offset18_count7, .-bitfield__read_var_BitField_offset18_count7
	.align 4
	.type	bitfield__write_var_BitField_offset18_count7, #function
	.proc	020
bitfield__write_var_BitField_offset18_count7:
.LLFB795:
	save	%sp, -104, %sp
.LLCFI795:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1590
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1591
	 nop
.LL1590:
	sethi	%hi(103424), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1591:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE795:
	.size	bitfield__write_var_BitField_offset18_count7, .-bitfield__write_var_BitField_offset18_count7
	.align 4
	.type	bitfield__read_var_BitField_offset18_count8, #function
	.proc	04
bitfield__read_var_BitField_offset18_count8:
.LLFB796:
	save	%sp, -96, %sp
.LLCFI796:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE796:
	.size	bitfield__read_var_BitField_offset18_count8, .-bitfield__read_var_BitField_offset18_count8
	.align 4
	.type	bitfield__write_var_BitField_offset18_count8, #function
	.proc	020
bitfield__write_var_BitField_offset18_count8:
.LLFB797:
	save	%sp, -104, %sp
.LLCFI797:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1594
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1595
	 nop
.LL1594:
	sethi	%hi(103424), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1595:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE797:
	.size	bitfield__write_var_BitField_offset18_count8, .-bitfield__write_var_BitField_offset18_count8
	.align 4
	.type	bitfield__read_var_BitField_offset18_count9, #function
	.proc	04
bitfield__read_var_BitField_offset18_count9:
.LLFB798:
	save	%sp, -96, %sp
.LLCFI798:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE798:
	.size	bitfield__read_var_BitField_offset18_count9, .-bitfield__read_var_BitField_offset18_count9
	.align 4
	.type	bitfield__write_var_BitField_offset18_count9, #function
	.proc	020
bitfield__write_var_BitField_offset18_count9:
.LLFB799:
	save	%sp, -104, %sp
.LLCFI799:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1598
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1599
	 nop
.LL1598:
	sethi	%hi(103424), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1599:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE799:
	.size	bitfield__write_var_BitField_offset18_count9, .-bitfield__write_var_BitField_offset18_count9
	.align 4
	.type	bitfield__read_var_BitField_offset18_count10, #function
	.proc	04
bitfield__read_var_BitField_offset18_count10:
.LLFB800:
	save	%sp, -96, %sp
.LLCFI800:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE800:
	.size	bitfield__read_var_BitField_offset18_count10, .-bitfield__read_var_BitField_offset18_count10
	.align 4
	.type	bitfield__write_var_BitField_offset18_count10, #function
	.proc	020
bitfield__write_var_BitField_offset18_count10:
.LLFB801:
	save	%sp, -104, %sp
.LLCFI801:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1602
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1603
	 nop
.LL1602:
	sethi	%hi(103424), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1603:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE801:
	.size	bitfield__write_var_BitField_offset18_count10, .-bitfield__write_var_BitField_offset18_count10
	.align 4
	.type	bitfield__read_var_BitField_offset18_count11, #function
	.proc	04
bitfield__read_var_BitField_offset18_count11:
.LLFB802:
	save	%sp, -96, %sp
.LLCFI802:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE802:
	.size	bitfield__read_var_BitField_offset18_count11, .-bitfield__read_var_BitField_offset18_count11
	.align 4
	.type	bitfield__write_var_BitField_offset18_count11, #function
	.proc	020
bitfield__write_var_BitField_offset18_count11:
.LLFB803:
	save	%sp, -104, %sp
.LLCFI803:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1606
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1607
	 nop
.LL1606:
	sethi	%hi(104448), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1607:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE803:
	.size	bitfield__write_var_BitField_offset18_count11, .-bitfield__write_var_BitField_offset18_count11
	.align 4
	.type	bitfield__read_var_BitField_offset18_count12, #function
	.proc	04
bitfield__read_var_BitField_offset18_count12:
.LLFB804:
	save	%sp, -96, %sp
.LLCFI804:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE804:
	.size	bitfield__read_var_BitField_offset18_count12, .-bitfield__read_var_BitField_offset18_count12
	.align 4
	.type	bitfield__write_var_BitField_offset18_count12, #function
	.proc	020
bitfield__write_var_BitField_offset18_count12:
.LLFB805:
	save	%sp, -104, %sp
.LLCFI805:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1610
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1611
	 nop
.LL1610:
	sethi	%hi(104448), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1611:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE805:
	.size	bitfield__write_var_BitField_offset18_count12, .-bitfield__write_var_BitField_offset18_count12
	.align 4
	.type	bitfield__read_var_BitField_offset18_count13, #function
	.proc	04
bitfield__read_var_BitField_offset18_count13:
.LLFB806:
	save	%sp, -96, %sp
.LLCFI806:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 18, %g1
	srl	%g1, 19, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE806:
	.size	bitfield__read_var_BitField_offset18_count13, .-bitfield__read_var_BitField_offset18_count13
	.align 4
	.type	bitfield__write_var_BitField_offset18_count13, #function
	.proc	020
bitfield__write_var_BitField_offset18_count13:
.LLFB807:
	save	%sp, -104, %sp
.LLCFI807:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1614
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1615
	 nop
.LL1614:
	sethi	%hi(104448), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1615:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-16384), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %o3
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%o3, %g1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE807:
	.size	bitfield__write_var_BitField_offset18_count13, .-bitfield__write_var_BitField_offset18_count13
	.align 4
	.type	bitfield__read_var_BitField_offset18_count14, #function
	.proc	04
bitfield__read_var_BitField_offset18_count14:
.LLFB808:
	save	%sp, -96, %sp
.LLCFI808:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE808:
	.size	bitfield__read_var_BitField_offset18_count14, .-bitfield__read_var_BitField_offset18_count14
	.align 4
	.type	bitfield__write_var_BitField_offset18_count14, #function
	.proc	020
bitfield__write_var_BitField_offset18_count14:
.LLFB809:
	save	%sp, -104, %sp
.LLCFI809:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1618
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1619
	 nop
.LL1618:
	sethi	%hi(104448), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1619:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-16384), %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	mov	%o5, %o3
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE809:
	.size	bitfield__write_var_BitField_offset18_count14, .-bitfield__write_var_BitField_offset18_count14
	.align 4
	.type	bitfield__read_var_BitField_offset19_count1, #function
	.proc	04
bitfield__read_var_BitField_offset19_count1:
.LLFB810:
	save	%sp, -96, %sp
.LLCFI810:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE810:
	.size	bitfield__read_var_BitField_offset19_count1, .-bitfield__read_var_BitField_offset19_count1
	.align 4
	.type	bitfield__write_var_BitField_offset19_count1, #function
	.proc	020
bitfield__write_var_BitField_offset19_count1:
.LLFB811:
	save	%sp, -104, %sp
.LLCFI811:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1622
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1623
	 nop
.LL1622:
	sethi	%hi(105472), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1623:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -17, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE811:
	.size	bitfield__write_var_BitField_offset19_count1, .-bitfield__write_var_BitField_offset19_count1
	.align 4
	.type	bitfield__read_var_BitField_offset19_count2, #function
	.proc	04
bitfield__read_var_BitField_offset19_count2:
.LLFB812:
	save	%sp, -96, %sp
.LLCFI812:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE812:
	.size	bitfield__read_var_BitField_offset19_count2, .-bitfield__read_var_BitField_offset19_count2
	.align 4
	.type	bitfield__write_var_BitField_offset19_count2, #function
	.proc	020
bitfield__write_var_BitField_offset19_count2:
.LLFB813:
	save	%sp, -104, %sp
.LLCFI813:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1626
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1627
	 nop
.LL1626:
	sethi	%hi(105472), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1627:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -25, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE813:
	.size	bitfield__write_var_BitField_offset19_count2, .-bitfield__write_var_BitField_offset19_count2
	.align 4
	.type	bitfield__read_var_BitField_offset19_count3, #function
	.proc	04
bitfield__read_var_BitField_offset19_count3:
.LLFB814:
	save	%sp, -96, %sp
.LLCFI814:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE814:
	.size	bitfield__read_var_BitField_offset19_count3, .-bitfield__read_var_BitField_offset19_count3
	.align 4
	.type	bitfield__write_var_BitField_offset19_count3, #function
	.proc	020
bitfield__write_var_BitField_offset19_count3:
.LLFB815:
	save	%sp, -104, %sp
.LLCFI815:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1630
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1631
	 nop
.LL1630:
	sethi	%hi(105472), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1631:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -29, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE815:
	.size	bitfield__write_var_BitField_offset19_count3, .-bitfield__write_var_BitField_offset19_count3
	.align 4
	.type	bitfield__read_var_BitField_offset19_count4, #function
	.proc	04
bitfield__read_var_BitField_offset19_count4:
.LLFB816:
	save	%sp, -96, %sp
.LLCFI816:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE816:
	.size	bitfield__read_var_BitField_offset19_count4, .-bitfield__read_var_BitField_offset19_count4
	.align 4
	.type	bitfield__write_var_BitField_offset19_count4, #function
	.proc	020
bitfield__write_var_BitField_offset19_count4:
.LLFB817:
	save	%sp, -104, %sp
.LLCFI817:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1634
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1635
	 nop
.LL1634:
	sethi	%hi(105472), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1635:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, -31, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE817:
	.size	bitfield__write_var_BitField_offset19_count4, .-bitfield__write_var_BitField_offset19_count4
	.align 4
	.type	bitfield__read_var_BitField_offset19_count5, #function
	.proc	04
bitfield__read_var_BitField_offset19_count5:
.LLFB818:
	save	%sp, -96, %sp
.LLCFI818:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE818:
	.size	bitfield__read_var_BitField_offset19_count5, .-bitfield__read_var_BitField_offset19_count5
	.align 4
	.type	bitfield__write_var_BitField_offset19_count5, #function
	.proc	020
bitfield__write_var_BitField_offset19_count5:
.LLFB819:
	save	%sp, -104, %sp
.LLCFI819:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1638
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1639
	 nop
.LL1638:
	sethi	%hi(106496), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1639:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -32, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE819:
	.size	bitfield__write_var_BitField_offset19_count5, .-bitfield__write_var_BitField_offset19_count5
	.align 4
	.type	bitfield__read_var_BitField_offset19_count6, #function
	.proc	04
bitfield__read_var_BitField_offset19_count6:
.LLFB820:
	save	%sp, -96, %sp
.LLCFI820:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE820:
	.size	bitfield__read_var_BitField_offset19_count6, .-bitfield__read_var_BitField_offset19_count6
	.align 4
	.type	bitfield__write_var_BitField_offset19_count6, #function
	.proc	020
bitfield__write_var_BitField_offset19_count6:
.LLFB821:
	save	%sp, -104, %sp
.LLCFI821:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1642
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1643
	 nop
.LL1642:
	sethi	%hi(106496), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1643:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 127, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE821:
	.size	bitfield__write_var_BitField_offset19_count6, .-bitfield__write_var_BitField_offset19_count6
	.align 4
	.type	bitfield__read_var_BitField_offset19_count7, #function
	.proc	04
bitfield__read_var_BitField_offset19_count7:
.LLFB822:
	save	%sp, -96, %sp
.LLCFI822:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE822:
	.size	bitfield__read_var_BitField_offset19_count7, .-bitfield__read_var_BitField_offset19_count7
	.align 4
	.type	bitfield__write_var_BitField_offset19_count7, #function
	.proc	020
bitfield__write_var_BitField_offset19_count7:
.LLFB823:
	save	%sp, -104, %sp
.LLCFI823:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1646
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1647
	 nop
.LL1646:
	sethi	%hi(106496), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1647:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 63, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE823:
	.size	bitfield__write_var_BitField_offset19_count7, .-bitfield__write_var_BitField_offset19_count7
	.align 4
	.type	bitfield__read_var_BitField_offset19_count8, #function
	.proc	04
bitfield__read_var_BitField_offset19_count8:
.LLFB824:
	save	%sp, -96, %sp
.LLCFI824:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE824:
	.size	bitfield__read_var_BitField_offset19_count8, .-bitfield__read_var_BitField_offset19_count8
	.align 4
	.type	bitfield__write_var_BitField_offset19_count8, #function
	.proc	020
bitfield__write_var_BitField_offset19_count8:
.LLFB825:
	save	%sp, -104, %sp
.LLCFI825:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1650
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1651
	 nop
.LL1650:
	sethi	%hi(106496), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1651:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 31, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE825:
	.size	bitfield__write_var_BitField_offset19_count8, .-bitfield__write_var_BitField_offset19_count8
	.align 4
	.type	bitfield__read_var_BitField_offset19_count9, #function
	.proc	04
bitfield__read_var_BitField_offset19_count9:
.LLFB826:
	save	%sp, -96, %sp
.LLCFI826:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE826:
	.size	bitfield__read_var_BitField_offset19_count9, .-bitfield__read_var_BitField_offset19_count9
	.align 4
	.type	bitfield__write_var_BitField_offset19_count9, #function
	.proc	020
bitfield__write_var_BitField_offset19_count9:
.LLFB827:
	save	%sp, -104, %sp
.LLCFI827:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1654
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1655
	 nop
.LL1654:
	sethi	%hi(107520), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1655:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 15, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE827:
	.size	bitfield__write_var_BitField_offset19_count9, .-bitfield__write_var_BitField_offset19_count9
	.align 4
	.type	bitfield__read_var_BitField_offset19_count10, #function
	.proc	04
bitfield__read_var_BitField_offset19_count10:
.LLFB828:
	save	%sp, -96, %sp
.LLCFI828:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE828:
	.size	bitfield__read_var_BitField_offset19_count10, .-bitfield__read_var_BitField_offset19_count10
	.align 4
	.type	bitfield__write_var_BitField_offset19_count10, #function
	.proc	020
bitfield__write_var_BitField_offset19_count10:
.LLFB829:
	save	%sp, -104, %sp
.LLCFI829:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1658
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1659
	 nop
.LL1658:
	sethi	%hi(107520), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1659:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 7, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE829:
	.size	bitfield__write_var_BitField_offset19_count10, .-bitfield__write_var_BitField_offset19_count10
	.align 4
	.type	bitfield__read_var_BitField_offset19_count11, #function
	.proc	04
bitfield__read_var_BitField_offset19_count11:
.LLFB830:
	save	%sp, -96, %sp
.LLCFI830:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE830:
	.size	bitfield__read_var_BitField_offset19_count11, .-bitfield__read_var_BitField_offset19_count11
	.align 4
	.type	bitfield__write_var_BitField_offset19_count11, #function
	.proc	020
bitfield__write_var_BitField_offset19_count11:
.LLFB831:
	save	%sp, -104, %sp
.LLCFI831:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1662
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1663
	 nop
.LL1662:
	sethi	%hi(107520), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1663:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	or	%o5, 3, %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE831:
	.size	bitfield__write_var_BitField_offset19_count11, .-bitfield__write_var_BitField_offset19_count11
	.align 4
	.type	bitfield__read_var_BitField_offset19_count12, #function
	.proc	04
bitfield__read_var_BitField_offset19_count12:
.LLFB832:
	save	%sp, -96, %sp
.LLCFI832:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 19, %g1
	srl	%g1, 20, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE832:
	.size	bitfield__read_var_BitField_offset19_count12, .-bitfield__read_var_BitField_offset19_count12
	.align 4
	.type	bitfield__write_var_BitField_offset19_count12, #function
	.proc	020
bitfield__write_var_BitField_offset19_count12:
.LLFB833:
	save	%sp, -104, %sp
.LLCFI833:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1666
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1667
	 nop
.LL1666:
	sethi	%hi(107520), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1667:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	mov	%g1, %o4
	sethi	%hi(-8192), %g1
	or	%g1, 1, %g1
	and	%o4, %g1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 4095, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE833:
	.size	bitfield__write_var_BitField_offset19_count12, .-bitfield__write_var_BitField_offset19_count12
	.align 4
	.type	bitfield__read_var_BitField_offset19_count13, #function
	.proc	04
bitfield__read_var_BitField_offset19_count13:
.LLFB834:
	save	%sp, -96, %sp
.LLCFI834:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE834:
	.size	bitfield__read_var_BitField_offset19_count13, .-bitfield__read_var_BitField_offset19_count13
	.align 4
	.type	bitfield__write_var_BitField_offset19_count13, #function
	.proc	020
bitfield__write_var_BitField_offset19_count13:
.LLFB835:
	save	%sp, -104, %sp
.LLCFI835:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1670
	 nop
	ld	[%fp+72], %o5
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1671
	 nop
.LL1670:
	sethi	%hi(108544), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1671:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	mov	%o5, %o4
	sethi	%hi(-8192), %o5
	and	%o4, %o5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	mov	%o5, %o3
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o3, %o5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE835:
	.size	bitfield__write_var_BitField_offset19_count13, .-bitfield__write_var_BitField_offset19_count13
	.align 4
	.type	bitfield__read_var_BitField_offset20_count1, #function
	.proc	04
bitfield__read_var_BitField_offset20_count1:
.LLFB836:
	save	%sp, -96, %sp
.LLCFI836:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE836:
	.size	bitfield__read_var_BitField_offset20_count1, .-bitfield__read_var_BitField_offset20_count1
	.align 4
	.type	bitfield__write_var_BitField_offset20_count1, #function
	.proc	020
bitfield__write_var_BitField_offset20_count1:
.LLFB837:
	save	%sp, -104, %sp
.LLCFI837:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1674
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1675
	 nop
.LL1674:
	sethi	%hi(108544), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1675:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -9, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE837:
	.size	bitfield__write_var_BitField_offset20_count1, .-bitfield__write_var_BitField_offset20_count1
	.align 4
	.type	bitfield__read_var_BitField_offset20_count2, #function
	.proc	04
bitfield__read_var_BitField_offset20_count2:
.LLFB838:
	save	%sp, -96, %sp
.LLCFI838:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE838:
	.size	bitfield__read_var_BitField_offset20_count2, .-bitfield__read_var_BitField_offset20_count2
	.align 4
	.type	bitfield__write_var_BitField_offset20_count2, #function
	.proc	020
bitfield__write_var_BitField_offset20_count2:
.LLFB839:
	save	%sp, -104, %sp
.LLCFI839:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1678
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1679
	 nop
.LL1678:
	sethi	%hi(108544), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1679:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -13, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE839:
	.size	bitfield__write_var_BitField_offset20_count2, .-bitfield__write_var_BitField_offset20_count2
	.align 4
	.type	bitfield__read_var_BitField_offset20_count3, #function
	.proc	04
bitfield__read_var_BitField_offset20_count3:
.LLFB840:
	save	%sp, -96, %sp
.LLCFI840:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE840:
	.size	bitfield__read_var_BitField_offset20_count3, .-bitfield__read_var_BitField_offset20_count3
	.align 4
	.type	bitfield__write_var_BitField_offset20_count3, #function
	.proc	020
bitfield__write_var_BitField_offset20_count3:
.LLFB841:
	save	%sp, -104, %sp
.LLCFI841:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1682
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1683
	 nop
.LL1682:
	sethi	%hi(108544), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1683:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, -15, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE841:
	.size	bitfield__write_var_BitField_offset20_count3, .-bitfield__write_var_BitField_offset20_count3
	.align 4
	.type	bitfield__read_var_BitField_offset20_count4, #function
	.proc	04
bitfield__read_var_BitField_offset20_count4:
.LLFB842:
	save	%sp, -96, %sp
.LLCFI842:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE842:
	.size	bitfield__read_var_BitField_offset20_count4, .-bitfield__read_var_BitField_offset20_count4
	.align 4
	.type	bitfield__write_var_BitField_offset20_count4, #function
	.proc	020
bitfield__write_var_BitField_offset20_count4:
.LLFB843:
	save	%sp, -104, %sp
.LLCFI843:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1686
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1687
	 nop
.LL1686:
	sethi	%hi(109568), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1687:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -16, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE843:
	.size	bitfield__write_var_BitField_offset20_count4, .-bitfield__write_var_BitField_offset20_count4
	.align 4
	.type	bitfield__read_var_BitField_offset20_count5, #function
	.proc	04
bitfield__read_var_BitField_offset20_count5:
.LLFB844:
	save	%sp, -96, %sp
.LLCFI844:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE844:
	.size	bitfield__read_var_BitField_offset20_count5, .-bitfield__read_var_BitField_offset20_count5
	.align 4
	.type	bitfield__write_var_BitField_offset20_count5, #function
	.proc	020
bitfield__write_var_BitField_offset20_count5:
.LLFB845:
	save	%sp, -104, %sp
.LLCFI845:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1690
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1691
	 nop
.LL1690:
	sethi	%hi(109568), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1691:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -3969, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE845:
	.size	bitfield__write_var_BitField_offset20_count5, .-bitfield__write_var_BitField_offset20_count5
	.align 4
	.type	bitfield__read_var_BitField_offset20_count6, #function
	.proc	04
bitfield__read_var_BitField_offset20_count6:
.LLFB846:
	save	%sp, -96, %sp
.LLCFI846:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE846:
	.size	bitfield__read_var_BitField_offset20_count6, .-bitfield__read_var_BitField_offset20_count6
	.align 4
	.type	bitfield__write_var_BitField_offset20_count6, #function
	.proc	020
bitfield__write_var_BitField_offset20_count6:
.LLFB847:
	save	%sp, -104, %sp
.LLCFI847:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1694
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1695
	 nop
.LL1694:
	sethi	%hi(109568), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1695:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -4033, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE847:
	.size	bitfield__write_var_BitField_offset20_count6, .-bitfield__write_var_BitField_offset20_count6
	.align 4
	.type	bitfield__read_var_BitField_offset20_count7, #function
	.proc	04
bitfield__read_var_BitField_offset20_count7:
.LLFB848:
	save	%sp, -96, %sp
.LLCFI848:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE848:
	.size	bitfield__read_var_BitField_offset20_count7, .-bitfield__read_var_BitField_offset20_count7
	.align 4
	.type	bitfield__write_var_BitField_offset20_count7, #function
	.proc	020
bitfield__write_var_BitField_offset20_count7:
.LLFB849:
	save	%sp, -104, %sp
.LLCFI849:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1698
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1699
	 nop
.LL1698:
	sethi	%hi(109568), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1699:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -4065, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE849:
	.size	bitfield__write_var_BitField_offset20_count7, .-bitfield__write_var_BitField_offset20_count7
	.align 4
	.type	bitfield__read_var_BitField_offset20_count8, #function
	.proc	04
bitfield__read_var_BitField_offset20_count8:
.LLFB850:
	save	%sp, -96, %sp
.LLCFI850:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE850:
	.size	bitfield__read_var_BitField_offset20_count8, .-bitfield__read_var_BitField_offset20_count8
	.align 4
	.type	bitfield__write_var_BitField_offset20_count8, #function
	.proc	020
bitfield__write_var_BitField_offset20_count8:
.LLFB851:
	save	%sp, -104, %sp
.LLCFI851:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1702
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1703
	 nop
.LL1702:
	sethi	%hi(110592), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1703:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -4081, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE851:
	.size	bitfield__write_var_BitField_offset20_count8, .-bitfield__write_var_BitField_offset20_count8
	.align 4
	.type	bitfield__read_var_BitField_offset20_count9, #function
	.proc	04
bitfield__read_var_BitField_offset20_count9:
.LLFB852:
	save	%sp, -96, %sp
.LLCFI852:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE852:
	.size	bitfield__read_var_BitField_offset20_count9, .-bitfield__read_var_BitField_offset20_count9
	.align 4
	.type	bitfield__write_var_BitField_offset20_count9, #function
	.proc	020
bitfield__write_var_BitField_offset20_count9:
.LLFB853:
	save	%sp, -104, %sp
.LLCFI853:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1706
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1707
	 nop
.LL1706:
	sethi	%hi(110592), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1707:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -4089, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE853:
	.size	bitfield__write_var_BitField_offset20_count9, .-bitfield__write_var_BitField_offset20_count9
	.align 4
	.type	bitfield__read_var_BitField_offset20_count10, #function
	.proc	04
bitfield__read_var_BitField_offset20_count10:
.LLFB854:
	save	%sp, -96, %sp
.LLCFI854:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE854:
	.size	bitfield__read_var_BitField_offset20_count10, .-bitfield__read_var_BitField_offset20_count10
	.align 4
	.type	bitfield__write_var_BitField_offset20_count10, #function
	.proc	020
bitfield__write_var_BitField_offset20_count10:
.LLFB855:
	save	%sp, -104, %sp
.LLCFI855:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1710
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1711
	 nop
.LL1710:
	sethi	%hi(110592), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1711:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -4093, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE855:
	.size	bitfield__write_var_BitField_offset20_count10, .-bitfield__write_var_BitField_offset20_count10
	.align 4
	.type	bitfield__read_var_BitField_offset20_count11, #function
	.proc	04
bitfield__read_var_BitField_offset20_count11:
.LLFB856:
	save	%sp, -96, %sp
.LLCFI856:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 20, %g1
	srl	%g1, 21, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE856:
	.size	bitfield__read_var_BitField_offset20_count11, .-bitfield__read_var_BitField_offset20_count11
	.align 4
	.type	bitfield__write_var_BitField_offset20_count11, #function
	.proc	020
bitfield__write_var_BitField_offset20_count11:
.LLFB857:
	save	%sp, -104, %sp
.LLCFI857:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1714
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1715
	 nop
.LL1714:
	sethi	%hi(110592), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1715:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	and	%g1, -4095, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 2047, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE857:
	.size	bitfield__write_var_BitField_offset20_count11, .-bitfield__write_var_BitField_offset20_count11
	.align 4
	.type	bitfield__read_var_BitField_offset20_count12, #function
	.proc	04
bitfield__read_var_BitField_offset20_count12:
.LLFB858:
	save	%sp, -96, %sp
.LLCFI858:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 4095, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE858:
	.size	bitfield__read_var_BitField_offset20_count12, .-bitfield__read_var_BitField_offset20_count12
	.align 4
	.type	bitfield__write_var_BitField_offset20_count12, #function
	.proc	020
bitfield__write_var_BitField_offset20_count12:
.LLFB859:
	save	%sp, -104, %sp
.LLCFI859:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1718
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 4095
	ble	%icc, .LL1719
	 nop
.LL1718:
	sethi	%hi(111616), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1719:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -4096, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 4095, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE859:
	.size	bitfield__write_var_BitField_offset20_count12, .-bitfield__write_var_BitField_offset20_count12
	.align 4
	.type	bitfield__read_var_BitField_offset21_count1, #function
	.proc	04
bitfield__read_var_BitField_offset21_count1:
.LLFB860:
	save	%sp, -96, %sp
.LLCFI860:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE860:
	.size	bitfield__read_var_BitField_offset21_count1, .-bitfield__read_var_BitField_offset21_count1
	.align 4
	.type	bitfield__write_var_BitField_offset21_count1, #function
	.proc	020
bitfield__write_var_BitField_offset21_count1:
.LLFB861:
	save	%sp, -104, %sp
.LLCFI861:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1722
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1723
	 nop
.LL1722:
	sethi	%hi(111616), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1723:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE861:
	.size	bitfield__write_var_BitField_offset21_count1, .-bitfield__write_var_BitField_offset21_count1
	.align 4
	.type	bitfield__read_var_BitField_offset21_count2, #function
	.proc	04
bitfield__read_var_BitField_offset21_count2:
.LLFB862:
	save	%sp, -96, %sp
.LLCFI862:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE862:
	.size	bitfield__read_var_BitField_offset21_count2, .-bitfield__read_var_BitField_offset21_count2
	.align 4
	.type	bitfield__write_var_BitField_offset21_count2, #function
	.proc	020
bitfield__write_var_BitField_offset21_count2:
.LLFB863:
	save	%sp, -104, %sp
.LLCFI863:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1726
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1727
	 nop
.LL1726:
	sethi	%hi(111616), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1727:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, -7, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE863:
	.size	bitfield__write_var_BitField_offset21_count2, .-bitfield__write_var_BitField_offset21_count2
	.align 4
	.type	bitfield__read_var_BitField_offset21_count3, #function
	.proc	04
bitfield__read_var_BitField_offset21_count3:
.LLFB864:
	save	%sp, -96, %sp
.LLCFI864:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE864:
	.size	bitfield__read_var_BitField_offset21_count3, .-bitfield__read_var_BitField_offset21_count3
	.align 4
	.type	bitfield__write_var_BitField_offset21_count3, #function
	.proc	020
bitfield__write_var_BitField_offset21_count3:
.LLFB865:
	save	%sp, -104, %sp
.LLCFI865:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1730
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1731
	 nop
.LL1730:
	sethi	%hi(111616), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1731:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -8, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE865:
	.size	bitfield__write_var_BitField_offset21_count3, .-bitfield__write_var_BitField_offset21_count3
	.align 4
	.type	bitfield__read_var_BitField_offset21_count4, #function
	.proc	04
bitfield__read_var_BitField_offset21_count4:
.LLFB866:
	save	%sp, -96, %sp
.LLCFI866:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE866:
	.size	bitfield__read_var_BitField_offset21_count4, .-bitfield__read_var_BitField_offset21_count4
	.align 4
	.type	bitfield__write_var_BitField_offset21_count4, #function
	.proc	020
bitfield__write_var_BitField_offset21_count4:
.LLFB867:
	save	%sp, -104, %sp
.LLCFI867:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1734
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1735
	 nop
.LL1734:
	sethi	%hi(112640), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1735:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -1921, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE867:
	.size	bitfield__write_var_BitField_offset21_count4, .-bitfield__write_var_BitField_offset21_count4
	.align 4
	.type	bitfield__read_var_BitField_offset21_count5, #function
	.proc	04
bitfield__read_var_BitField_offset21_count5:
.LLFB868:
	save	%sp, -96, %sp
.LLCFI868:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE868:
	.size	bitfield__read_var_BitField_offset21_count5, .-bitfield__read_var_BitField_offset21_count5
	.align 4
	.type	bitfield__write_var_BitField_offset21_count5, #function
	.proc	020
bitfield__write_var_BitField_offset21_count5:
.LLFB869:
	save	%sp, -104, %sp
.LLCFI869:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1738
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1739
	 nop
.LL1738:
	sethi	%hi(112640), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1739:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -1985, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE869:
	.size	bitfield__write_var_BitField_offset21_count5, .-bitfield__write_var_BitField_offset21_count5
	.align 4
	.type	bitfield__read_var_BitField_offset21_count6, #function
	.proc	04
bitfield__read_var_BitField_offset21_count6:
.LLFB870:
	save	%sp, -96, %sp
.LLCFI870:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE870:
	.size	bitfield__read_var_BitField_offset21_count6, .-bitfield__read_var_BitField_offset21_count6
	.align 4
	.type	bitfield__write_var_BitField_offset21_count6, #function
	.proc	020
bitfield__write_var_BitField_offset21_count6:
.LLFB871:
	save	%sp, -104, %sp
.LLCFI871:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1742
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1743
	 nop
.LL1742:
	sethi	%hi(112640), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1743:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -2017, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE871:
	.size	bitfield__write_var_BitField_offset21_count6, .-bitfield__write_var_BitField_offset21_count6
	.align 4
	.type	bitfield__read_var_BitField_offset21_count7, #function
	.proc	04
bitfield__read_var_BitField_offset21_count7:
.LLFB872:
	save	%sp, -96, %sp
.LLCFI872:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE872:
	.size	bitfield__read_var_BitField_offset21_count7, .-bitfield__read_var_BitField_offset21_count7
	.align 4
	.type	bitfield__write_var_BitField_offset21_count7, #function
	.proc	020
bitfield__write_var_BitField_offset21_count7:
.LLFB873:
	save	%sp, -104, %sp
.LLCFI873:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1746
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1747
	 nop
.LL1746:
	sethi	%hi(112640), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1747:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -2033, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE873:
	.size	bitfield__write_var_BitField_offset21_count7, .-bitfield__write_var_BitField_offset21_count7
	.align 4
	.type	bitfield__read_var_BitField_offset21_count8, #function
	.proc	04
bitfield__read_var_BitField_offset21_count8:
.LLFB874:
	save	%sp, -96, %sp
.LLCFI874:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE874:
	.size	bitfield__read_var_BitField_offset21_count8, .-bitfield__read_var_BitField_offset21_count8
	.align 4
	.type	bitfield__write_var_BitField_offset21_count8, #function
	.proc	020
bitfield__write_var_BitField_offset21_count8:
.LLFB875:
	save	%sp, -104, %sp
.LLCFI875:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1750
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1751
	 nop
.LL1750:
	sethi	%hi(113664), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1751:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -2041, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE875:
	.size	bitfield__write_var_BitField_offset21_count8, .-bitfield__write_var_BitField_offset21_count8
	.align 4
	.type	bitfield__read_var_BitField_offset21_count9, #function
	.proc	04
bitfield__read_var_BitField_offset21_count9:
.LLFB876:
	save	%sp, -96, %sp
.LLCFI876:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE876:
	.size	bitfield__read_var_BitField_offset21_count9, .-bitfield__read_var_BitField_offset21_count9
	.align 4
	.type	bitfield__write_var_BitField_offset21_count9, #function
	.proc	020
bitfield__write_var_BitField_offset21_count9:
.LLFB877:
	save	%sp, -104, %sp
.LLCFI877:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1754
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1755
	 nop
.LL1754:
	sethi	%hi(113664), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1755:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -2045, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE877:
	.size	bitfield__write_var_BitField_offset21_count9, .-bitfield__write_var_BitField_offset21_count9
	.align 4
	.type	bitfield__read_var_BitField_offset21_count10, #function
	.proc	04
bitfield__read_var_BitField_offset21_count10:
.LLFB878:
	save	%sp, -96, %sp
.LLCFI878:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 21, %g1
	srl	%g1, 22, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE878:
	.size	bitfield__read_var_BitField_offset21_count10, .-bitfield__read_var_BitField_offset21_count10
	.align 4
	.type	bitfield__write_var_BitField_offset21_count10, #function
	.proc	020
bitfield__write_var_BitField_offset21_count10:
.LLFB879:
	save	%sp, -104, %sp
.LLCFI879:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1758
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1759
	 nop
.LL1758:
	sethi	%hi(113664), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1759:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	and	%g1, -2047, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 1023, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE879:
	.size	bitfield__write_var_BitField_offset21_count10, .-bitfield__write_var_BitField_offset21_count10
	.align 4
	.type	bitfield__read_var_BitField_offset21_count11, #function
	.proc	04
bitfield__read_var_BitField_offset21_count11:
.LLFB880:
	save	%sp, -96, %sp
.LLCFI880:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 2047, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE880:
	.size	bitfield__read_var_BitField_offset21_count11, .-bitfield__read_var_BitField_offset21_count11
	.align 4
	.type	bitfield__write_var_BitField_offset21_count11, #function
	.proc	020
bitfield__write_var_BitField_offset21_count11:
.LLFB881:
	save	%sp, -104, %sp
.LLCFI881:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1762
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 2047
	ble	%icc, .LL1763
	 nop
.LL1762:
	sethi	%hi(113664), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1763:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -2048, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 2047, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE881:
	.size	bitfield__write_var_BitField_offset21_count11, .-bitfield__write_var_BitField_offset21_count11
	.align 4
	.type	bitfield__read_var_BitField_offset22_count1, #function
	.proc	04
bitfield__read_var_BitField_offset22_count1:
.LLFB882:
	save	%sp, -96, %sp
.LLCFI882:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE882:
	.size	bitfield__read_var_BitField_offset22_count1, .-bitfield__read_var_BitField_offset22_count1
	.align 4
	.type	bitfield__write_var_BitField_offset22_count1, #function
	.proc	020
bitfield__write_var_BitField_offset22_count1:
.LLFB883:
	save	%sp, -104, %sp
.LLCFI883:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1766
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1767
	 nop
.LL1766:
	sethi	%hi(114688), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1767:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ldub	[%g1], %g1
	and	%g1, -3, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE883:
	.size	bitfield__write_var_BitField_offset22_count1, .-bitfield__write_var_BitField_offset22_count1
	.align 4
	.type	bitfield__read_var_BitField_offset22_count2, #function
	.proc	04
bitfield__read_var_BitField_offset22_count2:
.LLFB884:
	save	%sp, -96, %sp
.LLCFI884:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE884:
	.size	bitfield__read_var_BitField_offset22_count2, .-bitfield__read_var_BitField_offset22_count2
	.align 4
	.type	bitfield__write_var_BitField_offset22_count2, #function
	.proc	020
bitfield__write_var_BitField_offset22_count2:
.LLFB885:
	save	%sp, -104, %sp
.LLCFI885:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1770
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1771
	 nop
.LL1770:
	sethi	%hi(114688), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1771:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -4, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE885:
	.size	bitfield__write_var_BitField_offset22_count2, .-bitfield__write_var_BitField_offset22_count2
	.align 4
	.type	bitfield__read_var_BitField_offset22_count3, #function
	.proc	04
bitfield__read_var_BitField_offset22_count3:
.LLFB886:
	save	%sp, -96, %sp
.LLCFI886:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE886:
	.size	bitfield__read_var_BitField_offset22_count3, .-bitfield__read_var_BitField_offset22_count3
	.align 4
	.type	bitfield__write_var_BitField_offset22_count3, #function
	.proc	020
bitfield__write_var_BitField_offset22_count3:
.LLFB887:
	save	%sp, -104, %sp
.LLCFI887:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1774
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1775
	 nop
.LL1774:
	sethi	%hi(114688), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1775:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -897, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE887:
	.size	bitfield__write_var_BitField_offset22_count3, .-bitfield__write_var_BitField_offset22_count3
	.align 4
	.type	bitfield__read_var_BitField_offset22_count4, #function
	.proc	04
bitfield__read_var_BitField_offset22_count4:
.LLFB888:
	save	%sp, -96, %sp
.LLCFI888:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE888:
	.size	bitfield__read_var_BitField_offset22_count4, .-bitfield__read_var_BitField_offset22_count4
	.align 4
	.type	bitfield__write_var_BitField_offset22_count4, #function
	.proc	020
bitfield__write_var_BitField_offset22_count4:
.LLFB889:
	save	%sp, -104, %sp
.LLCFI889:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1778
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1779
	 nop
.LL1778:
	sethi	%hi(114688), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1779:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -961, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE889:
	.size	bitfield__write_var_BitField_offset22_count4, .-bitfield__write_var_BitField_offset22_count4
	.align 4
	.type	bitfield__read_var_BitField_offset22_count5, #function
	.proc	04
bitfield__read_var_BitField_offset22_count5:
.LLFB890:
	save	%sp, -96, %sp
.LLCFI890:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE890:
	.size	bitfield__read_var_BitField_offset22_count5, .-bitfield__read_var_BitField_offset22_count5
	.align 4
	.type	bitfield__write_var_BitField_offset22_count5, #function
	.proc	020
bitfield__write_var_BitField_offset22_count5:
.LLFB891:
	save	%sp, -104, %sp
.LLCFI891:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1782
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1783
	 nop
.LL1782:
	sethi	%hi(115712), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1783:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -993, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE891:
	.size	bitfield__write_var_BitField_offset22_count5, .-bitfield__write_var_BitField_offset22_count5
	.align 4
	.type	bitfield__read_var_BitField_offset22_count6, #function
	.proc	04
bitfield__read_var_BitField_offset22_count6:
.LLFB892:
	save	%sp, -96, %sp
.LLCFI892:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE892:
	.size	bitfield__read_var_BitField_offset22_count6, .-bitfield__read_var_BitField_offset22_count6
	.align 4
	.type	bitfield__write_var_BitField_offset22_count6, #function
	.proc	020
bitfield__write_var_BitField_offset22_count6:
.LLFB893:
	save	%sp, -104, %sp
.LLCFI893:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1786
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1787
	 nop
.LL1786:
	sethi	%hi(115712), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1787:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -1009, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE893:
	.size	bitfield__write_var_BitField_offset22_count6, .-bitfield__write_var_BitField_offset22_count6
	.align 4
	.type	bitfield__read_var_BitField_offset22_count7, #function
	.proc	04
bitfield__read_var_BitField_offset22_count7:
.LLFB894:
	save	%sp, -96, %sp
.LLCFI894:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE894:
	.size	bitfield__read_var_BitField_offset22_count7, .-bitfield__read_var_BitField_offset22_count7
	.align 4
	.type	bitfield__write_var_BitField_offset22_count7, #function
	.proc	020
bitfield__write_var_BitField_offset22_count7:
.LLFB895:
	save	%sp, -104, %sp
.LLCFI895:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1790
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1791
	 nop
.LL1790:
	sethi	%hi(115712), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1791:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -1017, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE895:
	.size	bitfield__write_var_BitField_offset22_count7, .-bitfield__write_var_BitField_offset22_count7
	.align 4
	.type	bitfield__read_var_BitField_offset22_count8, #function
	.proc	04
bitfield__read_var_BitField_offset22_count8:
.LLFB896:
	save	%sp, -96, %sp
.LLCFI896:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE896:
	.size	bitfield__read_var_BitField_offset22_count8, .-bitfield__read_var_BitField_offset22_count8
	.align 4
	.type	bitfield__write_var_BitField_offset22_count8, #function
	.proc	020
bitfield__write_var_BitField_offset22_count8:
.LLFB897:
	save	%sp, -104, %sp
.LLCFI897:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1794
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1795
	 nop
.LL1794:
	sethi	%hi(115712), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1795:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -1021, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 255, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE897:
	.size	bitfield__write_var_BitField_offset22_count8, .-bitfield__write_var_BitField_offset22_count8
	.align 4
	.type	bitfield__read_var_BitField_offset22_count9, #function
	.proc	04
bitfield__read_var_BitField_offset22_count9:
.LLFB898:
	save	%sp, -96, %sp
.LLCFI898:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 22, %g1
	srl	%g1, 23, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE898:
	.size	bitfield__read_var_BitField_offset22_count9, .-bitfield__read_var_BitField_offset22_count9
	.align 4
	.type	bitfield__write_var_BitField_offset22_count9, #function
	.proc	020
bitfield__write_var_BitField_offset22_count9:
.LLFB899:
	save	%sp, -104, %sp
.LLCFI899:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1798
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1799
	 nop
.LL1798:
	sethi	%hi(116736), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1799:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	and	%g1, -1023, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 511, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE899:
	.size	bitfield__write_var_BitField_offset22_count9, .-bitfield__write_var_BitField_offset22_count9
	.align 4
	.type	bitfield__read_var_BitField_offset22_count10, #function
	.proc	04
bitfield__read_var_BitField_offset22_count10:
.LLFB900:
	save	%sp, -96, %sp
.LLCFI900:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 1023, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE900:
	.size	bitfield__read_var_BitField_offset22_count10, .-bitfield__read_var_BitField_offset22_count10
	.align 4
	.type	bitfield__write_var_BitField_offset22_count10, #function
	.proc	020
bitfield__write_var_BitField_offset22_count10:
.LLFB901:
	save	%sp, -104, %sp
.LLCFI901:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1802
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1023
	ble	%icc, .LL1803
	 nop
.LL1802:
	sethi	%hi(116736), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1803:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -1024, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1023, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE901:
	.size	bitfield__write_var_BitField_offset22_count10, .-bitfield__write_var_BitField_offset22_count10
	.align 4
	.type	bitfield__read_var_BitField_offset23_count1, #function
	.proc	04
bitfield__read_var_BitField_offset23_count1:
.LLFB902:
	save	%sp, -96, %sp
.LLCFI902:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE902:
	.size	bitfield__read_var_BitField_offset23_count1, .-bitfield__read_var_BitField_offset23_count1
	.align 4
	.type	bitfield__write_var_BitField_offset23_count1, #function
	.proc	020
bitfield__write_var_BitField_offset23_count1:
.LLFB903:
	save	%sp, -104, %sp
.LLCFI903:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1806
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1807
	 nop
.LL1806:
	sethi	%hi(116736), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1807:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	ldub	[%o5], %o5
	and	%o5, -2, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE903:
	.size	bitfield__write_var_BitField_offset23_count1, .-bitfield__write_var_BitField_offset23_count1
	.align 4
	.type	bitfield__read_var_BitField_offset23_count2, #function
	.proc	04
bitfield__read_var_BitField_offset23_count2:
.LLFB904:
	save	%sp, -96, %sp
.LLCFI904:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE904:
	.size	bitfield__read_var_BitField_offset23_count2, .-bitfield__read_var_BitField_offset23_count2
	.align 4
	.type	bitfield__write_var_BitField_offset23_count2, #function
	.proc	020
bitfield__write_var_BitField_offset23_count2:
.LLFB905:
	save	%sp, -104, %sp
.LLCFI905:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1810
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1811
	 nop
.LL1810:
	sethi	%hi(116736), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1811:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -385, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE905:
	.size	bitfield__write_var_BitField_offset23_count2, .-bitfield__write_var_BitField_offset23_count2
	.align 4
	.type	bitfield__read_var_BitField_offset23_count3, #function
	.proc	04
bitfield__read_var_BitField_offset23_count3:
.LLFB906:
	save	%sp, -96, %sp
.LLCFI906:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE906:
	.size	bitfield__read_var_BitField_offset23_count3, .-bitfield__read_var_BitField_offset23_count3
	.align 4
	.type	bitfield__write_var_BitField_offset23_count3, #function
	.proc	020
bitfield__write_var_BitField_offset23_count3:
.LLFB907:
	save	%sp, -104, %sp
.LLCFI907:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1814
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1815
	 nop
.LL1814:
	sethi	%hi(117760), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1815:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -449, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE907:
	.size	bitfield__write_var_BitField_offset23_count3, .-bitfield__write_var_BitField_offset23_count3
	.align 4
	.type	bitfield__read_var_BitField_offset23_count4, #function
	.proc	04
bitfield__read_var_BitField_offset23_count4:
.LLFB908:
	save	%sp, -96, %sp
.LLCFI908:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE908:
	.size	bitfield__read_var_BitField_offset23_count4, .-bitfield__read_var_BitField_offset23_count4
	.align 4
	.type	bitfield__write_var_BitField_offset23_count4, #function
	.proc	020
bitfield__write_var_BitField_offset23_count4:
.LLFB909:
	save	%sp, -104, %sp
.LLCFI909:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1818
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1819
	 nop
.LL1818:
	sethi	%hi(117760), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1819:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -481, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE909:
	.size	bitfield__write_var_BitField_offset23_count4, .-bitfield__write_var_BitField_offset23_count4
	.align 4
	.type	bitfield__read_var_BitField_offset23_count5, #function
	.proc	04
bitfield__read_var_BitField_offset23_count5:
.LLFB910:
	save	%sp, -96, %sp
.LLCFI910:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE910:
	.size	bitfield__read_var_BitField_offset23_count5, .-bitfield__read_var_BitField_offset23_count5
	.align 4
	.type	bitfield__write_var_BitField_offset23_count5, #function
	.proc	020
bitfield__write_var_BitField_offset23_count5:
.LLFB911:
	save	%sp, -104, %sp
.LLCFI911:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1822
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1823
	 nop
.LL1822:
	sethi	%hi(117760), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1823:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -497, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE911:
	.size	bitfield__write_var_BitField_offset23_count5, .-bitfield__write_var_BitField_offset23_count5
	.align 4
	.type	bitfield__read_var_BitField_offset23_count6, #function
	.proc	04
bitfield__read_var_BitField_offset23_count6:
.LLFB912:
	save	%sp, -96, %sp
.LLCFI912:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE912:
	.size	bitfield__read_var_BitField_offset23_count6, .-bitfield__read_var_BitField_offset23_count6
	.align 4
	.type	bitfield__write_var_BitField_offset23_count6, #function
	.proc	020
bitfield__write_var_BitField_offset23_count6:
.LLFB913:
	save	%sp, -104, %sp
.LLCFI913:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1826
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1827
	 nop
.LL1826:
	sethi	%hi(117760), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1827:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -505, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE913:
	.size	bitfield__write_var_BitField_offset23_count6, .-bitfield__write_var_BitField_offset23_count6
	.align 4
	.type	bitfield__read_var_BitField_offset23_count7, #function
	.proc	04
bitfield__read_var_BitField_offset23_count7:
.LLFB914:
	save	%sp, -96, %sp
.LLCFI914:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE914:
	.size	bitfield__read_var_BitField_offset23_count7, .-bitfield__read_var_BitField_offset23_count7
	.align 4
	.type	bitfield__write_var_BitField_offset23_count7, #function
	.proc	020
bitfield__write_var_BitField_offset23_count7:
.LLFB915:
	save	%sp, -104, %sp
.LLCFI915:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1830
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1831
	 nop
.LL1830:
	sethi	%hi(118784), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1831:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -509, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE915:
	.size	bitfield__write_var_BitField_offset23_count7, .-bitfield__write_var_BitField_offset23_count7
	.align 4
	.type	bitfield__read_var_BitField_offset23_count8, #function
	.proc	04
bitfield__read_var_BitField_offset23_count8:
.LLFB916:
	save	%sp, -96, %sp
.LLCFI916:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sll	%g1, 23, %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE916:
	.size	bitfield__read_var_BitField_offset23_count8, .-bitfield__read_var_BitField_offset23_count8
	.align 4
	.type	bitfield__write_var_BitField_offset23_count8, #function
	.proc	020
bitfield__write_var_BitField_offset23_count8:
.LLFB917:
	save	%sp, -104, %sp
.LLCFI917:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1834
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1835
	 nop
.LL1834:
	sethi	%hi(118784), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1835:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %o5
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	lduh	[%g1], %g1
	and	%g1, -511, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 255, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	sth	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE917:
	.size	bitfield__write_var_BitField_offset23_count8, .-bitfield__write_var_BitField_offset23_count8
	.align 4
	.type	bitfield__read_var_BitField_offset23_count9, #function
	.proc	04
bitfield__read_var_BitField_offset23_count9:
.LLFB918:
	save	%sp, -96, %sp
.LLCFI918:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	and	%g1, 511, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE918:
	.size	bitfield__read_var_BitField_offset23_count9, .-bitfield__read_var_BitField_offset23_count9
	.align 4
	.type	bitfield__write_var_BitField_offset23_count9, #function
	.proc	020
bitfield__write_var_BitField_offset23_count9:
.LLFB919:
	save	%sp, -104, %sp
.LLCFI919:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1838
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 511
	ble	%icc, .LL1839
	 nop
.LL1838:
	sethi	%hi(118784), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1839:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 2, %g1
	ld	[%fp-4], %o5
	add	%o5, 2, %o5
	lduh	[%o5], %o5
	and	%o5, -512, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 511, %o5
	or	%o4, %o5, %o5
	sth	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE919:
	.size	bitfield__write_var_BitField_offset23_count9, .-bitfield__write_var_BitField_offset23_count9
	.align 4
	.type	bitfield__read_var_BitField_offset24_count1, #function
	.proc	04
bitfield__read_var_BitField_offset24_count1:
.LLFB920:
	save	%sp, -96, %sp
.LLCFI920:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE920:
	.size	bitfield__read_var_BitField_offset24_count1, .-bitfield__read_var_BitField_offset24_count1
	.align 4
	.type	bitfield__write_var_BitField_offset24_count1, #function
	.proc	020
bitfield__write_var_BitField_offset24_count1:
.LLFB921:
	save	%sp, -104, %sp
.LLCFI921:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1842
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1843
	 nop
.LL1842:
	sethi	%hi(118784), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1843:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, 127, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE921:
	.size	bitfield__write_var_BitField_offset24_count1, .-bitfield__write_var_BitField_offset24_count1
	.align 4
	.type	bitfield__read_var_BitField_offset24_count2, #function
	.proc	04
bitfield__read_var_BitField_offset24_count2:
.LLFB922:
	save	%sp, -96, %sp
.LLCFI922:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE922:
	.size	bitfield__read_var_BitField_offset24_count2, .-bitfield__read_var_BitField_offset24_count2
	.align 4
	.type	bitfield__write_var_BitField_offset24_count2, #function
	.proc	020
bitfield__write_var_BitField_offset24_count2:
.LLFB923:
	save	%sp, -104, %sp
.LLCFI923:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1846
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1847
	 nop
.LL1846:
	sethi	%hi(119808), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1847:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, 63, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE923:
	.size	bitfield__write_var_BitField_offset24_count2, .-bitfield__write_var_BitField_offset24_count2
	.align 4
	.type	bitfield__read_var_BitField_offset24_count3, #function
	.proc	04
bitfield__read_var_BitField_offset24_count3:
.LLFB924:
	save	%sp, -96, %sp
.LLCFI924:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE924:
	.size	bitfield__read_var_BitField_offset24_count3, .-bitfield__read_var_BitField_offset24_count3
	.align 4
	.type	bitfield__write_var_BitField_offset24_count3, #function
	.proc	020
bitfield__write_var_BitField_offset24_count3:
.LLFB925:
	save	%sp, -104, %sp
.LLCFI925:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1850
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1851
	 nop
.LL1850:
	sethi	%hi(119808), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1851:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, 31, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE925:
	.size	bitfield__write_var_BitField_offset24_count3, .-bitfield__write_var_BitField_offset24_count3
	.align 4
	.type	bitfield__read_var_BitField_offset24_count4, #function
	.proc	04
bitfield__read_var_BitField_offset24_count4:
.LLFB926:
	save	%sp, -96, %sp
.LLCFI926:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE926:
	.size	bitfield__read_var_BitField_offset24_count4, .-bitfield__read_var_BitField_offset24_count4
	.align 4
	.type	bitfield__write_var_BitField_offset24_count4, #function
	.proc	020
bitfield__write_var_BitField_offset24_count4:
.LLFB927:
	save	%sp, -104, %sp
.LLCFI927:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1854
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1855
	 nop
.LL1854:
	sethi	%hi(119808), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1855:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, 15, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE927:
	.size	bitfield__write_var_BitField_offset24_count4, .-bitfield__write_var_BitField_offset24_count4
	.align 4
	.type	bitfield__read_var_BitField_offset24_count5, #function
	.proc	04
bitfield__read_var_BitField_offset24_count5:
.LLFB928:
	save	%sp, -96, %sp
.LLCFI928:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE928:
	.size	bitfield__read_var_BitField_offset24_count5, .-bitfield__read_var_BitField_offset24_count5
	.align 4
	.type	bitfield__write_var_BitField_offset24_count5, #function
	.proc	020
bitfield__write_var_BitField_offset24_count5:
.LLFB929:
	save	%sp, -104, %sp
.LLCFI929:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1858
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1859
	 nop
.LL1858:
	sethi	%hi(119808), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1859:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, 7, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE929:
	.size	bitfield__write_var_BitField_offset24_count5, .-bitfield__write_var_BitField_offset24_count5
	.align 4
	.type	bitfield__read_var_BitField_offset24_count6, #function
	.proc	04
bitfield__read_var_BitField_offset24_count6:
.LLFB930:
	save	%sp, -96, %sp
.LLCFI930:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE930:
	.size	bitfield__read_var_BitField_offset24_count6, .-bitfield__read_var_BitField_offset24_count6
	.align 4
	.type	bitfield__write_var_BitField_offset24_count6, #function
	.proc	020
bitfield__write_var_BitField_offset24_count6:
.LLFB931:
	save	%sp, -104, %sp
.LLCFI931:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1862
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1863
	 nop
.LL1862:
	sethi	%hi(120832), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1863:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, 3, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE931:
	.size	bitfield__write_var_BitField_offset24_count6, .-bitfield__write_var_BitField_offset24_count6
	.align 4
	.type	bitfield__read_var_BitField_offset24_count7, #function
	.proc	04
bitfield__read_var_BitField_offset24_count7:
.LLFB932:
	save	%sp, -96, %sp
.LLCFI932:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 24, %g1
	srl	%g1, 25, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE932:
	.size	bitfield__read_var_BitField_offset24_count7, .-bitfield__read_var_BitField_offset24_count7
	.align 4
	.type	bitfield__write_var_BitField_offset24_count7, #function
	.proc	020
bitfield__write_var_BitField_offset24_count7:
.LLFB933:
	save	%sp, -104, %sp
.LLCFI933:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1866
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1867
	 nop
.LL1866:
	sethi	%hi(120832), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1867:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, 1, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE933:
	.size	bitfield__write_var_BitField_offset24_count7, .-bitfield__write_var_BitField_offset24_count7
	.align 4
	.type	bitfield__read_var_BitField_offset24_count8, #function
	.proc	04
bitfield__read_var_BitField_offset24_count8:
.LLFB934:
	save	%sp, -96, %sp
.LLCFI934:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE934:
	.size	bitfield__read_var_BitField_offset24_count8, .-bitfield__read_var_BitField_offset24_count8
	.align 4
	.type	bitfield__write_var_BitField_offset24_count8, #function
	.proc	020
bitfield__write_var_BitField_offset24_count8:
.LLFB935:
	save	%sp, -96, %sp
.LLCFI935:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1870
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 255
	ble	%icc, .LL1871
	 nop
.LL1870:
	sethi	%hi(120832), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1871:
	ld	[%fp+68], %g1
	add	%g1, 3, %g1
	ld	[%fp+72], %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE935:
	.size	bitfield__write_var_BitField_offset24_count8, .-bitfield__write_var_BitField_offset24_count8
	.align 4
	.type	bitfield__read_var_BitField_offset25_count1, #function
	.proc	04
bitfield__read_var_BitField_offset25_count1:
.LLFB936:
	save	%sp, -96, %sp
.LLCFI936:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE936:
	.size	bitfield__read_var_BitField_offset25_count1, .-bitfield__read_var_BitField_offset25_count1
	.align 4
	.type	bitfield__write_var_BitField_offset25_count1, #function
	.proc	020
bitfield__write_var_BitField_offset25_count1:
.LLFB937:
	save	%sp, -104, %sp
.LLCFI937:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1874
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1875
	 nop
.LL1874:
	sethi	%hi(120832), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1875:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -65, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 6, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE937:
	.size	bitfield__write_var_BitField_offset25_count1, .-bitfield__write_var_BitField_offset25_count1
	.align 4
	.type	bitfield__read_var_BitField_offset25_count2, #function
	.proc	04
bitfield__read_var_BitField_offset25_count2:
.LLFB938:
	save	%sp, -96, %sp
.LLCFI938:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE938:
	.size	bitfield__read_var_BitField_offset25_count2, .-bitfield__read_var_BitField_offset25_count2
	.align 4
	.type	bitfield__write_var_BitField_offset25_count2, #function
	.proc	020
bitfield__write_var_BitField_offset25_count2:
.LLFB939:
	save	%sp, -104, %sp
.LLCFI939:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1878
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1879
	 nop
.LL1878:
	sethi	%hi(121856), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1879:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -97, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE939:
	.size	bitfield__write_var_BitField_offset25_count2, .-bitfield__write_var_BitField_offset25_count2
	.align 4
	.type	bitfield__read_var_BitField_offset25_count3, #function
	.proc	04
bitfield__read_var_BitField_offset25_count3:
.LLFB940:
	save	%sp, -96, %sp
.LLCFI940:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE940:
	.size	bitfield__read_var_BitField_offset25_count3, .-bitfield__read_var_BitField_offset25_count3
	.align 4
	.type	bitfield__write_var_BitField_offset25_count3, #function
	.proc	020
bitfield__write_var_BitField_offset25_count3:
.LLFB941:
	save	%sp, -104, %sp
.LLCFI941:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1882
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1883
	 nop
.LL1882:
	sethi	%hi(121856), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1883:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -113, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE941:
	.size	bitfield__write_var_BitField_offset25_count3, .-bitfield__write_var_BitField_offset25_count3
	.align 4
	.type	bitfield__read_var_BitField_offset25_count4, #function
	.proc	04
bitfield__read_var_BitField_offset25_count4:
.LLFB942:
	save	%sp, -96, %sp
.LLCFI942:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE942:
	.size	bitfield__read_var_BitField_offset25_count4, .-bitfield__read_var_BitField_offset25_count4
	.align 4
	.type	bitfield__write_var_BitField_offset25_count4, #function
	.proc	020
bitfield__write_var_BitField_offset25_count4:
.LLFB943:
	save	%sp, -104, %sp
.LLCFI943:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1886
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1887
	 nop
.LL1886:
	sethi	%hi(121856), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1887:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -121, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE943:
	.size	bitfield__write_var_BitField_offset25_count4, .-bitfield__write_var_BitField_offset25_count4
	.align 4
	.type	bitfield__read_var_BitField_offset25_count5, #function
	.proc	04
bitfield__read_var_BitField_offset25_count5:
.LLFB944:
	save	%sp, -96, %sp
.LLCFI944:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE944:
	.size	bitfield__read_var_BitField_offset25_count5, .-bitfield__read_var_BitField_offset25_count5
	.align 4
	.type	bitfield__write_var_BitField_offset25_count5, #function
	.proc	020
bitfield__write_var_BitField_offset25_count5:
.LLFB945:
	save	%sp, -104, %sp
.LLCFI945:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1890
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1891
	 nop
.LL1890:
	sethi	%hi(121856), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1891:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -125, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE945:
	.size	bitfield__write_var_BitField_offset25_count5, .-bitfield__write_var_BitField_offset25_count5
	.align 4
	.type	bitfield__read_var_BitField_offset25_count6, #function
	.proc	04
bitfield__read_var_BitField_offset25_count6:
.LLFB946:
	save	%sp, -96, %sp
.LLCFI946:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 25, %g1
	srl	%g1, 26, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE946:
	.size	bitfield__read_var_BitField_offset25_count6, .-bitfield__read_var_BitField_offset25_count6
	.align 4
	.type	bitfield__write_var_BitField_offset25_count6, #function
	.proc	020
bitfield__write_var_BitField_offset25_count6:
.LLFB947:
	save	%sp, -104, %sp
.LLCFI947:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1894
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1895
	 nop
.LL1894:
	sethi	%hi(122880), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1895:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, -127, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 63, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE947:
	.size	bitfield__write_var_BitField_offset25_count6, .-bitfield__write_var_BitField_offset25_count6
	.align 4
	.type	bitfield__read_var_BitField_offset25_count7, #function
	.proc	04
bitfield__read_var_BitField_offset25_count7:
.LLFB948:
	save	%sp, -96, %sp
.LLCFI948:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 127, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE948:
	.size	bitfield__read_var_BitField_offset25_count7, .-bitfield__read_var_BitField_offset25_count7
	.align 4
	.type	bitfield__write_var_BitField_offset25_count7, #function
	.proc	020
bitfield__write_var_BitField_offset25_count7:
.LLFB949:
	save	%sp, -104, %sp
.LLCFI949:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1898
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 127
	ble	%icc, .LL1899
	 nop
.LL1898:
	sethi	%hi(122880), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1899:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -128, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 127, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE949:
	.size	bitfield__write_var_BitField_offset25_count7, .-bitfield__write_var_BitField_offset25_count7
	.align 4
	.type	bitfield__read_var_BitField_offset26_count1, #function
	.proc	04
bitfield__read_var_BitField_offset26_count1:
.LLFB950:
	save	%sp, -96, %sp
.LLCFI950:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE950:
	.size	bitfield__read_var_BitField_offset26_count1, .-bitfield__read_var_BitField_offset26_count1
	.align 4
	.type	bitfield__write_var_BitField_offset26_count1, #function
	.proc	020
bitfield__write_var_BitField_offset26_count1:
.LLFB951:
	save	%sp, -104, %sp
.LLCFI951:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1902
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1903
	 nop
.LL1902:
	sethi	%hi(122880), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1903:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -33, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 5, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE951:
	.size	bitfield__write_var_BitField_offset26_count1, .-bitfield__write_var_BitField_offset26_count1
	.align 4
	.type	bitfield__read_var_BitField_offset26_count2, #function
	.proc	04
bitfield__read_var_BitField_offset26_count2:
.LLFB952:
	save	%sp, -96, %sp
.LLCFI952:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE952:
	.size	bitfield__read_var_BitField_offset26_count2, .-bitfield__read_var_BitField_offset26_count2
	.align 4
	.type	bitfield__write_var_BitField_offset26_count2, #function
	.proc	020
bitfield__write_var_BitField_offset26_count2:
.LLFB953:
	save	%sp, -104, %sp
.LLCFI953:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1906
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1907
	 nop
.LL1906:
	sethi	%hi(122880), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1907:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -49, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE953:
	.size	bitfield__write_var_BitField_offset26_count2, .-bitfield__write_var_BitField_offset26_count2
	.align 4
	.type	bitfield__read_var_BitField_offset26_count3, #function
	.proc	04
bitfield__read_var_BitField_offset26_count3:
.LLFB954:
	save	%sp, -96, %sp
.LLCFI954:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE954:
	.size	bitfield__read_var_BitField_offset26_count3, .-bitfield__read_var_BitField_offset26_count3
	.align 4
	.type	bitfield__write_var_BitField_offset26_count3, #function
	.proc	020
bitfield__write_var_BitField_offset26_count3:
.LLFB955:
	save	%sp, -104, %sp
.LLCFI955:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1910
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1911
	 nop
.LL1910:
	sethi	%hi(123904), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1911:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -57, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE955:
	.size	bitfield__write_var_BitField_offset26_count3, .-bitfield__write_var_BitField_offset26_count3
	.align 4
	.type	bitfield__read_var_BitField_offset26_count4, #function
	.proc	04
bitfield__read_var_BitField_offset26_count4:
.LLFB956:
	save	%sp, -96, %sp
.LLCFI956:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE956:
	.size	bitfield__read_var_BitField_offset26_count4, .-bitfield__read_var_BitField_offset26_count4
	.align 4
	.type	bitfield__write_var_BitField_offset26_count4, #function
	.proc	020
bitfield__write_var_BitField_offset26_count4:
.LLFB957:
	save	%sp, -104, %sp
.LLCFI957:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1914
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1915
	 nop
.LL1914:
	sethi	%hi(123904), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1915:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -61, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE957:
	.size	bitfield__write_var_BitField_offset26_count4, .-bitfield__write_var_BitField_offset26_count4
	.align 4
	.type	bitfield__read_var_BitField_offset26_count5, #function
	.proc	04
bitfield__read_var_BitField_offset26_count5:
.LLFB958:
	save	%sp, -96, %sp
.LLCFI958:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 26, %g1
	srl	%g1, 27, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE958:
	.size	bitfield__read_var_BitField_offset26_count5, .-bitfield__read_var_BitField_offset26_count5
	.align 4
	.type	bitfield__write_var_BitField_offset26_count5, #function
	.proc	020
bitfield__write_var_BitField_offset26_count5:
.LLFB959:
	save	%sp, -104, %sp
.LLCFI959:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1918
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1919
	 nop
.LL1918:
	sethi	%hi(123904), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1919:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, -63, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 31, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE959:
	.size	bitfield__write_var_BitField_offset26_count5, .-bitfield__write_var_BitField_offset26_count5
	.align 4
	.type	bitfield__read_var_BitField_offset26_count6, #function
	.proc	04
bitfield__read_var_BitField_offset26_count6:
.LLFB960:
	save	%sp, -96, %sp
.LLCFI960:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 63, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE960:
	.size	bitfield__read_var_BitField_offset26_count6, .-bitfield__read_var_BitField_offset26_count6
	.align 4
	.type	bitfield__write_var_BitField_offset26_count6, #function
	.proc	020
bitfield__write_var_BitField_offset26_count6:
.LLFB961:
	save	%sp, -104, %sp
.LLCFI961:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1922
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 63
	ble	%icc, .LL1923
	 nop
.LL1922:
	sethi	%hi(123904), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1923:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -64, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 63, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE961:
	.size	bitfield__write_var_BitField_offset26_count6, .-bitfield__write_var_BitField_offset26_count6
	.align 4
	.type	bitfield__read_var_BitField_offset27_count1, #function
	.proc	04
bitfield__read_var_BitField_offset27_count1:
.LLFB962:
	save	%sp, -96, %sp
.LLCFI962:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE962:
	.size	bitfield__read_var_BitField_offset27_count1, .-bitfield__read_var_BitField_offset27_count1
	.align 4
	.type	bitfield__write_var_BitField_offset27_count1, #function
	.proc	020
bitfield__write_var_BitField_offset27_count1:
.LLFB963:
	save	%sp, -104, %sp
.LLCFI963:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1926
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1927
	 nop
.LL1926:
	sethi	%hi(124928), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1927:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -17, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 4, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE963:
	.size	bitfield__write_var_BitField_offset27_count1, .-bitfield__write_var_BitField_offset27_count1
	.align 4
	.type	bitfield__read_var_BitField_offset27_count2, #function
	.proc	04
bitfield__read_var_BitField_offset27_count2:
.LLFB964:
	save	%sp, -96, %sp
.LLCFI964:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE964:
	.size	bitfield__read_var_BitField_offset27_count2, .-bitfield__read_var_BitField_offset27_count2
	.align 4
	.type	bitfield__write_var_BitField_offset27_count2, #function
	.proc	020
bitfield__write_var_BitField_offset27_count2:
.LLFB965:
	save	%sp, -104, %sp
.LLCFI965:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1930
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1931
	 nop
.LL1930:
	sethi	%hi(124928), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1931:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -25, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE965:
	.size	bitfield__write_var_BitField_offset27_count2, .-bitfield__write_var_BitField_offset27_count2
	.align 4
	.type	bitfield__read_var_BitField_offset27_count3, #function
	.proc	04
bitfield__read_var_BitField_offset27_count3:
.LLFB966:
	save	%sp, -96, %sp
.LLCFI966:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE966:
	.size	bitfield__read_var_BitField_offset27_count3, .-bitfield__read_var_BitField_offset27_count3
	.align 4
	.type	bitfield__write_var_BitField_offset27_count3, #function
	.proc	020
bitfield__write_var_BitField_offset27_count3:
.LLFB967:
	save	%sp, -104, %sp
.LLCFI967:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1934
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1935
	 nop
.LL1934:
	sethi	%hi(124928), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1935:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -29, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE967:
	.size	bitfield__write_var_BitField_offset27_count3, .-bitfield__write_var_BitField_offset27_count3
	.align 4
	.type	bitfield__read_var_BitField_offset27_count4, #function
	.proc	04
bitfield__read_var_BitField_offset27_count4:
.LLFB968:
	save	%sp, -96, %sp
.LLCFI968:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 27, %g1
	srl	%g1, 28, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE968:
	.size	bitfield__read_var_BitField_offset27_count4, .-bitfield__read_var_BitField_offset27_count4
	.align 4
	.type	bitfield__write_var_BitField_offset27_count4, #function
	.proc	020
bitfield__write_var_BitField_offset27_count4:
.LLFB969:
	save	%sp, -104, %sp
.LLCFI969:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1938
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1939
	 nop
.LL1938:
	sethi	%hi(124928), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1939:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, -31, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 15, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE969:
	.size	bitfield__write_var_BitField_offset27_count4, .-bitfield__write_var_BitField_offset27_count4
	.align 4
	.type	bitfield__read_var_BitField_offset27_count5, #function
	.proc	04
bitfield__read_var_BitField_offset27_count5:
.LLFB970:
	save	%sp, -96, %sp
.LLCFI970:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE970:
	.size	bitfield__read_var_BitField_offset27_count5, .-bitfield__read_var_BitField_offset27_count5
	.align 4
	.type	bitfield__write_var_BitField_offset27_count5, #function
	.proc	020
bitfield__write_var_BitField_offset27_count5:
.LLFB971:
	save	%sp, -104, %sp
.LLCFI971:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1942
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 31
	ble	%icc, .LL1943
	 nop
.LL1942:
	sethi	%hi(125952), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1943:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -32, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 31, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE971:
	.size	bitfield__write_var_BitField_offset27_count5, .-bitfield__write_var_BitField_offset27_count5
	.align 4
	.type	bitfield__read_var_BitField_offset28_count1, #function
	.proc	04
bitfield__read_var_BitField_offset28_count1:
.LLFB972:
	save	%sp, -96, %sp
.LLCFI972:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE972:
	.size	bitfield__read_var_BitField_offset28_count1, .-bitfield__read_var_BitField_offset28_count1
	.align 4
	.type	bitfield__write_var_BitField_offset28_count1, #function
	.proc	020
bitfield__write_var_BitField_offset28_count1:
.LLFB973:
	save	%sp, -104, %sp
.LLCFI973:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1946
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1947
	 nop
.LL1946:
	sethi	%hi(125952), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1947:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -9, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE973:
	.size	bitfield__write_var_BitField_offset28_count1, .-bitfield__write_var_BitField_offset28_count1
	.align 4
	.type	bitfield__read_var_BitField_offset28_count2, #function
	.proc	04
bitfield__read_var_BitField_offset28_count2:
.LLFB974:
	save	%sp, -96, %sp
.LLCFI974:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE974:
	.size	bitfield__read_var_BitField_offset28_count2, .-bitfield__read_var_BitField_offset28_count2
	.align 4
	.type	bitfield__write_var_BitField_offset28_count2, #function
	.proc	020
bitfield__write_var_BitField_offset28_count2:
.LLFB975:
	save	%sp, -104, %sp
.LLCFI975:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1950
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1951
	 nop
.LL1950:
	sethi	%hi(125952), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1951:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -13, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE975:
	.size	bitfield__write_var_BitField_offset28_count2, .-bitfield__write_var_BitField_offset28_count2
	.align 4
	.type	bitfield__read_var_BitField_offset28_count3, #function
	.proc	04
bitfield__read_var_BitField_offset28_count3:
.LLFB976:
	save	%sp, -96, %sp
.LLCFI976:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 28, %g1
	srl	%g1, 29, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE976:
	.size	bitfield__read_var_BitField_offset28_count3, .-bitfield__read_var_BitField_offset28_count3
	.align 4
	.type	bitfield__write_var_BitField_offset28_count3, #function
	.proc	020
bitfield__write_var_BitField_offset28_count3:
.LLFB977:
	save	%sp, -104, %sp
.LLCFI977:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1954
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1955
	 nop
.LL1954:
	sethi	%hi(125952), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1955:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, -15, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 7, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE977:
	.size	bitfield__write_var_BitField_offset28_count3, .-bitfield__write_var_BitField_offset28_count3
	.align 4
	.type	bitfield__read_var_BitField_offset28_count4, #function
	.proc	04
bitfield__read_var_BitField_offset28_count4:
.LLFB978:
	save	%sp, -96, %sp
.LLCFI978:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 15, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE978:
	.size	bitfield__read_var_BitField_offset28_count4, .-bitfield__read_var_BitField_offset28_count4
	.align 4
	.type	bitfield__write_var_BitField_offset28_count4, #function
	.proc	020
bitfield__write_var_BitField_offset28_count4:
.LLFB979:
	save	%sp, -104, %sp
.LLCFI979:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1958
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 15
	ble	%icc, .LL1959
	 nop
.LL1958:
	sethi	%hi(126976), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1959:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -16, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 15, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE979:
	.size	bitfield__write_var_BitField_offset28_count4, .-bitfield__write_var_BitField_offset28_count4
	.align 4
	.type	bitfield__read_var_BitField_offset29_count1, #function
	.proc	04
bitfield__read_var_BitField_offset29_count1:
.LLFB980:
	save	%sp, -96, %sp
.LLCFI980:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE980:
	.size	bitfield__read_var_BitField_offset29_count1, .-bitfield__read_var_BitField_offset29_count1
	.align 4
	.type	bitfield__write_var_BitField_offset29_count1, #function
	.proc	020
bitfield__write_var_BitField_offset29_count1:
.LLFB981:
	save	%sp, -104, %sp
.LLCFI981:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1962
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1963
	 nop
.LL1962:
	sethi	%hi(126976), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1963:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -5, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	sll	%o5, 2, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE981:
	.size	bitfield__write_var_BitField_offset29_count1, .-bitfield__write_var_BitField_offset29_count1
	.align 4
	.type	bitfield__read_var_BitField_offset29_count2, #function
	.proc	04
bitfield__read_var_BitField_offset29_count2:
.LLFB982:
	save	%sp, -96, %sp
.LLCFI982:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 29, %g1
	srl	%g1, 30, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE982:
	.size	bitfield__read_var_BitField_offset29_count2, .-bitfield__read_var_BitField_offset29_count2
	.align 4
	.type	bitfield__write_var_BitField_offset29_count2, #function
	.proc	020
bitfield__write_var_BitField_offset29_count2:
.LLFB983:
	save	%sp, -104, %sp
.LLCFI983:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1966
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1967
	 nop
.LL1966:
	sethi	%hi(126976), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1967:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, -7, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 3, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE983:
	.size	bitfield__write_var_BitField_offset29_count2, .-bitfield__write_var_BitField_offset29_count2
	.align 4
	.type	bitfield__read_var_BitField_offset29_count3, #function
	.proc	04
bitfield__read_var_BitField_offset29_count3:
.LLFB984:
	save	%sp, -96, %sp
.LLCFI984:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 7, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE984:
	.size	bitfield__read_var_BitField_offset29_count3, .-bitfield__read_var_BitField_offset29_count3
	.align 4
	.type	bitfield__write_var_BitField_offset29_count3, #function
	.proc	020
bitfield__write_var_BitField_offset29_count3:
.LLFB985:
	save	%sp, -104, %sp
.LLCFI985:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1970
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 7
	ble	%icc, .LL1971
	 nop
.LL1970:
	sethi	%hi(126976), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1971:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -8, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 7, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE985:
	.size	bitfield__write_var_BitField_offset29_count3, .-bitfield__write_var_BitField_offset29_count3
	.align 4
	.type	bitfield__read_var_BitField_offset30_count1, #function
	.proc	04
bitfield__read_var_BitField_offset30_count1:
.LLFB986:
	save	%sp, -96, %sp
.LLCFI986:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	sll	%g1, 30, %g1
	srl	%g1, 31, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE986:
	.size	bitfield__read_var_BitField_offset30_count1, .-bitfield__read_var_BitField_offset30_count1
	.align 4
	.type	bitfield__write_var_BitField_offset30_count1, #function
	.proc	020
bitfield__write_var_BitField_offset30_count1:
.LLFB987:
	save	%sp, -104, %sp
.LLCFI987:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1974
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1975
	 nop
.LL1974:
	sethi	%hi(128000), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1975:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %o5
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ldub	[%g1], %g1
	and	%g1, -3, %g1
	mov	%g1, %o4
	ld	[%fp+72], %g1
	and	%g1, 1, %g1
	add	%g1, %g1, %g1
	or	%o4, %g1, %g1
	stb	%g1, [%o5]
	return	%i7+8
	 nop
.LLFE987:
	.size	bitfield__write_var_BitField_offset30_count1, .-bitfield__write_var_BitField_offset30_count1
	.align 4
	.type	bitfield__read_var_BitField_offset30_count2, #function
	.proc	04
bitfield__read_var_BitField_offset30_count2:
.LLFB988:
	save	%sp, -96, %sp
.LLCFI988:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 3, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE988:
	.size	bitfield__read_var_BitField_offset30_count2, .-bitfield__read_var_BitField_offset30_count2
	.align 4
	.type	bitfield__write_var_BitField_offset30_count2, #function
	.proc	020
bitfield__write_var_BitField_offset30_count2:
.LLFB989:
	save	%sp, -104, %sp
.LLCFI989:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1978
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 3
	ble	%icc, .LL1979
	 nop
.LL1978:
	sethi	%hi(128000), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1979:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -4, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 3, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE989:
	.size	bitfield__write_var_BitField_offset30_count2, .-bitfield__write_var_BitField_offset30_count2
	.align 4
	.type	bitfield__read_var_BitField_offset31_count1, #function
	.proc	04
bitfield__read_var_BitField_offset31_count1:
.LLFB990:
	save	%sp, -96, %sp
.LLCFI990:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	and	%g1, 0xff, %g1
	and	%g1, 1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE990:
	.size	bitfield__read_var_BitField_offset31_count1, .-bitfield__read_var_BitField_offset31_count1
	.align 4
	.type	bitfield__write_var_BitField_offset31_count1, #function
	.proc	020
bitfield__write_var_BitField_offset31_count1:
.LLFB991:
	save	%sp, -104, %sp
.LLCFI991:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %g1
	cmp	%g1, 0
	bl	%icc, .LL1982
	 nop
	ld	[%fp+72], %g1
	cmp	%g1, 1
	ble	%icc, .LL1983
	 nop
.LL1982:
	sethi	%hi(128000), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1983:
	ld	[%fp+68], %g1
	st	%g1, [%fp-4]
	ld	[%fp-4], %g1
	add	%g1, 3, %g1
	ld	[%fp-4], %o5
	add	%o5, 3, %o5
	ldub	[%o5], %o5
	and	%o5, -2, %o5
	mov	%o5, %o4
	ld	[%fp+72], %o5
	and	%o5, 1, %o5
	or	%o4, %o5, %o5
	stb	%o5, [%g1]
	return	%i7+8
	 nop
.LLFE991:
	.size	bitfield__write_var_BitField_offset31_count1, .-bitfield__write_var_BitField_offset31_count1
	.align 4
	.global bitfield_M3
	.type	bitfield_M3, #function
	.proc	0120
bitfield_M3:
.LLFB992:
	save	%sp, -96, %sp
.LLCFI992:
	st	%i0, [%fp+68]
	sethi	%hi(MM_bitfield), %g1
	or	%g1, %lo(MM_bitfield), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE992:
	.size	bitfield_M3, .-bitfield_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB993:
	save	%sp, -96, %sp
.LLCFI993:
	st	%i0, [%fp+68]
	sethi	%hi(MM_bitfield), %g1
	or	%g1, %lo(MM_bitfield), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE993:
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
	.ascii	"../SOLsun/bitfield.m3"
	.skip 7
	.align 8
	.type	MM_bitfield, #object
	.size	MM_bitfield, 208
MM_bitfield:
	.long	L_1+42204
	.skip 16
	.long	L_1+34256
	.skip 12
	.long	MM_bitfield+148
	.skip 4
	.long	bitfield_M3
	.long	3
	.half	881
	.skip 2
	.long	0
	.long	882
	.long	0
	.long	883
	.long	884
	.half	885
	.skip 2
	.long	886
	.skip 4
	.long	0
	.long	887
	.long	888
	.skip 4
	.long	1082904350
	.long	-1202590843
	.byte	123
	.skip 7
	.long	0
	.long	892
	.byte	125
	.skip 3
	.long	894
	.long	1147140440
	.skip 12
	.long	bitfield_I3
	.long	MM_bitfield+160
	.skip 4
	.long	Long_I3
	.long	MM_bitfield+172
	.skip 4
	.long	Word_I3
	.long	MM_bitfield+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_bitfield+196
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
.LLEFDE5:
.LLSFDE7:
	.uaword	.LLEFDE7-.LLASFDE7
.LLASFDE7:
	.uaword	.LLASFDE7-.LLframe1
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
.LLEFDE7:
.LLSFDE9:
	.uaword	.LLEFDE9-.LLASFDE9
.LLASFDE9:
	.uaword	.LLASFDE9-.LLframe1
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
.LLEFDE9:
.LLSFDE11:
	.uaword	.LLEFDE11-.LLASFDE11
.LLASFDE11:
	.uaword	.LLASFDE11-.LLframe1
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
.LLEFDE11:
.LLSFDE13:
	.uaword	.LLEFDE13-.LLASFDE13
.LLASFDE13:
	.uaword	.LLASFDE13-.LLframe1
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
.LLEFDE13:
.LLSFDE15:
	.uaword	.LLEFDE15-.LLASFDE15
.LLASFDE15:
	.uaword	.LLASFDE15-.LLframe1
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
.LLEFDE15:
.LLSFDE17:
	.uaword	.LLEFDE17-.LLASFDE17
.LLASFDE17:
	.uaword	.LLASFDE17-.LLframe1
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
.LLEFDE17:
.LLSFDE19:
	.uaword	.LLEFDE19-.LLASFDE19
.LLASFDE19:
	.uaword	.LLASFDE19-.LLframe1
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
.LLEFDE19:
.LLSFDE21:
	.uaword	.LLEFDE21-.LLASFDE21
.LLASFDE21:
	.uaword	.LLASFDE21-.LLframe1
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
.LLEFDE21:
.LLSFDE23:
	.uaword	.LLEFDE23-.LLASFDE23
.LLASFDE23:
	.uaword	.LLASFDE23-.LLframe1
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
.LLEFDE23:
.LLSFDE25:
	.uaword	.LLEFDE25-.LLASFDE25
.LLASFDE25:
	.uaword	.LLASFDE25-.LLframe1
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
.LLEFDE25:
.LLSFDE27:
	.uaword	.LLEFDE27-.LLASFDE27
.LLASFDE27:
	.uaword	.LLASFDE27-.LLframe1
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
.LLEFDE27:
.LLSFDE29:
	.uaword	.LLEFDE29-.LLASFDE29
.LLASFDE29:
	.uaword	.LLASFDE29-.LLframe1
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
.LLEFDE29:
.LLSFDE31:
	.uaword	.LLEFDE31-.LLASFDE31
.LLASFDE31:
	.uaword	.LLASFDE31-.LLframe1
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
.LLEFDE31:
.LLSFDE33:
	.uaword	.LLEFDE33-.LLASFDE33
.LLASFDE33:
	.uaword	.LLASFDE33-.LLframe1
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
.LLEFDE33:
.LLSFDE35:
	.uaword	.LLEFDE35-.LLASFDE35
.LLASFDE35:
	.uaword	.LLASFDE35-.LLframe1
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
.LLEFDE35:
.LLSFDE37:
	.uaword	.LLEFDE37-.LLASFDE37
.LLASFDE37:
	.uaword	.LLASFDE37-.LLframe1
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
.LLEFDE37:
.LLSFDE39:
	.uaword	.LLEFDE39-.LLASFDE39
.LLASFDE39:
	.uaword	.LLASFDE39-.LLframe1
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
.LLEFDE39:
.LLSFDE41:
	.uaword	.LLEFDE41-.LLASFDE41
.LLASFDE41:
	.uaword	.LLASFDE41-.LLframe1
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
.LLEFDE41:
.LLSFDE43:
	.uaword	.LLEFDE43-.LLASFDE43
.LLASFDE43:
	.uaword	.LLASFDE43-.LLframe1
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
.LLEFDE43:
.LLSFDE45:
	.uaword	.LLEFDE45-.LLASFDE45
.LLASFDE45:
	.uaword	.LLASFDE45-.LLframe1
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
.LLEFDE45:
.LLSFDE47:
	.uaword	.LLEFDE47-.LLASFDE47
.LLASFDE47:
	.uaword	.LLASFDE47-.LLframe1
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
.LLEFDE47:
.LLSFDE49:
	.uaword	.LLEFDE49-.LLASFDE49
.LLASFDE49:
	.uaword	.LLASFDE49-.LLframe1
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
.LLEFDE49:
.LLSFDE51:
	.uaword	.LLEFDE51-.LLASFDE51
.LLASFDE51:
	.uaword	.LLASFDE51-.LLframe1
	.uaword	.LLFB51
	.uaword	.LLFE51-.LLFB51
	.byte	0x4
	.uaword	.LLCFI51-.LLFB51
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
	.uaword	.LLFB53
	.uaword	.LLFE53-.LLFB53
	.byte	0x4
	.uaword	.LLCFI53-.LLFB53
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
	.uaword	.LLFB55
	.uaword	.LLFE55-.LLFB55
	.byte	0x4
	.uaword	.LLCFI55-.LLFB55
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
	.uaword	.LLFB57
	.uaword	.LLFE57-.LLFB57
	.byte	0x4
	.uaword	.LLCFI57-.LLFB57
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
	.uaword	.LLFB59
	.uaword	.LLFE59-.LLFB59
	.byte	0x4
	.uaword	.LLCFI59-.LLFB59
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
	.uaword	.LLFB61
	.uaword	.LLFE61-.LLFB61
	.byte	0x4
	.uaword	.LLCFI61-.LLFB61
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
	.uaword	.LLFB63
	.uaword	.LLFE63-.LLFB63
	.byte	0x4
	.uaword	.LLCFI63-.LLFB63
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
	.uaword	.LLFB65
	.uaword	.LLFE65-.LLFB65
	.byte	0x4
	.uaword	.LLCFI65-.LLFB65
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
	.uaword	.LLFB67
	.uaword	.LLFE67-.LLFB67
	.byte	0x4
	.uaword	.LLCFI67-.LLFB67
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
	.uaword	.LLFB69
	.uaword	.LLFE69-.LLFB69
	.byte	0x4
	.uaword	.LLCFI69-.LLFB69
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
	.uaword	.LLFB71
	.uaword	.LLFE71-.LLFB71
	.byte	0x4
	.uaword	.LLCFI71-.LLFB71
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
	.uaword	.LLFB73
	.uaword	.LLFE73-.LLFB73
	.byte	0x4
	.uaword	.LLCFI73-.LLFB73
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
	.uaword	.LLFB75
	.uaword	.LLFE75-.LLFB75
	.byte	0x4
	.uaword	.LLCFI75-.LLFB75
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
	.uaword	.LLFB77
	.uaword	.LLFE77-.LLFB77
	.byte	0x4
	.uaword	.LLCFI77-.LLFB77
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
	.uaword	.LLFB79
	.uaword	.LLFE79-.LLFB79
	.byte	0x4
	.uaword	.LLCFI79-.LLFB79
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
	.uaword	.LLFB81
	.uaword	.LLFE81-.LLFB81
	.byte	0x4
	.uaword	.LLCFI81-.LLFB81
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
	.uaword	.LLFB83
	.uaword	.LLFE83-.LLFB83
	.byte	0x4
	.uaword	.LLCFI83-.LLFB83
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
	.uaword	.LLFB85
	.uaword	.LLFE85-.LLFB85
	.byte	0x4
	.uaword	.LLCFI85-.LLFB85
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
	.uaword	.LLFB87
	.uaword	.LLFE87-.LLFB87
	.byte	0x4
	.uaword	.LLCFI87-.LLFB87
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
	.uaword	.LLFB89
	.uaword	.LLFE89-.LLFB89
	.byte	0x4
	.uaword	.LLCFI89-.LLFB89
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
	.uaword	.LLFB91
	.uaword	.LLFE91-.LLFB91
	.byte	0x4
	.uaword	.LLCFI91-.LLFB91
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
	.uaword	.LLFB93
	.uaword	.LLFE93-.LLFB93
	.byte	0x4
	.uaword	.LLCFI93-.LLFB93
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
	.uaword	.LLFB95
	.uaword	.LLFE95-.LLFB95
	.byte	0x4
	.uaword	.LLCFI95-.LLFB95
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
	.uaword	.LLFB97
	.uaword	.LLFE97-.LLFB97
	.byte	0x4
	.uaword	.LLCFI97-.LLFB97
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
	.uaword	.LLFB99
	.uaword	.LLFE99-.LLFB99
	.byte	0x4
	.uaword	.LLCFI99-.LLFB99
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
	.uaword	.LLFB101
	.uaword	.LLFE101-.LLFB101
	.byte	0x4
	.uaword	.LLCFI101-.LLFB101
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
	.uaword	.LLFB103
	.uaword	.LLFE103-.LLFB103
	.byte	0x4
	.uaword	.LLCFI103-.LLFB103
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
	.uaword	.LLFB105
	.uaword	.LLFE105-.LLFB105
	.byte	0x4
	.uaword	.LLCFI105-.LLFB105
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
	.uaword	.LLFB107
	.uaword	.LLFE107-.LLFB107
	.byte	0x4
	.uaword	.LLCFI107-.LLFB107
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
	.uaword	.LLFB109
	.uaword	.LLFE109-.LLFB109
	.byte	0x4
	.uaword	.LLCFI109-.LLFB109
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
	.uaword	.LLFB111
	.uaword	.LLFE111-.LLFB111
	.byte	0x4
	.uaword	.LLCFI111-.LLFB111
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
	.uaword	.LLFB113
	.uaword	.LLFE113-.LLFB113
	.byte	0x4
	.uaword	.LLCFI113-.LLFB113
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
	.uaword	.LLFB115
	.uaword	.LLFE115-.LLFB115
	.byte	0x4
	.uaword	.LLCFI115-.LLFB115
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
	.uaword	.LLFB117
	.uaword	.LLFE117-.LLFB117
	.byte	0x4
	.uaword	.LLCFI117-.LLFB117
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
	.uaword	.LLFB119
	.uaword	.LLFE119-.LLFB119
	.byte	0x4
	.uaword	.LLCFI119-.LLFB119
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
	.uaword	.LLFB121
	.uaword	.LLFE121-.LLFB121
	.byte	0x4
	.uaword	.LLCFI121-.LLFB121
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
	.uaword	.LLFB123
	.uaword	.LLFE123-.LLFB123
	.byte	0x4
	.uaword	.LLCFI123-.LLFB123
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
	.uaword	.LLFB125
	.uaword	.LLFE125-.LLFB125
	.byte	0x4
	.uaword	.LLCFI125-.LLFB125
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
	.uaword	.LLFB127
	.uaword	.LLFE127-.LLFB127
	.byte	0x4
	.uaword	.LLCFI127-.LLFB127
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
	.uaword	.LLFB129
	.uaword	.LLFE129-.LLFB129
	.byte	0x4
	.uaword	.LLCFI129-.LLFB129
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
	.uaword	.LLFB131
	.uaword	.LLFE131-.LLFB131
	.byte	0x4
	.uaword	.LLCFI131-.LLFB131
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
	.uaword	.LLFB133
	.uaword	.LLFE133-.LLFB133
	.byte	0x4
	.uaword	.LLCFI133-.LLFB133
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
	.uaword	.LLFB135
	.uaword	.LLFE135-.LLFB135
	.byte	0x4
	.uaword	.LLCFI135-.LLFB135
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
	.uaword	.LLFB137
	.uaword	.LLFE137-.LLFB137
	.byte	0x4
	.uaword	.LLCFI137-.LLFB137
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
	.uaword	.LLFB139
	.uaword	.LLFE139-.LLFB139
	.byte	0x4
	.uaword	.LLCFI139-.LLFB139
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
	.uaword	.LLFB141
	.uaword	.LLFE141-.LLFB141
	.byte	0x4
	.uaword	.LLCFI141-.LLFB141
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
	.uaword	.LLFB143
	.uaword	.LLFE143-.LLFB143
	.byte	0x4
	.uaword	.LLCFI143-.LLFB143
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
	.uaword	.LLFB145
	.uaword	.LLFE145-.LLFB145
	.byte	0x4
	.uaword	.LLCFI145-.LLFB145
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
	.uaword	.LLFB147
	.uaword	.LLFE147-.LLFB147
	.byte	0x4
	.uaword	.LLCFI147-.LLFB147
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
	.uaword	.LLFB149
	.uaword	.LLFE149-.LLFB149
	.byte	0x4
	.uaword	.LLCFI149-.LLFB149
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
	.uaword	.LLFB151
	.uaword	.LLFE151-.LLFB151
	.byte	0x4
	.uaword	.LLCFI151-.LLFB151
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
	.uaword	.LLFB153
	.uaword	.LLFE153-.LLFB153
	.byte	0x4
	.uaword	.LLCFI153-.LLFB153
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
	.uaword	.LLFB155
	.uaword	.LLFE155-.LLFB155
	.byte	0x4
	.uaword	.LLCFI155-.LLFB155
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
	.uaword	.LLFB157
	.uaword	.LLFE157-.LLFB157
	.byte	0x4
	.uaword	.LLCFI157-.LLFB157
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
	.uaword	.LLFB159
	.uaword	.LLFE159-.LLFB159
	.byte	0x4
	.uaword	.LLCFI159-.LLFB159
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
	.uaword	.LLFB161
	.uaword	.LLFE161-.LLFB161
	.byte	0x4
	.uaword	.LLCFI161-.LLFB161
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE161:
.LLSFDE163:
	.uaword	.LLEFDE163-.LLASFDE163
.LLASFDE163:
	.uaword	.LLASFDE163-.LLframe1
	.uaword	.LLFB163
	.uaword	.LLFE163-.LLFB163
	.byte	0x4
	.uaword	.LLCFI163-.LLFB163
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE163:
.LLSFDE165:
	.uaword	.LLEFDE165-.LLASFDE165
.LLASFDE165:
	.uaword	.LLASFDE165-.LLframe1
	.uaword	.LLFB165
	.uaword	.LLFE165-.LLFB165
	.byte	0x4
	.uaword	.LLCFI165-.LLFB165
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE165:
.LLSFDE167:
	.uaword	.LLEFDE167-.LLASFDE167
.LLASFDE167:
	.uaword	.LLASFDE167-.LLframe1
	.uaword	.LLFB167
	.uaword	.LLFE167-.LLFB167
	.byte	0x4
	.uaword	.LLCFI167-.LLFB167
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE167:
.LLSFDE169:
	.uaword	.LLEFDE169-.LLASFDE169
.LLASFDE169:
	.uaword	.LLASFDE169-.LLframe1
	.uaword	.LLFB169
	.uaword	.LLFE169-.LLFB169
	.byte	0x4
	.uaword	.LLCFI169-.LLFB169
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE169:
.LLSFDE171:
	.uaword	.LLEFDE171-.LLASFDE171
.LLASFDE171:
	.uaword	.LLASFDE171-.LLframe1
	.uaword	.LLFB171
	.uaword	.LLFE171-.LLFB171
	.byte	0x4
	.uaword	.LLCFI171-.LLFB171
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE171:
.LLSFDE173:
	.uaword	.LLEFDE173-.LLASFDE173
.LLASFDE173:
	.uaword	.LLASFDE173-.LLframe1
	.uaword	.LLFB173
	.uaword	.LLFE173-.LLFB173
	.byte	0x4
	.uaword	.LLCFI173-.LLFB173
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE173:
.LLSFDE175:
	.uaword	.LLEFDE175-.LLASFDE175
.LLASFDE175:
	.uaword	.LLASFDE175-.LLframe1
	.uaword	.LLFB175
	.uaword	.LLFE175-.LLFB175
	.byte	0x4
	.uaword	.LLCFI175-.LLFB175
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE175:
.LLSFDE177:
	.uaword	.LLEFDE177-.LLASFDE177
.LLASFDE177:
	.uaword	.LLASFDE177-.LLframe1
	.uaword	.LLFB177
	.uaword	.LLFE177-.LLFB177
	.byte	0x4
	.uaword	.LLCFI177-.LLFB177
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE177:
.LLSFDE179:
	.uaword	.LLEFDE179-.LLASFDE179
.LLASFDE179:
	.uaword	.LLASFDE179-.LLframe1
	.uaword	.LLFB179
	.uaword	.LLFE179-.LLFB179
	.byte	0x4
	.uaword	.LLCFI179-.LLFB179
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE179:
.LLSFDE181:
	.uaword	.LLEFDE181-.LLASFDE181
.LLASFDE181:
	.uaword	.LLASFDE181-.LLframe1
	.uaword	.LLFB181
	.uaword	.LLFE181-.LLFB181
	.byte	0x4
	.uaword	.LLCFI181-.LLFB181
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE181:
.LLSFDE183:
	.uaword	.LLEFDE183-.LLASFDE183
.LLASFDE183:
	.uaword	.LLASFDE183-.LLframe1
	.uaword	.LLFB183
	.uaword	.LLFE183-.LLFB183
	.byte	0x4
	.uaword	.LLCFI183-.LLFB183
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE183:
.LLSFDE185:
	.uaword	.LLEFDE185-.LLASFDE185
.LLASFDE185:
	.uaword	.LLASFDE185-.LLframe1
	.uaword	.LLFB185
	.uaword	.LLFE185-.LLFB185
	.byte	0x4
	.uaword	.LLCFI185-.LLFB185
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE185:
.LLSFDE187:
	.uaword	.LLEFDE187-.LLASFDE187
.LLASFDE187:
	.uaword	.LLASFDE187-.LLframe1
	.uaword	.LLFB187
	.uaword	.LLFE187-.LLFB187
	.byte	0x4
	.uaword	.LLCFI187-.LLFB187
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE187:
.LLSFDE189:
	.uaword	.LLEFDE189-.LLASFDE189
.LLASFDE189:
	.uaword	.LLASFDE189-.LLframe1
	.uaword	.LLFB189
	.uaword	.LLFE189-.LLFB189
	.byte	0x4
	.uaword	.LLCFI189-.LLFB189
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE189:
.LLSFDE191:
	.uaword	.LLEFDE191-.LLASFDE191
.LLASFDE191:
	.uaword	.LLASFDE191-.LLframe1
	.uaword	.LLFB191
	.uaword	.LLFE191-.LLFB191
	.byte	0x4
	.uaword	.LLCFI191-.LLFB191
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE191:
.LLSFDE193:
	.uaword	.LLEFDE193-.LLASFDE193
.LLASFDE193:
	.uaword	.LLASFDE193-.LLframe1
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
.LLEFDE193:
.LLSFDE195:
	.uaword	.LLEFDE195-.LLASFDE195
.LLASFDE195:
	.uaword	.LLASFDE195-.LLframe1
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
.LLEFDE195:
.LLSFDE197:
	.uaword	.LLEFDE197-.LLASFDE197
.LLASFDE197:
	.uaword	.LLASFDE197-.LLframe1
	.uaword	.LLFB197
	.uaword	.LLFE197-.LLFB197
	.byte	0x4
	.uaword	.LLCFI197-.LLFB197
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE197:
.LLSFDE199:
	.uaword	.LLEFDE199-.LLASFDE199
.LLASFDE199:
	.uaword	.LLASFDE199-.LLframe1
	.uaword	.LLFB199
	.uaword	.LLFE199-.LLFB199
	.byte	0x4
	.uaword	.LLCFI199-.LLFB199
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE199:
.LLSFDE201:
	.uaword	.LLEFDE201-.LLASFDE201
.LLASFDE201:
	.uaword	.LLASFDE201-.LLframe1
	.uaword	.LLFB201
	.uaword	.LLFE201-.LLFB201
	.byte	0x4
	.uaword	.LLCFI201-.LLFB201
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE201:
.LLSFDE203:
	.uaword	.LLEFDE203-.LLASFDE203
.LLASFDE203:
	.uaword	.LLASFDE203-.LLframe1
	.uaword	.LLFB203
	.uaword	.LLFE203-.LLFB203
	.byte	0x4
	.uaword	.LLCFI203-.LLFB203
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE203:
.LLSFDE205:
	.uaword	.LLEFDE205-.LLASFDE205
.LLASFDE205:
	.uaword	.LLASFDE205-.LLframe1
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
.LLEFDE205:
.LLSFDE207:
	.uaword	.LLEFDE207-.LLASFDE207
.LLASFDE207:
	.uaword	.LLASFDE207-.LLframe1
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
.LLEFDE207:
.LLSFDE209:
	.uaword	.LLEFDE209-.LLASFDE209
.LLASFDE209:
	.uaword	.LLASFDE209-.LLframe1
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
.LLEFDE209:
.LLSFDE211:
	.uaword	.LLEFDE211-.LLASFDE211
.LLASFDE211:
	.uaword	.LLASFDE211-.LLframe1
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
.LLEFDE211:
.LLSFDE213:
	.uaword	.LLEFDE213-.LLASFDE213
.LLASFDE213:
	.uaword	.LLASFDE213-.LLframe1
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
.LLEFDE213:
.LLSFDE215:
	.uaword	.LLEFDE215-.LLASFDE215
.LLASFDE215:
	.uaword	.LLASFDE215-.LLframe1
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
.LLEFDE215:
.LLSFDE217:
	.uaword	.LLEFDE217-.LLASFDE217
.LLASFDE217:
	.uaword	.LLASFDE217-.LLframe1
	.uaword	.LLFB217
	.uaword	.LLFE217-.LLFB217
	.byte	0x4
	.uaword	.LLCFI217-.LLFB217
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE217:
.LLSFDE219:
	.uaword	.LLEFDE219-.LLASFDE219
.LLASFDE219:
	.uaword	.LLASFDE219-.LLframe1
	.uaword	.LLFB219
	.uaword	.LLFE219-.LLFB219
	.byte	0x4
	.uaword	.LLCFI219-.LLFB219
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE219:
.LLSFDE221:
	.uaword	.LLEFDE221-.LLASFDE221
.LLASFDE221:
	.uaword	.LLASFDE221-.LLframe1
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
.LLEFDE221:
.LLSFDE223:
	.uaword	.LLEFDE223-.LLASFDE223
.LLASFDE223:
	.uaword	.LLASFDE223-.LLframe1
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
.LLEFDE223:
.LLSFDE225:
	.uaword	.LLEFDE225-.LLASFDE225
.LLASFDE225:
	.uaword	.LLASFDE225-.LLframe1
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
.LLEFDE225:
.LLSFDE227:
	.uaword	.LLEFDE227-.LLASFDE227
.LLASFDE227:
	.uaword	.LLASFDE227-.LLframe1
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
.LLEFDE227:
.LLSFDE229:
	.uaword	.LLEFDE229-.LLASFDE229
.LLASFDE229:
	.uaword	.LLASFDE229-.LLframe1
	.uaword	.LLFB229
	.uaword	.LLFE229-.LLFB229
	.byte	0x4
	.uaword	.LLCFI229-.LLFB229
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE229:
.LLSFDE231:
	.uaword	.LLEFDE231-.LLASFDE231
.LLASFDE231:
	.uaword	.LLASFDE231-.LLframe1
	.uaword	.LLFB231
	.uaword	.LLFE231-.LLFB231
	.byte	0x4
	.uaword	.LLCFI231-.LLFB231
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE231:
.LLSFDE233:
	.uaword	.LLEFDE233-.LLASFDE233
.LLASFDE233:
	.uaword	.LLASFDE233-.LLframe1
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
.LLEFDE233:
.LLSFDE235:
	.uaword	.LLEFDE235-.LLASFDE235
.LLASFDE235:
	.uaword	.LLASFDE235-.LLframe1
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
.LLEFDE235:
.LLSFDE237:
	.uaword	.LLEFDE237-.LLASFDE237
.LLASFDE237:
	.uaword	.LLASFDE237-.LLframe1
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
.LLEFDE237:
.LLSFDE239:
	.uaword	.LLEFDE239-.LLASFDE239
.LLASFDE239:
	.uaword	.LLASFDE239-.LLframe1
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
.LLEFDE239:
.LLSFDE241:
	.uaword	.LLEFDE241-.LLASFDE241
.LLASFDE241:
	.uaword	.LLASFDE241-.LLframe1
	.uaword	.LLFB241
	.uaword	.LLFE241-.LLFB241
	.byte	0x4
	.uaword	.LLCFI241-.LLFB241
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE241:
.LLSFDE243:
	.uaword	.LLEFDE243-.LLASFDE243
.LLASFDE243:
	.uaword	.LLASFDE243-.LLframe1
	.uaword	.LLFB243
	.uaword	.LLFE243-.LLFB243
	.byte	0x4
	.uaword	.LLCFI243-.LLFB243
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE243:
.LLSFDE245:
	.uaword	.LLEFDE245-.LLASFDE245
.LLASFDE245:
	.uaword	.LLASFDE245-.LLframe1
	.uaword	.LLFB245
	.uaword	.LLFE245-.LLFB245
	.byte	0x4
	.uaword	.LLCFI245-.LLFB245
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE245:
.LLSFDE247:
	.uaword	.LLEFDE247-.LLASFDE247
.LLASFDE247:
	.uaword	.LLASFDE247-.LLframe1
	.uaword	.LLFB247
	.uaword	.LLFE247-.LLFB247
	.byte	0x4
	.uaword	.LLCFI247-.LLFB247
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE247:
.LLSFDE249:
	.uaword	.LLEFDE249-.LLASFDE249
.LLASFDE249:
	.uaword	.LLASFDE249-.LLframe1
	.uaword	.LLFB249
	.uaword	.LLFE249-.LLFB249
	.byte	0x4
	.uaword	.LLCFI249-.LLFB249
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE249:
.LLSFDE251:
	.uaword	.LLEFDE251-.LLASFDE251
.LLASFDE251:
	.uaword	.LLASFDE251-.LLframe1
	.uaword	.LLFB251
	.uaword	.LLFE251-.LLFB251
	.byte	0x4
	.uaword	.LLCFI251-.LLFB251
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE251:
.LLSFDE253:
	.uaword	.LLEFDE253-.LLASFDE253
.LLASFDE253:
	.uaword	.LLASFDE253-.LLframe1
	.uaword	.LLFB253
	.uaword	.LLFE253-.LLFB253
	.byte	0x4
	.uaword	.LLCFI253-.LLFB253
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE253:
.LLSFDE255:
	.uaword	.LLEFDE255-.LLASFDE255
.LLASFDE255:
	.uaword	.LLASFDE255-.LLframe1
	.uaword	.LLFB255
	.uaword	.LLFE255-.LLFB255
	.byte	0x4
	.uaword	.LLCFI255-.LLFB255
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE255:
.LLSFDE257:
	.uaword	.LLEFDE257-.LLASFDE257
.LLASFDE257:
	.uaword	.LLASFDE257-.LLframe1
	.uaword	.LLFB257
	.uaword	.LLFE257-.LLFB257
	.byte	0x4
	.uaword	.LLCFI257-.LLFB257
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE257:
.LLSFDE259:
	.uaword	.LLEFDE259-.LLASFDE259
.LLASFDE259:
	.uaword	.LLASFDE259-.LLframe1
	.uaword	.LLFB259
	.uaword	.LLFE259-.LLFB259
	.byte	0x4
	.uaword	.LLCFI259-.LLFB259
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE259:
.LLSFDE261:
	.uaword	.LLEFDE261-.LLASFDE261
.LLASFDE261:
	.uaword	.LLASFDE261-.LLframe1
	.uaword	.LLFB261
	.uaword	.LLFE261-.LLFB261
	.byte	0x4
	.uaword	.LLCFI261-.LLFB261
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE261:
.LLSFDE263:
	.uaword	.LLEFDE263-.LLASFDE263
.LLASFDE263:
	.uaword	.LLASFDE263-.LLframe1
	.uaword	.LLFB263
	.uaword	.LLFE263-.LLFB263
	.byte	0x4
	.uaword	.LLCFI263-.LLFB263
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE263:
.LLSFDE265:
	.uaword	.LLEFDE265-.LLASFDE265
.LLASFDE265:
	.uaword	.LLASFDE265-.LLframe1
	.uaword	.LLFB265
	.uaword	.LLFE265-.LLFB265
	.byte	0x4
	.uaword	.LLCFI265-.LLFB265
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE265:
.LLSFDE267:
	.uaword	.LLEFDE267-.LLASFDE267
.LLASFDE267:
	.uaword	.LLASFDE267-.LLframe1
	.uaword	.LLFB267
	.uaword	.LLFE267-.LLFB267
	.byte	0x4
	.uaword	.LLCFI267-.LLFB267
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE267:
.LLSFDE269:
	.uaword	.LLEFDE269-.LLASFDE269
.LLASFDE269:
	.uaword	.LLASFDE269-.LLframe1
	.uaword	.LLFB269
	.uaword	.LLFE269-.LLFB269
	.byte	0x4
	.uaword	.LLCFI269-.LLFB269
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE269:
.LLSFDE271:
	.uaword	.LLEFDE271-.LLASFDE271
.LLASFDE271:
	.uaword	.LLASFDE271-.LLframe1
	.uaword	.LLFB271
	.uaword	.LLFE271-.LLFB271
	.byte	0x4
	.uaword	.LLCFI271-.LLFB271
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE271:
.LLSFDE273:
	.uaword	.LLEFDE273-.LLASFDE273
.LLASFDE273:
	.uaword	.LLASFDE273-.LLframe1
	.uaword	.LLFB273
	.uaword	.LLFE273-.LLFB273
	.byte	0x4
	.uaword	.LLCFI273-.LLFB273
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE273:
.LLSFDE275:
	.uaword	.LLEFDE275-.LLASFDE275
.LLASFDE275:
	.uaword	.LLASFDE275-.LLframe1
	.uaword	.LLFB275
	.uaword	.LLFE275-.LLFB275
	.byte	0x4
	.uaword	.LLCFI275-.LLFB275
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE275:
.LLSFDE277:
	.uaword	.LLEFDE277-.LLASFDE277
.LLASFDE277:
	.uaword	.LLASFDE277-.LLframe1
	.uaword	.LLFB277
	.uaword	.LLFE277-.LLFB277
	.byte	0x4
	.uaword	.LLCFI277-.LLFB277
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE277:
.LLSFDE279:
	.uaword	.LLEFDE279-.LLASFDE279
.LLASFDE279:
	.uaword	.LLASFDE279-.LLframe1
	.uaword	.LLFB279
	.uaword	.LLFE279-.LLFB279
	.byte	0x4
	.uaword	.LLCFI279-.LLFB279
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE279:
.LLSFDE281:
	.uaword	.LLEFDE281-.LLASFDE281
.LLASFDE281:
	.uaword	.LLASFDE281-.LLframe1
	.uaword	.LLFB281
	.uaword	.LLFE281-.LLFB281
	.byte	0x4
	.uaword	.LLCFI281-.LLFB281
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE281:
.LLSFDE283:
	.uaword	.LLEFDE283-.LLASFDE283
.LLASFDE283:
	.uaword	.LLASFDE283-.LLframe1
	.uaword	.LLFB283
	.uaword	.LLFE283-.LLFB283
	.byte	0x4
	.uaword	.LLCFI283-.LLFB283
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE283:
.LLSFDE285:
	.uaword	.LLEFDE285-.LLASFDE285
.LLASFDE285:
	.uaword	.LLASFDE285-.LLframe1
	.uaword	.LLFB285
	.uaword	.LLFE285-.LLFB285
	.byte	0x4
	.uaword	.LLCFI285-.LLFB285
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE285:
.LLSFDE287:
	.uaword	.LLEFDE287-.LLASFDE287
.LLASFDE287:
	.uaword	.LLASFDE287-.LLframe1
	.uaword	.LLFB287
	.uaword	.LLFE287-.LLFB287
	.byte	0x4
	.uaword	.LLCFI287-.LLFB287
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE287:
.LLSFDE289:
	.uaword	.LLEFDE289-.LLASFDE289
.LLASFDE289:
	.uaword	.LLASFDE289-.LLframe1
	.uaword	.LLFB289
	.uaword	.LLFE289-.LLFB289
	.byte	0x4
	.uaword	.LLCFI289-.LLFB289
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE289:
.LLSFDE291:
	.uaword	.LLEFDE291-.LLASFDE291
.LLASFDE291:
	.uaword	.LLASFDE291-.LLframe1
	.uaword	.LLFB291
	.uaword	.LLFE291-.LLFB291
	.byte	0x4
	.uaword	.LLCFI291-.LLFB291
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE291:
.LLSFDE293:
	.uaword	.LLEFDE293-.LLASFDE293
.LLASFDE293:
	.uaword	.LLASFDE293-.LLframe1
	.uaword	.LLFB293
	.uaword	.LLFE293-.LLFB293
	.byte	0x4
	.uaword	.LLCFI293-.LLFB293
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE293:
.LLSFDE295:
	.uaword	.LLEFDE295-.LLASFDE295
.LLASFDE295:
	.uaword	.LLASFDE295-.LLframe1
	.uaword	.LLFB295
	.uaword	.LLFE295-.LLFB295
	.byte	0x4
	.uaword	.LLCFI295-.LLFB295
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE295:
.LLSFDE297:
	.uaword	.LLEFDE297-.LLASFDE297
.LLASFDE297:
	.uaword	.LLASFDE297-.LLframe1
	.uaword	.LLFB297
	.uaword	.LLFE297-.LLFB297
	.byte	0x4
	.uaword	.LLCFI297-.LLFB297
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE297:
.LLSFDE299:
	.uaword	.LLEFDE299-.LLASFDE299
.LLASFDE299:
	.uaword	.LLASFDE299-.LLframe1
	.uaword	.LLFB299
	.uaword	.LLFE299-.LLFB299
	.byte	0x4
	.uaword	.LLCFI299-.LLFB299
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE299:
.LLSFDE301:
	.uaword	.LLEFDE301-.LLASFDE301
.LLASFDE301:
	.uaword	.LLASFDE301-.LLframe1
	.uaword	.LLFB301
	.uaword	.LLFE301-.LLFB301
	.byte	0x4
	.uaword	.LLCFI301-.LLFB301
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE301:
.LLSFDE303:
	.uaword	.LLEFDE303-.LLASFDE303
.LLASFDE303:
	.uaword	.LLASFDE303-.LLframe1
	.uaword	.LLFB303
	.uaword	.LLFE303-.LLFB303
	.byte	0x4
	.uaword	.LLCFI303-.LLFB303
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE303:
.LLSFDE305:
	.uaword	.LLEFDE305-.LLASFDE305
.LLASFDE305:
	.uaword	.LLASFDE305-.LLframe1
	.uaword	.LLFB305
	.uaword	.LLFE305-.LLFB305
	.byte	0x4
	.uaword	.LLCFI305-.LLFB305
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE305:
.LLSFDE307:
	.uaword	.LLEFDE307-.LLASFDE307
.LLASFDE307:
	.uaword	.LLASFDE307-.LLframe1
	.uaword	.LLFB307
	.uaword	.LLFE307-.LLFB307
	.byte	0x4
	.uaword	.LLCFI307-.LLFB307
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE307:
.LLSFDE309:
	.uaword	.LLEFDE309-.LLASFDE309
.LLASFDE309:
	.uaword	.LLASFDE309-.LLframe1
	.uaword	.LLFB309
	.uaword	.LLFE309-.LLFB309
	.byte	0x4
	.uaword	.LLCFI309-.LLFB309
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE309:
.LLSFDE311:
	.uaword	.LLEFDE311-.LLASFDE311
.LLASFDE311:
	.uaword	.LLASFDE311-.LLframe1
	.uaword	.LLFB311
	.uaword	.LLFE311-.LLFB311
	.byte	0x4
	.uaword	.LLCFI311-.LLFB311
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE311:
.LLSFDE313:
	.uaword	.LLEFDE313-.LLASFDE313
.LLASFDE313:
	.uaword	.LLASFDE313-.LLframe1
	.uaword	.LLFB313
	.uaword	.LLFE313-.LLFB313
	.byte	0x4
	.uaword	.LLCFI313-.LLFB313
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE313:
.LLSFDE315:
	.uaword	.LLEFDE315-.LLASFDE315
.LLASFDE315:
	.uaword	.LLASFDE315-.LLframe1
	.uaword	.LLFB315
	.uaword	.LLFE315-.LLFB315
	.byte	0x4
	.uaword	.LLCFI315-.LLFB315
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE315:
.LLSFDE317:
	.uaword	.LLEFDE317-.LLASFDE317
.LLASFDE317:
	.uaword	.LLASFDE317-.LLframe1
	.uaword	.LLFB317
	.uaword	.LLFE317-.LLFB317
	.byte	0x4
	.uaword	.LLCFI317-.LLFB317
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE317:
.LLSFDE319:
	.uaword	.LLEFDE319-.LLASFDE319
.LLASFDE319:
	.uaword	.LLASFDE319-.LLframe1
	.uaword	.LLFB319
	.uaword	.LLFE319-.LLFB319
	.byte	0x4
	.uaword	.LLCFI319-.LLFB319
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE319:
.LLSFDE321:
	.uaword	.LLEFDE321-.LLASFDE321
.LLASFDE321:
	.uaword	.LLASFDE321-.LLframe1
	.uaword	.LLFB321
	.uaword	.LLFE321-.LLFB321
	.byte	0x4
	.uaword	.LLCFI321-.LLFB321
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE321:
.LLSFDE323:
	.uaword	.LLEFDE323-.LLASFDE323
.LLASFDE323:
	.uaword	.LLASFDE323-.LLframe1
	.uaword	.LLFB323
	.uaword	.LLFE323-.LLFB323
	.byte	0x4
	.uaword	.LLCFI323-.LLFB323
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE323:
.LLSFDE325:
	.uaword	.LLEFDE325-.LLASFDE325
.LLASFDE325:
	.uaword	.LLASFDE325-.LLframe1
	.uaword	.LLFB325
	.uaword	.LLFE325-.LLFB325
	.byte	0x4
	.uaword	.LLCFI325-.LLFB325
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE325:
.LLSFDE327:
	.uaword	.LLEFDE327-.LLASFDE327
.LLASFDE327:
	.uaword	.LLASFDE327-.LLframe1
	.uaword	.LLFB327
	.uaword	.LLFE327-.LLFB327
	.byte	0x4
	.uaword	.LLCFI327-.LLFB327
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE327:
.LLSFDE329:
	.uaword	.LLEFDE329-.LLASFDE329
.LLASFDE329:
	.uaword	.LLASFDE329-.LLframe1
	.uaword	.LLFB329
	.uaword	.LLFE329-.LLFB329
	.byte	0x4
	.uaword	.LLCFI329-.LLFB329
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE329:
.LLSFDE331:
	.uaword	.LLEFDE331-.LLASFDE331
.LLASFDE331:
	.uaword	.LLASFDE331-.LLframe1
	.uaword	.LLFB331
	.uaword	.LLFE331-.LLFB331
	.byte	0x4
	.uaword	.LLCFI331-.LLFB331
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE331:
.LLSFDE333:
	.uaword	.LLEFDE333-.LLASFDE333
.LLASFDE333:
	.uaword	.LLASFDE333-.LLframe1
	.uaword	.LLFB333
	.uaword	.LLFE333-.LLFB333
	.byte	0x4
	.uaword	.LLCFI333-.LLFB333
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE333:
.LLSFDE335:
	.uaword	.LLEFDE335-.LLASFDE335
.LLASFDE335:
	.uaword	.LLASFDE335-.LLframe1
	.uaword	.LLFB335
	.uaword	.LLFE335-.LLFB335
	.byte	0x4
	.uaword	.LLCFI335-.LLFB335
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE335:
.LLSFDE337:
	.uaword	.LLEFDE337-.LLASFDE337
.LLASFDE337:
	.uaword	.LLASFDE337-.LLframe1
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
.LLEFDE337:
.LLSFDE339:
	.uaword	.LLEFDE339-.LLASFDE339
.LLASFDE339:
	.uaword	.LLASFDE339-.LLframe1
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
.LLEFDE339:
.LLSFDE341:
	.uaword	.LLEFDE341-.LLASFDE341
.LLASFDE341:
	.uaword	.LLASFDE341-.LLframe1
	.uaword	.LLFB341
	.uaword	.LLFE341-.LLFB341
	.byte	0x4
	.uaword	.LLCFI341-.LLFB341
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE341:
.LLSFDE343:
	.uaword	.LLEFDE343-.LLASFDE343
.LLASFDE343:
	.uaword	.LLASFDE343-.LLframe1
	.uaword	.LLFB343
	.uaword	.LLFE343-.LLFB343
	.byte	0x4
	.uaword	.LLCFI343-.LLFB343
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE343:
.LLSFDE345:
	.uaword	.LLEFDE345-.LLASFDE345
.LLASFDE345:
	.uaword	.LLASFDE345-.LLframe1
	.uaword	.LLFB345
	.uaword	.LLFE345-.LLFB345
	.byte	0x4
	.uaword	.LLCFI345-.LLFB345
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE345:
.LLSFDE347:
	.uaword	.LLEFDE347-.LLASFDE347
.LLASFDE347:
	.uaword	.LLASFDE347-.LLframe1
	.uaword	.LLFB347
	.uaword	.LLFE347-.LLFB347
	.byte	0x4
	.uaword	.LLCFI347-.LLFB347
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE347:
.LLSFDE349:
	.uaword	.LLEFDE349-.LLASFDE349
.LLASFDE349:
	.uaword	.LLASFDE349-.LLframe1
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
.LLEFDE349:
.LLSFDE351:
	.uaword	.LLEFDE351-.LLASFDE351
.LLASFDE351:
	.uaword	.LLASFDE351-.LLframe1
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
.LLEFDE351:
.LLSFDE353:
	.uaword	.LLEFDE353-.LLASFDE353
.LLASFDE353:
	.uaword	.LLASFDE353-.LLframe1
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
.LLEFDE353:
.LLSFDE355:
	.uaword	.LLEFDE355-.LLASFDE355
.LLASFDE355:
	.uaword	.LLASFDE355-.LLframe1
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
.LLEFDE355:
.LLSFDE357:
	.uaword	.LLEFDE357-.LLASFDE357
.LLASFDE357:
	.uaword	.LLASFDE357-.LLframe1
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
.LLEFDE357:
.LLSFDE359:
	.uaword	.LLEFDE359-.LLASFDE359
.LLASFDE359:
	.uaword	.LLASFDE359-.LLframe1
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
.LLEFDE359:
.LLSFDE361:
	.uaword	.LLEFDE361-.LLASFDE361
.LLASFDE361:
	.uaword	.LLASFDE361-.LLframe1
	.uaword	.LLFB361
	.uaword	.LLFE361-.LLFB361
	.byte	0x4
	.uaword	.LLCFI361-.LLFB361
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE361:
.LLSFDE363:
	.uaword	.LLEFDE363-.LLASFDE363
.LLASFDE363:
	.uaword	.LLASFDE363-.LLframe1
	.uaword	.LLFB363
	.uaword	.LLFE363-.LLFB363
	.byte	0x4
	.uaword	.LLCFI363-.LLFB363
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE363:
.LLSFDE365:
	.uaword	.LLEFDE365-.LLASFDE365
.LLASFDE365:
	.uaword	.LLASFDE365-.LLframe1
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
.LLEFDE365:
.LLSFDE367:
	.uaword	.LLEFDE367-.LLASFDE367
.LLASFDE367:
	.uaword	.LLASFDE367-.LLframe1
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
.LLEFDE367:
.LLSFDE369:
	.uaword	.LLEFDE369-.LLASFDE369
.LLASFDE369:
	.uaword	.LLASFDE369-.LLframe1
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
.LLEFDE369:
.LLSFDE371:
	.uaword	.LLEFDE371-.LLASFDE371
.LLASFDE371:
	.uaword	.LLASFDE371-.LLframe1
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
.LLEFDE371:
.LLSFDE373:
	.uaword	.LLEFDE373-.LLASFDE373
.LLASFDE373:
	.uaword	.LLASFDE373-.LLframe1
	.uaword	.LLFB373
	.uaword	.LLFE373-.LLFB373
	.byte	0x4
	.uaword	.LLCFI373-.LLFB373
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE373:
.LLSFDE375:
	.uaword	.LLEFDE375-.LLASFDE375
.LLASFDE375:
	.uaword	.LLASFDE375-.LLframe1
	.uaword	.LLFB375
	.uaword	.LLFE375-.LLFB375
	.byte	0x4
	.uaword	.LLCFI375-.LLFB375
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE375:
.LLSFDE377:
	.uaword	.LLEFDE377-.LLASFDE377
.LLASFDE377:
	.uaword	.LLASFDE377-.LLframe1
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
.LLEFDE377:
.LLSFDE379:
	.uaword	.LLEFDE379-.LLASFDE379
.LLASFDE379:
	.uaword	.LLASFDE379-.LLframe1
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
.LLEFDE379:
.LLSFDE381:
	.uaword	.LLEFDE381-.LLASFDE381
.LLASFDE381:
	.uaword	.LLASFDE381-.LLframe1
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
.LLEFDE381:
.LLSFDE383:
	.uaword	.LLEFDE383-.LLASFDE383
.LLASFDE383:
	.uaword	.LLASFDE383-.LLframe1
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
.LLEFDE383:
.LLSFDE385:
	.uaword	.LLEFDE385-.LLASFDE385
.LLASFDE385:
	.uaword	.LLASFDE385-.LLframe1
	.uaword	.LLFB385
	.uaword	.LLFE385-.LLFB385
	.byte	0x4
	.uaword	.LLCFI385-.LLFB385
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE385:
.LLSFDE387:
	.uaword	.LLEFDE387-.LLASFDE387
.LLASFDE387:
	.uaword	.LLASFDE387-.LLframe1
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
.LLEFDE387:
.LLSFDE389:
	.uaword	.LLEFDE389-.LLASFDE389
.LLASFDE389:
	.uaword	.LLASFDE389-.LLframe1
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
.LLEFDE389:
.LLSFDE391:
	.uaword	.LLEFDE391-.LLASFDE391
.LLASFDE391:
	.uaword	.LLASFDE391-.LLframe1
	.uaword	.LLFB391
	.uaword	.LLFE391-.LLFB391
	.byte	0x4
	.uaword	.LLCFI391-.LLFB391
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE391:
.LLSFDE393:
	.uaword	.LLEFDE393-.LLASFDE393
.LLASFDE393:
	.uaword	.LLASFDE393-.LLframe1
	.uaword	.LLFB393
	.uaword	.LLFE393-.LLFB393
	.byte	0x4
	.uaword	.LLCFI393-.LLFB393
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE393:
.LLSFDE395:
	.uaword	.LLEFDE395-.LLASFDE395
.LLASFDE395:
	.uaword	.LLASFDE395-.LLframe1
	.uaword	.LLFB395
	.uaword	.LLFE395-.LLFB395
	.byte	0x4
	.uaword	.LLCFI395-.LLFB395
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE395:
.LLSFDE397:
	.uaword	.LLEFDE397-.LLASFDE397
.LLASFDE397:
	.uaword	.LLASFDE397-.LLframe1
	.uaword	.LLFB397
	.uaword	.LLFE397-.LLFB397
	.byte	0x4
	.uaword	.LLCFI397-.LLFB397
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE397:
.LLSFDE399:
	.uaword	.LLEFDE399-.LLASFDE399
.LLASFDE399:
	.uaword	.LLASFDE399-.LLframe1
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
.LLEFDE399:
.LLSFDE401:
	.uaword	.LLEFDE401-.LLASFDE401
.LLASFDE401:
	.uaword	.LLASFDE401-.LLframe1
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
.LLEFDE401:
.LLSFDE403:
	.uaword	.LLEFDE403-.LLASFDE403
.LLASFDE403:
	.uaword	.LLASFDE403-.LLframe1
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
.LLEFDE403:
.LLSFDE405:
	.uaword	.LLEFDE405-.LLASFDE405
.LLASFDE405:
	.uaword	.LLASFDE405-.LLframe1
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
.LLEFDE405:
.LLSFDE407:
	.uaword	.LLEFDE407-.LLASFDE407
.LLASFDE407:
	.uaword	.LLASFDE407-.LLframe1
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
.LLEFDE407:
.LLSFDE409:
	.uaword	.LLEFDE409-.LLASFDE409
.LLASFDE409:
	.uaword	.LLASFDE409-.LLframe1
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
.LLEFDE409:
.LLSFDE411:
	.uaword	.LLEFDE411-.LLASFDE411
.LLASFDE411:
	.uaword	.LLASFDE411-.LLframe1
	.uaword	.LLFB411
	.uaword	.LLFE411-.LLFB411
	.byte	0x4
	.uaword	.LLCFI411-.LLFB411
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE411:
.LLSFDE413:
	.uaword	.LLEFDE413-.LLASFDE413
.LLASFDE413:
	.uaword	.LLASFDE413-.LLframe1
	.uaword	.LLFB413
	.uaword	.LLFE413-.LLFB413
	.byte	0x4
	.uaword	.LLCFI413-.LLFB413
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE413:
.LLSFDE415:
	.uaword	.LLEFDE415-.LLASFDE415
.LLASFDE415:
	.uaword	.LLASFDE415-.LLframe1
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
.LLEFDE415:
.LLSFDE417:
	.uaword	.LLEFDE417-.LLASFDE417
.LLASFDE417:
	.uaword	.LLASFDE417-.LLframe1
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
.LLEFDE417:
.LLSFDE419:
	.uaword	.LLEFDE419-.LLASFDE419
.LLASFDE419:
	.uaword	.LLASFDE419-.LLframe1
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
.LLEFDE419:
.LLSFDE421:
	.uaword	.LLEFDE421-.LLASFDE421
.LLASFDE421:
	.uaword	.LLASFDE421-.LLframe1
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
.LLEFDE421:
.LLSFDE423:
	.uaword	.LLEFDE423-.LLASFDE423
.LLASFDE423:
	.uaword	.LLASFDE423-.LLframe1
	.uaword	.LLFB423
	.uaword	.LLFE423-.LLFB423
	.byte	0x4
	.uaword	.LLCFI423-.LLFB423
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE423:
.LLSFDE425:
	.uaword	.LLEFDE425-.LLASFDE425
.LLASFDE425:
	.uaword	.LLASFDE425-.LLframe1
	.uaword	.LLFB425
	.uaword	.LLFE425-.LLFB425
	.byte	0x4
	.uaword	.LLCFI425-.LLFB425
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE425:
.LLSFDE427:
	.uaword	.LLEFDE427-.LLASFDE427
.LLASFDE427:
	.uaword	.LLASFDE427-.LLframe1
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
.LLEFDE427:
.LLSFDE429:
	.uaword	.LLEFDE429-.LLASFDE429
.LLASFDE429:
	.uaword	.LLASFDE429-.LLframe1
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
.LLEFDE429:
.LLSFDE431:
	.uaword	.LLEFDE431-.LLASFDE431
.LLASFDE431:
	.uaword	.LLASFDE431-.LLframe1
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
.LLEFDE431:
.LLSFDE433:
	.uaword	.LLEFDE433-.LLASFDE433
.LLASFDE433:
	.uaword	.LLASFDE433-.LLframe1
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
.LLEFDE433:
.LLSFDE435:
	.uaword	.LLEFDE435-.LLASFDE435
.LLASFDE435:
	.uaword	.LLASFDE435-.LLframe1
	.uaword	.LLFB435
	.uaword	.LLFE435-.LLFB435
	.byte	0x4
	.uaword	.LLCFI435-.LLFB435
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE435:
.LLSFDE437:
	.uaword	.LLEFDE437-.LLASFDE437
.LLASFDE437:
	.uaword	.LLASFDE437-.LLframe1
	.uaword	.LLFB437
	.uaword	.LLFE437-.LLFB437
	.byte	0x4
	.uaword	.LLCFI437-.LLFB437
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE437:
.LLSFDE439:
	.uaword	.LLEFDE439-.LLASFDE439
.LLASFDE439:
	.uaword	.LLASFDE439-.LLframe1
	.uaword	.LLFB439
	.uaword	.LLFE439-.LLFB439
	.byte	0x4
	.uaword	.LLCFI439-.LLFB439
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE439:
.LLSFDE441:
	.uaword	.LLEFDE441-.LLASFDE441
.LLASFDE441:
	.uaword	.LLASFDE441-.LLframe1
	.uaword	.LLFB441
	.uaword	.LLFE441-.LLFB441
	.byte	0x4
	.uaword	.LLCFI441-.LLFB441
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE441:
.LLSFDE443:
	.uaword	.LLEFDE443-.LLASFDE443
.LLASFDE443:
	.uaword	.LLASFDE443-.LLframe1
	.uaword	.LLFB443
	.uaword	.LLFE443-.LLFB443
	.byte	0x4
	.uaword	.LLCFI443-.LLFB443
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE443:
.LLSFDE445:
	.uaword	.LLEFDE445-.LLASFDE445
.LLASFDE445:
	.uaword	.LLASFDE445-.LLframe1
	.uaword	.LLFB445
	.uaword	.LLFE445-.LLFB445
	.byte	0x4
	.uaword	.LLCFI445-.LLFB445
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE445:
.LLSFDE447:
	.uaword	.LLEFDE447-.LLASFDE447
.LLASFDE447:
	.uaword	.LLASFDE447-.LLframe1
	.uaword	.LLFB447
	.uaword	.LLFE447-.LLFB447
	.byte	0x4
	.uaword	.LLCFI447-.LLFB447
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE447:
.LLSFDE449:
	.uaword	.LLEFDE449-.LLASFDE449
.LLASFDE449:
	.uaword	.LLASFDE449-.LLframe1
	.uaword	.LLFB449
	.uaword	.LLFE449-.LLFB449
	.byte	0x4
	.uaword	.LLCFI449-.LLFB449
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE449:
.LLSFDE451:
	.uaword	.LLEFDE451-.LLASFDE451
.LLASFDE451:
	.uaword	.LLASFDE451-.LLframe1
	.uaword	.LLFB451
	.uaword	.LLFE451-.LLFB451
	.byte	0x4
	.uaword	.LLCFI451-.LLFB451
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE451:
.LLSFDE453:
	.uaword	.LLEFDE453-.LLASFDE453
.LLASFDE453:
	.uaword	.LLASFDE453-.LLframe1
	.uaword	.LLFB453
	.uaword	.LLFE453-.LLFB453
	.byte	0x4
	.uaword	.LLCFI453-.LLFB453
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE453:
.LLSFDE455:
	.uaword	.LLEFDE455-.LLASFDE455
.LLASFDE455:
	.uaword	.LLASFDE455-.LLframe1
	.uaword	.LLFB455
	.uaword	.LLFE455-.LLFB455
	.byte	0x4
	.uaword	.LLCFI455-.LLFB455
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE455:
.LLSFDE457:
	.uaword	.LLEFDE457-.LLASFDE457
.LLASFDE457:
	.uaword	.LLASFDE457-.LLframe1
	.uaword	.LLFB457
	.uaword	.LLFE457-.LLFB457
	.byte	0x4
	.uaword	.LLCFI457-.LLFB457
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE457:
.LLSFDE459:
	.uaword	.LLEFDE459-.LLASFDE459
.LLASFDE459:
	.uaword	.LLASFDE459-.LLframe1
	.uaword	.LLFB459
	.uaword	.LLFE459-.LLFB459
	.byte	0x4
	.uaword	.LLCFI459-.LLFB459
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE459:
.LLSFDE461:
	.uaword	.LLEFDE461-.LLASFDE461
.LLASFDE461:
	.uaword	.LLASFDE461-.LLframe1
	.uaword	.LLFB461
	.uaword	.LLFE461-.LLFB461
	.byte	0x4
	.uaword	.LLCFI461-.LLFB461
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE461:
.LLSFDE463:
	.uaword	.LLEFDE463-.LLASFDE463
.LLASFDE463:
	.uaword	.LLASFDE463-.LLframe1
	.uaword	.LLFB463
	.uaword	.LLFE463-.LLFB463
	.byte	0x4
	.uaword	.LLCFI463-.LLFB463
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE463:
.LLSFDE465:
	.uaword	.LLEFDE465-.LLASFDE465
.LLASFDE465:
	.uaword	.LLASFDE465-.LLframe1
	.uaword	.LLFB465
	.uaword	.LLFE465-.LLFB465
	.byte	0x4
	.uaword	.LLCFI465-.LLFB465
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE465:
.LLSFDE467:
	.uaword	.LLEFDE467-.LLASFDE467
.LLASFDE467:
	.uaword	.LLASFDE467-.LLframe1
	.uaword	.LLFB467
	.uaword	.LLFE467-.LLFB467
	.byte	0x4
	.uaword	.LLCFI467-.LLFB467
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE467:
.LLSFDE469:
	.uaword	.LLEFDE469-.LLASFDE469
.LLASFDE469:
	.uaword	.LLASFDE469-.LLframe1
	.uaword	.LLFB469
	.uaword	.LLFE469-.LLFB469
	.byte	0x4
	.uaword	.LLCFI469-.LLFB469
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE469:
.LLSFDE471:
	.uaword	.LLEFDE471-.LLASFDE471
.LLASFDE471:
	.uaword	.LLASFDE471-.LLframe1
	.uaword	.LLFB471
	.uaword	.LLFE471-.LLFB471
	.byte	0x4
	.uaword	.LLCFI471-.LLFB471
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE471:
.LLSFDE473:
	.uaword	.LLEFDE473-.LLASFDE473
.LLASFDE473:
	.uaword	.LLASFDE473-.LLframe1
	.uaword	.LLFB473
	.uaword	.LLFE473-.LLFB473
	.byte	0x4
	.uaword	.LLCFI473-.LLFB473
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE473:
.LLSFDE475:
	.uaword	.LLEFDE475-.LLASFDE475
.LLASFDE475:
	.uaword	.LLASFDE475-.LLframe1
	.uaword	.LLFB475
	.uaword	.LLFE475-.LLFB475
	.byte	0x4
	.uaword	.LLCFI475-.LLFB475
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE475:
.LLSFDE477:
	.uaword	.LLEFDE477-.LLASFDE477
.LLASFDE477:
	.uaword	.LLASFDE477-.LLframe1
	.uaword	.LLFB477
	.uaword	.LLFE477-.LLFB477
	.byte	0x4
	.uaword	.LLCFI477-.LLFB477
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE477:
.LLSFDE479:
	.uaword	.LLEFDE479-.LLASFDE479
.LLASFDE479:
	.uaword	.LLASFDE479-.LLframe1
	.uaword	.LLFB479
	.uaword	.LLFE479-.LLFB479
	.byte	0x4
	.uaword	.LLCFI479-.LLFB479
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE479:
.LLSFDE481:
	.uaword	.LLEFDE481-.LLASFDE481
.LLASFDE481:
	.uaword	.LLASFDE481-.LLframe1
	.uaword	.LLFB481
	.uaword	.LLFE481-.LLFB481
	.byte	0x4
	.uaword	.LLCFI481-.LLFB481
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE481:
.LLSFDE483:
	.uaword	.LLEFDE483-.LLASFDE483
.LLASFDE483:
	.uaword	.LLASFDE483-.LLframe1
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
.LLEFDE483:
.LLSFDE485:
	.uaword	.LLEFDE485-.LLASFDE485
.LLASFDE485:
	.uaword	.LLASFDE485-.LLframe1
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
.LLEFDE485:
.LLSFDE487:
	.uaword	.LLEFDE487-.LLASFDE487
.LLASFDE487:
	.uaword	.LLASFDE487-.LLframe1
	.uaword	.LLFB487
	.uaword	.LLFE487-.LLFB487
	.byte	0x4
	.uaword	.LLCFI487-.LLFB487
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE487:
.LLSFDE489:
	.uaword	.LLEFDE489-.LLASFDE489
.LLASFDE489:
	.uaword	.LLASFDE489-.LLframe1
	.uaword	.LLFB489
	.uaword	.LLFE489-.LLFB489
	.byte	0x4
	.uaword	.LLCFI489-.LLFB489
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE489:
.LLSFDE491:
	.uaword	.LLEFDE491-.LLASFDE491
.LLASFDE491:
	.uaword	.LLASFDE491-.LLframe1
	.uaword	.LLFB491
	.uaword	.LLFE491-.LLFB491
	.byte	0x4
	.uaword	.LLCFI491-.LLFB491
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE491:
.LLSFDE493:
	.uaword	.LLEFDE493-.LLASFDE493
.LLASFDE493:
	.uaword	.LLASFDE493-.LLframe1
	.uaword	.LLFB493
	.uaword	.LLFE493-.LLFB493
	.byte	0x4
	.uaword	.LLCFI493-.LLFB493
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE493:
.LLSFDE495:
	.uaword	.LLEFDE495-.LLASFDE495
.LLASFDE495:
	.uaword	.LLASFDE495-.LLframe1
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
.LLEFDE495:
.LLSFDE497:
	.uaword	.LLEFDE497-.LLASFDE497
.LLASFDE497:
	.uaword	.LLASFDE497-.LLframe1
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
.LLEFDE497:
.LLSFDE499:
	.uaword	.LLEFDE499-.LLASFDE499
.LLASFDE499:
	.uaword	.LLASFDE499-.LLframe1
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
.LLEFDE499:
.LLSFDE501:
	.uaword	.LLEFDE501-.LLASFDE501
.LLASFDE501:
	.uaword	.LLASFDE501-.LLframe1
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
.LLEFDE501:
.LLSFDE503:
	.uaword	.LLEFDE503-.LLASFDE503
.LLASFDE503:
	.uaword	.LLASFDE503-.LLframe1
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
.LLEFDE503:
.LLSFDE505:
	.uaword	.LLEFDE505-.LLASFDE505
.LLASFDE505:
	.uaword	.LLASFDE505-.LLframe1
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
.LLEFDE505:
.LLSFDE507:
	.uaword	.LLEFDE507-.LLASFDE507
.LLASFDE507:
	.uaword	.LLASFDE507-.LLframe1
	.uaword	.LLFB507
	.uaword	.LLFE507-.LLFB507
	.byte	0x4
	.uaword	.LLCFI507-.LLFB507
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE507:
.LLSFDE509:
	.uaword	.LLEFDE509-.LLASFDE509
.LLASFDE509:
	.uaword	.LLASFDE509-.LLframe1
	.uaword	.LLFB509
	.uaword	.LLFE509-.LLFB509
	.byte	0x4
	.uaword	.LLCFI509-.LLFB509
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE509:
.LLSFDE511:
	.uaword	.LLEFDE511-.LLASFDE511
.LLASFDE511:
	.uaword	.LLASFDE511-.LLframe1
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
.LLEFDE511:
.LLSFDE513:
	.uaword	.LLEFDE513-.LLASFDE513
.LLASFDE513:
	.uaword	.LLASFDE513-.LLframe1
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
.LLEFDE513:
.LLSFDE515:
	.uaword	.LLEFDE515-.LLASFDE515
.LLASFDE515:
	.uaword	.LLASFDE515-.LLframe1
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
.LLEFDE515:
.LLSFDE517:
	.uaword	.LLEFDE517-.LLASFDE517
.LLASFDE517:
	.uaword	.LLASFDE517-.LLframe1
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
.LLEFDE517:
.LLSFDE519:
	.uaword	.LLEFDE519-.LLASFDE519
.LLASFDE519:
	.uaword	.LLASFDE519-.LLframe1
	.uaword	.LLFB519
	.uaword	.LLFE519-.LLFB519
	.byte	0x4
	.uaword	.LLCFI519-.LLFB519
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE519:
.LLSFDE521:
	.uaword	.LLEFDE521-.LLASFDE521
.LLASFDE521:
	.uaword	.LLASFDE521-.LLframe1
	.uaword	.LLFB521
	.uaword	.LLFE521-.LLFB521
	.byte	0x4
	.uaword	.LLCFI521-.LLFB521
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE521:
.LLSFDE523:
	.uaword	.LLEFDE523-.LLASFDE523
.LLASFDE523:
	.uaword	.LLASFDE523-.LLframe1
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
.LLEFDE523:
.LLSFDE525:
	.uaword	.LLEFDE525-.LLASFDE525
.LLASFDE525:
	.uaword	.LLASFDE525-.LLframe1
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
.LLEFDE525:
.LLSFDE527:
	.uaword	.LLEFDE527-.LLASFDE527
.LLASFDE527:
	.uaword	.LLASFDE527-.LLframe1
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
.LLEFDE527:
.LLSFDE529:
	.uaword	.LLEFDE529-.LLASFDE529
.LLASFDE529:
	.uaword	.LLASFDE529-.LLframe1
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
.LLEFDE529:
.LLSFDE531:
	.uaword	.LLEFDE531-.LLASFDE531
.LLASFDE531:
	.uaword	.LLASFDE531-.LLframe1
	.uaword	.LLFB531
	.uaword	.LLFE531-.LLFB531
	.byte	0x4
	.uaword	.LLCFI531-.LLFB531
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE531:
.LLSFDE533:
	.uaword	.LLEFDE533-.LLASFDE533
.LLASFDE533:
	.uaword	.LLASFDE533-.LLframe1
	.uaword	.LLFB533
	.uaword	.LLFE533-.LLFB533
	.byte	0x4
	.uaword	.LLCFI533-.LLFB533
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE533:
.LLSFDE535:
	.uaword	.LLEFDE535-.LLASFDE535
.LLASFDE535:
	.uaword	.LLASFDE535-.LLframe1
	.uaword	.LLFB535
	.uaword	.LLFE535-.LLFB535
	.byte	0x4
	.uaword	.LLCFI535-.LLFB535
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE535:
.LLSFDE537:
	.uaword	.LLEFDE537-.LLASFDE537
.LLASFDE537:
	.uaword	.LLASFDE537-.LLframe1
	.uaword	.LLFB537
	.uaword	.LLFE537-.LLFB537
	.byte	0x4
	.uaword	.LLCFI537-.LLFB537
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE537:
.LLSFDE539:
	.uaword	.LLEFDE539-.LLASFDE539
.LLASFDE539:
	.uaword	.LLASFDE539-.LLframe1
	.uaword	.LLFB539
	.uaword	.LLFE539-.LLFB539
	.byte	0x4
	.uaword	.LLCFI539-.LLFB539
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE539:
.LLSFDE541:
	.uaword	.LLEFDE541-.LLASFDE541
.LLASFDE541:
	.uaword	.LLASFDE541-.LLframe1
	.uaword	.LLFB541
	.uaword	.LLFE541-.LLFB541
	.byte	0x4
	.uaword	.LLCFI541-.LLFB541
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE541:
.LLSFDE543:
	.uaword	.LLEFDE543-.LLASFDE543
.LLASFDE543:
	.uaword	.LLASFDE543-.LLframe1
	.uaword	.LLFB543
	.uaword	.LLFE543-.LLFB543
	.byte	0x4
	.uaword	.LLCFI543-.LLFB543
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE543:
.LLSFDE545:
	.uaword	.LLEFDE545-.LLASFDE545
.LLASFDE545:
	.uaword	.LLASFDE545-.LLframe1
	.uaword	.LLFB545
	.uaword	.LLFE545-.LLFB545
	.byte	0x4
	.uaword	.LLCFI545-.LLFB545
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE545:
.LLSFDE547:
	.uaword	.LLEFDE547-.LLASFDE547
.LLASFDE547:
	.uaword	.LLASFDE547-.LLframe1
	.uaword	.LLFB547
	.uaword	.LLFE547-.LLFB547
	.byte	0x4
	.uaword	.LLCFI547-.LLFB547
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE547:
.LLSFDE549:
	.uaword	.LLEFDE549-.LLASFDE549
.LLASFDE549:
	.uaword	.LLASFDE549-.LLframe1
	.uaword	.LLFB549
	.uaword	.LLFE549-.LLFB549
	.byte	0x4
	.uaword	.LLCFI549-.LLFB549
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE549:
.LLSFDE551:
	.uaword	.LLEFDE551-.LLASFDE551
.LLASFDE551:
	.uaword	.LLASFDE551-.LLframe1
	.uaword	.LLFB551
	.uaword	.LLFE551-.LLFB551
	.byte	0x4
	.uaword	.LLCFI551-.LLFB551
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE551:
.LLSFDE553:
	.uaword	.LLEFDE553-.LLASFDE553
.LLASFDE553:
	.uaword	.LLASFDE553-.LLframe1
	.uaword	.LLFB553
	.uaword	.LLFE553-.LLFB553
	.byte	0x4
	.uaword	.LLCFI553-.LLFB553
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE553:
.LLSFDE555:
	.uaword	.LLEFDE555-.LLASFDE555
.LLASFDE555:
	.uaword	.LLASFDE555-.LLframe1
	.uaword	.LLFB555
	.uaword	.LLFE555-.LLFB555
	.byte	0x4
	.uaword	.LLCFI555-.LLFB555
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE555:
.LLSFDE557:
	.uaword	.LLEFDE557-.LLASFDE557
.LLASFDE557:
	.uaword	.LLASFDE557-.LLframe1
	.uaword	.LLFB557
	.uaword	.LLFE557-.LLFB557
	.byte	0x4
	.uaword	.LLCFI557-.LLFB557
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE557:
.LLSFDE559:
	.uaword	.LLEFDE559-.LLASFDE559
.LLASFDE559:
	.uaword	.LLASFDE559-.LLframe1
	.uaword	.LLFB559
	.uaword	.LLFE559-.LLFB559
	.byte	0x4
	.uaword	.LLCFI559-.LLFB559
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE559:
.LLSFDE561:
	.uaword	.LLEFDE561-.LLASFDE561
.LLASFDE561:
	.uaword	.LLASFDE561-.LLframe1
	.uaword	.LLFB561
	.uaword	.LLFE561-.LLFB561
	.byte	0x4
	.uaword	.LLCFI561-.LLFB561
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE561:
.LLSFDE563:
	.uaword	.LLEFDE563-.LLASFDE563
.LLASFDE563:
	.uaword	.LLASFDE563-.LLframe1
	.uaword	.LLFB563
	.uaword	.LLFE563-.LLFB563
	.byte	0x4
	.uaword	.LLCFI563-.LLFB563
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE563:
.LLSFDE565:
	.uaword	.LLEFDE565-.LLASFDE565
.LLASFDE565:
	.uaword	.LLASFDE565-.LLframe1
	.uaword	.LLFB565
	.uaword	.LLFE565-.LLFB565
	.byte	0x4
	.uaword	.LLCFI565-.LLFB565
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE565:
.LLSFDE567:
	.uaword	.LLEFDE567-.LLASFDE567
.LLASFDE567:
	.uaword	.LLASFDE567-.LLframe1
	.uaword	.LLFB567
	.uaword	.LLFE567-.LLFB567
	.byte	0x4
	.uaword	.LLCFI567-.LLFB567
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE567:
.LLSFDE569:
	.uaword	.LLEFDE569-.LLASFDE569
.LLASFDE569:
	.uaword	.LLASFDE569-.LLframe1
	.uaword	.LLFB569
	.uaword	.LLFE569-.LLFB569
	.byte	0x4
	.uaword	.LLCFI569-.LLFB569
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE569:
.LLSFDE571:
	.uaword	.LLEFDE571-.LLASFDE571
.LLASFDE571:
	.uaword	.LLASFDE571-.LLframe1
	.uaword	.LLFB571
	.uaword	.LLFE571-.LLFB571
	.byte	0x4
	.uaword	.LLCFI571-.LLFB571
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE571:
.LLSFDE573:
	.uaword	.LLEFDE573-.LLASFDE573
.LLASFDE573:
	.uaword	.LLASFDE573-.LLframe1
	.uaword	.LLFB573
	.uaword	.LLFE573-.LLFB573
	.byte	0x4
	.uaword	.LLCFI573-.LLFB573
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE573:
.LLSFDE575:
	.uaword	.LLEFDE575-.LLASFDE575
.LLASFDE575:
	.uaword	.LLASFDE575-.LLframe1
	.uaword	.LLFB575
	.uaword	.LLFE575-.LLFB575
	.byte	0x4
	.uaword	.LLCFI575-.LLFB575
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE575:
.LLSFDE577:
	.uaword	.LLEFDE577-.LLASFDE577
.LLASFDE577:
	.uaword	.LLASFDE577-.LLframe1
	.uaword	.LLFB577
	.uaword	.LLFE577-.LLFB577
	.byte	0x4
	.uaword	.LLCFI577-.LLFB577
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE577:
.LLSFDE579:
	.uaword	.LLEFDE579-.LLASFDE579
.LLASFDE579:
	.uaword	.LLASFDE579-.LLframe1
	.uaword	.LLFB579
	.uaword	.LLFE579-.LLFB579
	.byte	0x4
	.uaword	.LLCFI579-.LLFB579
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE579:
.LLSFDE581:
	.uaword	.LLEFDE581-.LLASFDE581
.LLASFDE581:
	.uaword	.LLASFDE581-.LLframe1
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
.LLEFDE581:
.LLSFDE583:
	.uaword	.LLEFDE583-.LLASFDE583
.LLASFDE583:
	.uaword	.LLASFDE583-.LLframe1
	.uaword	.LLFB583
	.uaword	.LLFE583-.LLFB583
	.byte	0x4
	.uaword	.LLCFI583-.LLFB583
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE583:
.LLSFDE585:
	.uaword	.LLEFDE585-.LLASFDE585
.LLASFDE585:
	.uaword	.LLASFDE585-.LLframe1
	.uaword	.LLFB585
	.uaword	.LLFE585-.LLFB585
	.byte	0x4
	.uaword	.LLCFI585-.LLFB585
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE585:
.LLSFDE587:
	.uaword	.LLEFDE587-.LLASFDE587
.LLASFDE587:
	.uaword	.LLASFDE587-.LLframe1
	.uaword	.LLFB587
	.uaword	.LLFE587-.LLFB587
	.byte	0x4
	.uaword	.LLCFI587-.LLFB587
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE587:
.LLSFDE589:
	.uaword	.LLEFDE589-.LLASFDE589
.LLASFDE589:
	.uaword	.LLASFDE589-.LLframe1
	.uaword	.LLFB589
	.uaword	.LLFE589-.LLFB589
	.byte	0x4
	.uaword	.LLCFI589-.LLFB589
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE589:
.LLSFDE591:
	.uaword	.LLEFDE591-.LLASFDE591
.LLASFDE591:
	.uaword	.LLASFDE591-.LLframe1
	.uaword	.LLFB591
	.uaword	.LLFE591-.LLFB591
	.byte	0x4
	.uaword	.LLCFI591-.LLFB591
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE591:
.LLSFDE593:
	.uaword	.LLEFDE593-.LLASFDE593
.LLASFDE593:
	.uaword	.LLASFDE593-.LLframe1
	.uaword	.LLFB593
	.uaword	.LLFE593-.LLFB593
	.byte	0x4
	.uaword	.LLCFI593-.LLFB593
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE593:
.LLSFDE595:
	.uaword	.LLEFDE595-.LLASFDE595
.LLASFDE595:
	.uaword	.LLASFDE595-.LLframe1
	.uaword	.LLFB595
	.uaword	.LLFE595-.LLFB595
	.byte	0x4
	.uaword	.LLCFI595-.LLFB595
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE595:
.LLSFDE597:
	.uaword	.LLEFDE597-.LLASFDE597
.LLASFDE597:
	.uaword	.LLASFDE597-.LLframe1
	.uaword	.LLFB597
	.uaword	.LLFE597-.LLFB597
	.byte	0x4
	.uaword	.LLCFI597-.LLFB597
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE597:
.LLSFDE599:
	.uaword	.LLEFDE599-.LLASFDE599
.LLASFDE599:
	.uaword	.LLASFDE599-.LLframe1
	.uaword	.LLFB599
	.uaword	.LLFE599-.LLFB599
	.byte	0x4
	.uaword	.LLCFI599-.LLFB599
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE599:
.LLSFDE601:
	.uaword	.LLEFDE601-.LLASFDE601
.LLASFDE601:
	.uaword	.LLASFDE601-.LLframe1
	.uaword	.LLFB601
	.uaword	.LLFE601-.LLFB601
	.byte	0x4
	.uaword	.LLCFI601-.LLFB601
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE601:
.LLSFDE603:
	.uaword	.LLEFDE603-.LLASFDE603
.LLASFDE603:
	.uaword	.LLASFDE603-.LLframe1
	.uaword	.LLFB603
	.uaword	.LLFE603-.LLFB603
	.byte	0x4
	.uaword	.LLCFI603-.LLFB603
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE603:
.LLSFDE605:
	.uaword	.LLEFDE605-.LLASFDE605
.LLASFDE605:
	.uaword	.LLASFDE605-.LLframe1
	.uaword	.LLFB605
	.uaword	.LLFE605-.LLFB605
	.byte	0x4
	.uaword	.LLCFI605-.LLFB605
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE605:
.LLSFDE607:
	.uaword	.LLEFDE607-.LLASFDE607
.LLASFDE607:
	.uaword	.LLASFDE607-.LLframe1
	.uaword	.LLFB607
	.uaword	.LLFE607-.LLFB607
	.byte	0x4
	.uaword	.LLCFI607-.LLFB607
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE607:
.LLSFDE609:
	.uaword	.LLEFDE609-.LLASFDE609
.LLASFDE609:
	.uaword	.LLASFDE609-.LLframe1
	.uaword	.LLFB609
	.uaword	.LLFE609-.LLFB609
	.byte	0x4
	.uaword	.LLCFI609-.LLFB609
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE609:
.LLSFDE611:
	.uaword	.LLEFDE611-.LLASFDE611
.LLASFDE611:
	.uaword	.LLASFDE611-.LLframe1
	.uaword	.LLFB611
	.uaword	.LLFE611-.LLFB611
	.byte	0x4
	.uaword	.LLCFI611-.LLFB611
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE611:
.LLSFDE613:
	.uaword	.LLEFDE613-.LLASFDE613
.LLASFDE613:
	.uaword	.LLASFDE613-.LLframe1
	.uaword	.LLFB613
	.uaword	.LLFE613-.LLFB613
	.byte	0x4
	.uaword	.LLCFI613-.LLFB613
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE613:
.LLSFDE615:
	.uaword	.LLEFDE615-.LLASFDE615
.LLASFDE615:
	.uaword	.LLASFDE615-.LLframe1
	.uaword	.LLFB615
	.uaword	.LLFE615-.LLFB615
	.byte	0x4
	.uaword	.LLCFI615-.LLFB615
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE615:
.LLSFDE617:
	.uaword	.LLEFDE617-.LLASFDE617
.LLASFDE617:
	.uaword	.LLASFDE617-.LLframe1
	.uaword	.LLFB617
	.uaword	.LLFE617-.LLFB617
	.byte	0x4
	.uaword	.LLCFI617-.LLFB617
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE617:
.LLSFDE619:
	.uaword	.LLEFDE619-.LLASFDE619
.LLASFDE619:
	.uaword	.LLASFDE619-.LLframe1
	.uaword	.LLFB619
	.uaword	.LLFE619-.LLFB619
	.byte	0x4
	.uaword	.LLCFI619-.LLFB619
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE619:
.LLSFDE621:
	.uaword	.LLEFDE621-.LLASFDE621
.LLASFDE621:
	.uaword	.LLASFDE621-.LLframe1
	.uaword	.LLFB621
	.uaword	.LLFE621-.LLFB621
	.byte	0x4
	.uaword	.LLCFI621-.LLFB621
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE621:
.LLSFDE623:
	.uaword	.LLEFDE623-.LLASFDE623
.LLASFDE623:
	.uaword	.LLASFDE623-.LLframe1
	.uaword	.LLFB623
	.uaword	.LLFE623-.LLFB623
	.byte	0x4
	.uaword	.LLCFI623-.LLFB623
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE623:
.LLSFDE625:
	.uaword	.LLEFDE625-.LLASFDE625
.LLASFDE625:
	.uaword	.LLASFDE625-.LLframe1
	.uaword	.LLFB625
	.uaword	.LLFE625-.LLFB625
	.byte	0x4
	.uaword	.LLCFI625-.LLFB625
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE625:
.LLSFDE627:
	.uaword	.LLEFDE627-.LLASFDE627
.LLASFDE627:
	.uaword	.LLASFDE627-.LLframe1
	.uaword	.LLFB627
	.uaword	.LLFE627-.LLFB627
	.byte	0x4
	.uaword	.LLCFI627-.LLFB627
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE627:
.LLSFDE629:
	.uaword	.LLEFDE629-.LLASFDE629
.LLASFDE629:
	.uaword	.LLASFDE629-.LLframe1
	.uaword	.LLFB629
	.uaword	.LLFE629-.LLFB629
	.byte	0x4
	.uaword	.LLCFI629-.LLFB629
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE629:
.LLSFDE631:
	.uaword	.LLEFDE631-.LLASFDE631
.LLASFDE631:
	.uaword	.LLASFDE631-.LLframe1
	.uaword	.LLFB631
	.uaword	.LLFE631-.LLFB631
	.byte	0x4
	.uaword	.LLCFI631-.LLFB631
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE631:
.LLSFDE633:
	.uaword	.LLEFDE633-.LLASFDE633
.LLASFDE633:
	.uaword	.LLASFDE633-.LLframe1
	.uaword	.LLFB633
	.uaword	.LLFE633-.LLFB633
	.byte	0x4
	.uaword	.LLCFI633-.LLFB633
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE633:
.LLSFDE635:
	.uaword	.LLEFDE635-.LLASFDE635
.LLASFDE635:
	.uaword	.LLASFDE635-.LLframe1
	.uaword	.LLFB635
	.uaword	.LLFE635-.LLFB635
	.byte	0x4
	.uaword	.LLCFI635-.LLFB635
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE635:
.LLSFDE637:
	.uaword	.LLEFDE637-.LLASFDE637
.LLASFDE637:
	.uaword	.LLASFDE637-.LLframe1
	.uaword	.LLFB637
	.uaword	.LLFE637-.LLFB637
	.byte	0x4
	.uaword	.LLCFI637-.LLFB637
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE637:
.LLSFDE639:
	.uaword	.LLEFDE639-.LLASFDE639
.LLASFDE639:
	.uaword	.LLASFDE639-.LLframe1
	.uaword	.LLFB639
	.uaword	.LLFE639-.LLFB639
	.byte	0x4
	.uaword	.LLCFI639-.LLFB639
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE639:
.LLSFDE641:
	.uaword	.LLEFDE641-.LLASFDE641
.LLASFDE641:
	.uaword	.LLASFDE641-.LLframe1
	.uaword	.LLFB641
	.uaword	.LLFE641-.LLFB641
	.byte	0x4
	.uaword	.LLCFI641-.LLFB641
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE641:
.LLSFDE643:
	.uaword	.LLEFDE643-.LLASFDE643
.LLASFDE643:
	.uaword	.LLASFDE643-.LLframe1
	.uaword	.LLFB643
	.uaword	.LLFE643-.LLFB643
	.byte	0x4
	.uaword	.LLCFI643-.LLFB643
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE643:
.LLSFDE645:
	.uaword	.LLEFDE645-.LLASFDE645
.LLASFDE645:
	.uaword	.LLASFDE645-.LLframe1
	.uaword	.LLFB645
	.uaword	.LLFE645-.LLFB645
	.byte	0x4
	.uaword	.LLCFI645-.LLFB645
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE645:
.LLSFDE647:
	.uaword	.LLEFDE647-.LLASFDE647
.LLASFDE647:
	.uaword	.LLASFDE647-.LLframe1
	.uaword	.LLFB647
	.uaword	.LLFE647-.LLFB647
	.byte	0x4
	.uaword	.LLCFI647-.LLFB647
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE647:
.LLSFDE649:
	.uaword	.LLEFDE649-.LLASFDE649
.LLASFDE649:
	.uaword	.LLASFDE649-.LLframe1
	.uaword	.LLFB649
	.uaword	.LLFE649-.LLFB649
	.byte	0x4
	.uaword	.LLCFI649-.LLFB649
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE649:
.LLSFDE651:
	.uaword	.LLEFDE651-.LLASFDE651
.LLASFDE651:
	.uaword	.LLASFDE651-.LLframe1
	.uaword	.LLFB651
	.uaword	.LLFE651-.LLFB651
	.byte	0x4
	.uaword	.LLCFI651-.LLFB651
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE651:
.LLSFDE653:
	.uaword	.LLEFDE653-.LLASFDE653
.LLASFDE653:
	.uaword	.LLASFDE653-.LLframe1
	.uaword	.LLFB653
	.uaword	.LLFE653-.LLFB653
	.byte	0x4
	.uaword	.LLCFI653-.LLFB653
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE653:
.LLSFDE655:
	.uaword	.LLEFDE655-.LLASFDE655
.LLASFDE655:
	.uaword	.LLASFDE655-.LLframe1
	.uaword	.LLFB655
	.uaword	.LLFE655-.LLFB655
	.byte	0x4
	.uaword	.LLCFI655-.LLFB655
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE655:
.LLSFDE657:
	.uaword	.LLEFDE657-.LLASFDE657
.LLASFDE657:
	.uaword	.LLASFDE657-.LLframe1
	.uaword	.LLFB657
	.uaword	.LLFE657-.LLFB657
	.byte	0x4
	.uaword	.LLCFI657-.LLFB657
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE657:
.LLSFDE659:
	.uaword	.LLEFDE659-.LLASFDE659
.LLASFDE659:
	.uaword	.LLASFDE659-.LLframe1
	.uaword	.LLFB659
	.uaword	.LLFE659-.LLFB659
	.byte	0x4
	.uaword	.LLCFI659-.LLFB659
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE659:
.LLSFDE661:
	.uaword	.LLEFDE661-.LLASFDE661
.LLASFDE661:
	.uaword	.LLASFDE661-.LLframe1
	.uaword	.LLFB661
	.uaword	.LLFE661-.LLFB661
	.byte	0x4
	.uaword	.LLCFI661-.LLFB661
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE661:
.LLSFDE663:
	.uaword	.LLEFDE663-.LLASFDE663
.LLASFDE663:
	.uaword	.LLASFDE663-.LLframe1
	.uaword	.LLFB663
	.uaword	.LLFE663-.LLFB663
	.byte	0x4
	.uaword	.LLCFI663-.LLFB663
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE663:
.LLSFDE665:
	.uaword	.LLEFDE665-.LLASFDE665
.LLASFDE665:
	.uaword	.LLASFDE665-.LLframe1
	.uaword	.LLFB665
	.uaword	.LLFE665-.LLFB665
	.byte	0x4
	.uaword	.LLCFI665-.LLFB665
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE665:
.LLSFDE667:
	.uaword	.LLEFDE667-.LLASFDE667
.LLASFDE667:
	.uaword	.LLASFDE667-.LLframe1
	.uaword	.LLFB667
	.uaword	.LLFE667-.LLFB667
	.byte	0x4
	.uaword	.LLCFI667-.LLFB667
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE667:
.LLSFDE669:
	.uaword	.LLEFDE669-.LLASFDE669
.LLASFDE669:
	.uaword	.LLASFDE669-.LLframe1
	.uaword	.LLFB669
	.uaword	.LLFE669-.LLFB669
	.byte	0x4
	.uaword	.LLCFI669-.LLFB669
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE669:
.LLSFDE671:
	.uaword	.LLEFDE671-.LLASFDE671
.LLASFDE671:
	.uaword	.LLASFDE671-.LLframe1
	.uaword	.LLFB671
	.uaword	.LLFE671-.LLFB671
	.byte	0x4
	.uaword	.LLCFI671-.LLFB671
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE671:
.LLSFDE673:
	.uaword	.LLEFDE673-.LLASFDE673
.LLASFDE673:
	.uaword	.LLASFDE673-.LLframe1
	.uaword	.LLFB673
	.uaword	.LLFE673-.LLFB673
	.byte	0x4
	.uaword	.LLCFI673-.LLFB673
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE673:
.LLSFDE675:
	.uaword	.LLEFDE675-.LLASFDE675
.LLASFDE675:
	.uaword	.LLASFDE675-.LLframe1
	.uaword	.LLFB675
	.uaword	.LLFE675-.LLFB675
	.byte	0x4
	.uaword	.LLCFI675-.LLFB675
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE675:
.LLSFDE677:
	.uaword	.LLEFDE677-.LLASFDE677
.LLASFDE677:
	.uaword	.LLASFDE677-.LLframe1
	.uaword	.LLFB677
	.uaword	.LLFE677-.LLFB677
	.byte	0x4
	.uaword	.LLCFI677-.LLFB677
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE677:
.LLSFDE679:
	.uaword	.LLEFDE679-.LLASFDE679
.LLASFDE679:
	.uaword	.LLASFDE679-.LLframe1
	.uaword	.LLFB679
	.uaword	.LLFE679-.LLFB679
	.byte	0x4
	.uaword	.LLCFI679-.LLFB679
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE679:
.LLSFDE681:
	.uaword	.LLEFDE681-.LLASFDE681
.LLASFDE681:
	.uaword	.LLASFDE681-.LLframe1
	.uaword	.LLFB681
	.uaword	.LLFE681-.LLFB681
	.byte	0x4
	.uaword	.LLCFI681-.LLFB681
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE681:
.LLSFDE683:
	.uaword	.LLEFDE683-.LLASFDE683
.LLASFDE683:
	.uaword	.LLASFDE683-.LLframe1
	.uaword	.LLFB683
	.uaword	.LLFE683-.LLFB683
	.byte	0x4
	.uaword	.LLCFI683-.LLFB683
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE683:
.LLSFDE685:
	.uaword	.LLEFDE685-.LLASFDE685
.LLASFDE685:
	.uaword	.LLASFDE685-.LLframe1
	.uaword	.LLFB685
	.uaword	.LLFE685-.LLFB685
	.byte	0x4
	.uaword	.LLCFI685-.LLFB685
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE685:
.LLSFDE687:
	.uaword	.LLEFDE687-.LLASFDE687
.LLASFDE687:
	.uaword	.LLASFDE687-.LLframe1
	.uaword	.LLFB687
	.uaword	.LLFE687-.LLFB687
	.byte	0x4
	.uaword	.LLCFI687-.LLFB687
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE687:
.LLSFDE689:
	.uaword	.LLEFDE689-.LLASFDE689
.LLASFDE689:
	.uaword	.LLASFDE689-.LLframe1
	.uaword	.LLFB689
	.uaword	.LLFE689-.LLFB689
	.byte	0x4
	.uaword	.LLCFI689-.LLFB689
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE689:
.LLSFDE691:
	.uaword	.LLEFDE691-.LLASFDE691
.LLASFDE691:
	.uaword	.LLASFDE691-.LLframe1
	.uaword	.LLFB691
	.uaword	.LLFE691-.LLFB691
	.byte	0x4
	.uaword	.LLCFI691-.LLFB691
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE691:
.LLSFDE693:
	.uaword	.LLEFDE693-.LLASFDE693
.LLASFDE693:
	.uaword	.LLASFDE693-.LLframe1
	.uaword	.LLFB693
	.uaword	.LLFE693-.LLFB693
	.byte	0x4
	.uaword	.LLCFI693-.LLFB693
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE693:
.LLSFDE695:
	.uaword	.LLEFDE695-.LLASFDE695
.LLASFDE695:
	.uaword	.LLASFDE695-.LLframe1
	.uaword	.LLFB695
	.uaword	.LLFE695-.LLFB695
	.byte	0x4
	.uaword	.LLCFI695-.LLFB695
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE695:
.LLSFDE697:
	.uaword	.LLEFDE697-.LLASFDE697
.LLASFDE697:
	.uaword	.LLASFDE697-.LLframe1
	.uaword	.LLFB697
	.uaword	.LLFE697-.LLFB697
	.byte	0x4
	.uaword	.LLCFI697-.LLFB697
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE697:
.LLSFDE699:
	.uaword	.LLEFDE699-.LLASFDE699
.LLASFDE699:
	.uaword	.LLASFDE699-.LLframe1
	.uaword	.LLFB699
	.uaword	.LLFE699-.LLFB699
	.byte	0x4
	.uaword	.LLCFI699-.LLFB699
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE699:
.LLSFDE701:
	.uaword	.LLEFDE701-.LLASFDE701
.LLASFDE701:
	.uaword	.LLASFDE701-.LLframe1
	.uaword	.LLFB701
	.uaword	.LLFE701-.LLFB701
	.byte	0x4
	.uaword	.LLCFI701-.LLFB701
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE701:
.LLSFDE703:
	.uaword	.LLEFDE703-.LLASFDE703
.LLASFDE703:
	.uaword	.LLASFDE703-.LLframe1
	.uaword	.LLFB703
	.uaword	.LLFE703-.LLFB703
	.byte	0x4
	.uaword	.LLCFI703-.LLFB703
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE703:
.LLSFDE705:
	.uaword	.LLEFDE705-.LLASFDE705
.LLASFDE705:
	.uaword	.LLASFDE705-.LLframe1
	.uaword	.LLFB705
	.uaword	.LLFE705-.LLFB705
	.byte	0x4
	.uaword	.LLCFI705-.LLFB705
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE705:
.LLSFDE707:
	.uaword	.LLEFDE707-.LLASFDE707
.LLASFDE707:
	.uaword	.LLASFDE707-.LLframe1
	.uaword	.LLFB707
	.uaword	.LLFE707-.LLFB707
	.byte	0x4
	.uaword	.LLCFI707-.LLFB707
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE707:
.LLSFDE709:
	.uaword	.LLEFDE709-.LLASFDE709
.LLASFDE709:
	.uaword	.LLASFDE709-.LLframe1
	.uaword	.LLFB709
	.uaword	.LLFE709-.LLFB709
	.byte	0x4
	.uaword	.LLCFI709-.LLFB709
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE709:
.LLSFDE711:
	.uaword	.LLEFDE711-.LLASFDE711
.LLASFDE711:
	.uaword	.LLASFDE711-.LLframe1
	.uaword	.LLFB711
	.uaword	.LLFE711-.LLFB711
	.byte	0x4
	.uaword	.LLCFI711-.LLFB711
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE711:
.LLSFDE713:
	.uaword	.LLEFDE713-.LLASFDE713
.LLASFDE713:
	.uaword	.LLASFDE713-.LLframe1
	.uaword	.LLFB713
	.uaword	.LLFE713-.LLFB713
	.byte	0x4
	.uaword	.LLCFI713-.LLFB713
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE713:
.LLSFDE715:
	.uaword	.LLEFDE715-.LLASFDE715
.LLASFDE715:
	.uaword	.LLASFDE715-.LLframe1
	.uaword	.LLFB715
	.uaword	.LLFE715-.LLFB715
	.byte	0x4
	.uaword	.LLCFI715-.LLFB715
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE715:
.LLSFDE717:
	.uaword	.LLEFDE717-.LLASFDE717
.LLASFDE717:
	.uaword	.LLASFDE717-.LLframe1
	.uaword	.LLFB717
	.uaword	.LLFE717-.LLFB717
	.byte	0x4
	.uaword	.LLCFI717-.LLFB717
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE717:
.LLSFDE719:
	.uaword	.LLEFDE719-.LLASFDE719
.LLASFDE719:
	.uaword	.LLASFDE719-.LLframe1
	.uaword	.LLFB719
	.uaword	.LLFE719-.LLFB719
	.byte	0x4
	.uaword	.LLCFI719-.LLFB719
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE719:
.LLSFDE721:
	.uaword	.LLEFDE721-.LLASFDE721
.LLASFDE721:
	.uaword	.LLASFDE721-.LLframe1
	.uaword	.LLFB721
	.uaword	.LLFE721-.LLFB721
	.byte	0x4
	.uaword	.LLCFI721-.LLFB721
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE721:
.LLSFDE723:
	.uaword	.LLEFDE723-.LLASFDE723
.LLASFDE723:
	.uaword	.LLASFDE723-.LLframe1
	.uaword	.LLFB723
	.uaword	.LLFE723-.LLFB723
	.byte	0x4
	.uaword	.LLCFI723-.LLFB723
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE723:
.LLSFDE725:
	.uaword	.LLEFDE725-.LLASFDE725
.LLASFDE725:
	.uaword	.LLASFDE725-.LLframe1
	.uaword	.LLFB725
	.uaword	.LLFE725-.LLFB725
	.byte	0x4
	.uaword	.LLCFI725-.LLFB725
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE725:
.LLSFDE727:
	.uaword	.LLEFDE727-.LLASFDE727
.LLASFDE727:
	.uaword	.LLASFDE727-.LLframe1
	.uaword	.LLFB727
	.uaword	.LLFE727-.LLFB727
	.byte	0x4
	.uaword	.LLCFI727-.LLFB727
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE727:
.LLSFDE729:
	.uaword	.LLEFDE729-.LLASFDE729
.LLASFDE729:
	.uaword	.LLASFDE729-.LLframe1
	.uaword	.LLFB729
	.uaword	.LLFE729-.LLFB729
	.byte	0x4
	.uaword	.LLCFI729-.LLFB729
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE729:
.LLSFDE731:
	.uaword	.LLEFDE731-.LLASFDE731
.LLASFDE731:
	.uaword	.LLASFDE731-.LLframe1
	.uaword	.LLFB731
	.uaword	.LLFE731-.LLFB731
	.byte	0x4
	.uaword	.LLCFI731-.LLFB731
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE731:
.LLSFDE733:
	.uaword	.LLEFDE733-.LLASFDE733
.LLASFDE733:
	.uaword	.LLASFDE733-.LLframe1
	.uaword	.LLFB733
	.uaword	.LLFE733-.LLFB733
	.byte	0x4
	.uaword	.LLCFI733-.LLFB733
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE733:
.LLSFDE735:
	.uaword	.LLEFDE735-.LLASFDE735
.LLASFDE735:
	.uaword	.LLASFDE735-.LLframe1
	.uaword	.LLFB735
	.uaword	.LLFE735-.LLFB735
	.byte	0x4
	.uaword	.LLCFI735-.LLFB735
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE735:
.LLSFDE737:
	.uaword	.LLEFDE737-.LLASFDE737
.LLASFDE737:
	.uaword	.LLASFDE737-.LLframe1
	.uaword	.LLFB737
	.uaword	.LLFE737-.LLFB737
	.byte	0x4
	.uaword	.LLCFI737-.LLFB737
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE737:
.LLSFDE739:
	.uaword	.LLEFDE739-.LLASFDE739
.LLASFDE739:
	.uaword	.LLASFDE739-.LLframe1
	.uaword	.LLFB739
	.uaword	.LLFE739-.LLFB739
	.byte	0x4
	.uaword	.LLCFI739-.LLFB739
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE739:
.LLSFDE741:
	.uaword	.LLEFDE741-.LLASFDE741
.LLASFDE741:
	.uaword	.LLASFDE741-.LLframe1
	.uaword	.LLFB741
	.uaword	.LLFE741-.LLFB741
	.byte	0x4
	.uaword	.LLCFI741-.LLFB741
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE741:
.LLSFDE743:
	.uaword	.LLEFDE743-.LLASFDE743
.LLASFDE743:
	.uaword	.LLASFDE743-.LLframe1
	.uaword	.LLFB743
	.uaword	.LLFE743-.LLFB743
	.byte	0x4
	.uaword	.LLCFI743-.LLFB743
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE743:
.LLSFDE745:
	.uaword	.LLEFDE745-.LLASFDE745
.LLASFDE745:
	.uaword	.LLASFDE745-.LLframe1
	.uaword	.LLFB745
	.uaword	.LLFE745-.LLFB745
	.byte	0x4
	.uaword	.LLCFI745-.LLFB745
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE745:
.LLSFDE747:
	.uaword	.LLEFDE747-.LLASFDE747
.LLASFDE747:
	.uaword	.LLASFDE747-.LLframe1
	.uaword	.LLFB747
	.uaword	.LLFE747-.LLFB747
	.byte	0x4
	.uaword	.LLCFI747-.LLFB747
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE747:
.LLSFDE749:
	.uaword	.LLEFDE749-.LLASFDE749
.LLASFDE749:
	.uaword	.LLASFDE749-.LLframe1
	.uaword	.LLFB749
	.uaword	.LLFE749-.LLFB749
	.byte	0x4
	.uaword	.LLCFI749-.LLFB749
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE749:
.LLSFDE751:
	.uaword	.LLEFDE751-.LLASFDE751
.LLASFDE751:
	.uaword	.LLASFDE751-.LLframe1
	.uaword	.LLFB751
	.uaword	.LLFE751-.LLFB751
	.byte	0x4
	.uaword	.LLCFI751-.LLFB751
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE751:
.LLSFDE753:
	.uaword	.LLEFDE753-.LLASFDE753
.LLASFDE753:
	.uaword	.LLASFDE753-.LLframe1
	.uaword	.LLFB753
	.uaword	.LLFE753-.LLFB753
	.byte	0x4
	.uaword	.LLCFI753-.LLFB753
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE753:
.LLSFDE755:
	.uaword	.LLEFDE755-.LLASFDE755
.LLASFDE755:
	.uaword	.LLASFDE755-.LLframe1
	.uaword	.LLFB755
	.uaword	.LLFE755-.LLFB755
	.byte	0x4
	.uaword	.LLCFI755-.LLFB755
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE755:
.LLSFDE757:
	.uaword	.LLEFDE757-.LLASFDE757
.LLASFDE757:
	.uaword	.LLASFDE757-.LLframe1
	.uaword	.LLFB757
	.uaword	.LLFE757-.LLFB757
	.byte	0x4
	.uaword	.LLCFI757-.LLFB757
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE757:
.LLSFDE759:
	.uaword	.LLEFDE759-.LLASFDE759
.LLASFDE759:
	.uaword	.LLASFDE759-.LLframe1
	.uaword	.LLFB759
	.uaword	.LLFE759-.LLFB759
	.byte	0x4
	.uaword	.LLCFI759-.LLFB759
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE759:
.LLSFDE761:
	.uaword	.LLEFDE761-.LLASFDE761
.LLASFDE761:
	.uaword	.LLASFDE761-.LLframe1
	.uaword	.LLFB761
	.uaword	.LLFE761-.LLFB761
	.byte	0x4
	.uaword	.LLCFI761-.LLFB761
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE761:
.LLSFDE763:
	.uaword	.LLEFDE763-.LLASFDE763
.LLASFDE763:
	.uaword	.LLASFDE763-.LLframe1
	.uaword	.LLFB763
	.uaword	.LLFE763-.LLFB763
	.byte	0x4
	.uaword	.LLCFI763-.LLFB763
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE763:
.LLSFDE765:
	.uaword	.LLEFDE765-.LLASFDE765
.LLASFDE765:
	.uaword	.LLASFDE765-.LLframe1
	.uaword	.LLFB765
	.uaword	.LLFE765-.LLFB765
	.byte	0x4
	.uaword	.LLCFI765-.LLFB765
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE765:
.LLSFDE767:
	.uaword	.LLEFDE767-.LLASFDE767
.LLASFDE767:
	.uaword	.LLASFDE767-.LLframe1
	.uaword	.LLFB767
	.uaword	.LLFE767-.LLFB767
	.byte	0x4
	.uaword	.LLCFI767-.LLFB767
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE767:
.LLSFDE769:
	.uaword	.LLEFDE769-.LLASFDE769
.LLASFDE769:
	.uaword	.LLASFDE769-.LLframe1
	.uaword	.LLFB769
	.uaword	.LLFE769-.LLFB769
	.byte	0x4
	.uaword	.LLCFI769-.LLFB769
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE769:
.LLSFDE771:
	.uaword	.LLEFDE771-.LLASFDE771
.LLASFDE771:
	.uaword	.LLASFDE771-.LLframe1
	.uaword	.LLFB771
	.uaword	.LLFE771-.LLFB771
	.byte	0x4
	.uaword	.LLCFI771-.LLFB771
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE771:
.LLSFDE773:
	.uaword	.LLEFDE773-.LLASFDE773
.LLASFDE773:
	.uaword	.LLASFDE773-.LLframe1
	.uaword	.LLFB773
	.uaword	.LLFE773-.LLFB773
	.byte	0x4
	.uaword	.LLCFI773-.LLFB773
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE773:
.LLSFDE775:
	.uaword	.LLEFDE775-.LLASFDE775
.LLASFDE775:
	.uaword	.LLASFDE775-.LLframe1
	.uaword	.LLFB775
	.uaword	.LLFE775-.LLFB775
	.byte	0x4
	.uaword	.LLCFI775-.LLFB775
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE775:
.LLSFDE777:
	.uaword	.LLEFDE777-.LLASFDE777
.LLASFDE777:
	.uaword	.LLASFDE777-.LLframe1
	.uaword	.LLFB777
	.uaword	.LLFE777-.LLFB777
	.byte	0x4
	.uaword	.LLCFI777-.LLFB777
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE777:
.LLSFDE779:
	.uaword	.LLEFDE779-.LLASFDE779
.LLASFDE779:
	.uaword	.LLASFDE779-.LLframe1
	.uaword	.LLFB779
	.uaword	.LLFE779-.LLFB779
	.byte	0x4
	.uaword	.LLCFI779-.LLFB779
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE779:
.LLSFDE781:
	.uaword	.LLEFDE781-.LLASFDE781
.LLASFDE781:
	.uaword	.LLASFDE781-.LLframe1
	.uaword	.LLFB781
	.uaword	.LLFE781-.LLFB781
	.byte	0x4
	.uaword	.LLCFI781-.LLFB781
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE781:
.LLSFDE783:
	.uaword	.LLEFDE783-.LLASFDE783
.LLASFDE783:
	.uaword	.LLASFDE783-.LLframe1
	.uaword	.LLFB783
	.uaword	.LLFE783-.LLFB783
	.byte	0x4
	.uaword	.LLCFI783-.LLFB783
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE783:
.LLSFDE785:
	.uaword	.LLEFDE785-.LLASFDE785
.LLASFDE785:
	.uaword	.LLASFDE785-.LLframe1
	.uaword	.LLFB785
	.uaword	.LLFE785-.LLFB785
	.byte	0x4
	.uaword	.LLCFI785-.LLFB785
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE785:
.LLSFDE787:
	.uaword	.LLEFDE787-.LLASFDE787
.LLASFDE787:
	.uaword	.LLASFDE787-.LLframe1
	.uaword	.LLFB787
	.uaword	.LLFE787-.LLFB787
	.byte	0x4
	.uaword	.LLCFI787-.LLFB787
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE787:
.LLSFDE789:
	.uaword	.LLEFDE789-.LLASFDE789
.LLASFDE789:
	.uaword	.LLASFDE789-.LLframe1
	.uaword	.LLFB789
	.uaword	.LLFE789-.LLFB789
	.byte	0x4
	.uaword	.LLCFI789-.LLFB789
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE789:
.LLSFDE791:
	.uaword	.LLEFDE791-.LLASFDE791
.LLASFDE791:
	.uaword	.LLASFDE791-.LLframe1
	.uaword	.LLFB791
	.uaword	.LLFE791-.LLFB791
	.byte	0x4
	.uaword	.LLCFI791-.LLFB791
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE791:
.LLSFDE793:
	.uaword	.LLEFDE793-.LLASFDE793
.LLASFDE793:
	.uaword	.LLASFDE793-.LLframe1
	.uaword	.LLFB793
	.uaword	.LLFE793-.LLFB793
	.byte	0x4
	.uaword	.LLCFI793-.LLFB793
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE793:
.LLSFDE795:
	.uaword	.LLEFDE795-.LLASFDE795
.LLASFDE795:
	.uaword	.LLASFDE795-.LLframe1
	.uaword	.LLFB795
	.uaword	.LLFE795-.LLFB795
	.byte	0x4
	.uaword	.LLCFI795-.LLFB795
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE795:
.LLSFDE797:
	.uaword	.LLEFDE797-.LLASFDE797
.LLASFDE797:
	.uaword	.LLASFDE797-.LLframe1
	.uaword	.LLFB797
	.uaword	.LLFE797-.LLFB797
	.byte	0x4
	.uaword	.LLCFI797-.LLFB797
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE797:
.LLSFDE799:
	.uaword	.LLEFDE799-.LLASFDE799
.LLASFDE799:
	.uaword	.LLASFDE799-.LLframe1
	.uaword	.LLFB799
	.uaword	.LLFE799-.LLFB799
	.byte	0x4
	.uaword	.LLCFI799-.LLFB799
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE799:
.LLSFDE801:
	.uaword	.LLEFDE801-.LLASFDE801
.LLASFDE801:
	.uaword	.LLASFDE801-.LLframe1
	.uaword	.LLFB801
	.uaword	.LLFE801-.LLFB801
	.byte	0x4
	.uaword	.LLCFI801-.LLFB801
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE801:
.LLSFDE803:
	.uaword	.LLEFDE803-.LLASFDE803
.LLASFDE803:
	.uaword	.LLASFDE803-.LLframe1
	.uaword	.LLFB803
	.uaword	.LLFE803-.LLFB803
	.byte	0x4
	.uaword	.LLCFI803-.LLFB803
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE803:
.LLSFDE805:
	.uaword	.LLEFDE805-.LLASFDE805
.LLASFDE805:
	.uaword	.LLASFDE805-.LLframe1
	.uaword	.LLFB805
	.uaword	.LLFE805-.LLFB805
	.byte	0x4
	.uaword	.LLCFI805-.LLFB805
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE805:
.LLSFDE807:
	.uaword	.LLEFDE807-.LLASFDE807
.LLASFDE807:
	.uaword	.LLASFDE807-.LLframe1
	.uaword	.LLFB807
	.uaword	.LLFE807-.LLFB807
	.byte	0x4
	.uaword	.LLCFI807-.LLFB807
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE807:
.LLSFDE809:
	.uaword	.LLEFDE809-.LLASFDE809
.LLASFDE809:
	.uaword	.LLASFDE809-.LLframe1
	.uaword	.LLFB809
	.uaword	.LLFE809-.LLFB809
	.byte	0x4
	.uaword	.LLCFI809-.LLFB809
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE809:
.LLSFDE811:
	.uaword	.LLEFDE811-.LLASFDE811
.LLASFDE811:
	.uaword	.LLASFDE811-.LLframe1
	.uaword	.LLFB811
	.uaword	.LLFE811-.LLFB811
	.byte	0x4
	.uaword	.LLCFI811-.LLFB811
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE811:
.LLSFDE813:
	.uaword	.LLEFDE813-.LLASFDE813
.LLASFDE813:
	.uaword	.LLASFDE813-.LLframe1
	.uaword	.LLFB813
	.uaword	.LLFE813-.LLFB813
	.byte	0x4
	.uaword	.LLCFI813-.LLFB813
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE813:
.LLSFDE815:
	.uaword	.LLEFDE815-.LLASFDE815
.LLASFDE815:
	.uaword	.LLASFDE815-.LLframe1
	.uaword	.LLFB815
	.uaword	.LLFE815-.LLFB815
	.byte	0x4
	.uaword	.LLCFI815-.LLFB815
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE815:
.LLSFDE817:
	.uaword	.LLEFDE817-.LLASFDE817
.LLASFDE817:
	.uaword	.LLASFDE817-.LLframe1
	.uaword	.LLFB817
	.uaword	.LLFE817-.LLFB817
	.byte	0x4
	.uaword	.LLCFI817-.LLFB817
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE817:
.LLSFDE819:
	.uaword	.LLEFDE819-.LLASFDE819
.LLASFDE819:
	.uaword	.LLASFDE819-.LLframe1
	.uaword	.LLFB819
	.uaword	.LLFE819-.LLFB819
	.byte	0x4
	.uaword	.LLCFI819-.LLFB819
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE819:
.LLSFDE821:
	.uaword	.LLEFDE821-.LLASFDE821
.LLASFDE821:
	.uaword	.LLASFDE821-.LLframe1
	.uaword	.LLFB821
	.uaword	.LLFE821-.LLFB821
	.byte	0x4
	.uaword	.LLCFI821-.LLFB821
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE821:
.LLSFDE823:
	.uaword	.LLEFDE823-.LLASFDE823
.LLASFDE823:
	.uaword	.LLASFDE823-.LLframe1
	.uaword	.LLFB823
	.uaword	.LLFE823-.LLFB823
	.byte	0x4
	.uaword	.LLCFI823-.LLFB823
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE823:
.LLSFDE825:
	.uaword	.LLEFDE825-.LLASFDE825
.LLASFDE825:
	.uaword	.LLASFDE825-.LLframe1
	.uaword	.LLFB825
	.uaword	.LLFE825-.LLFB825
	.byte	0x4
	.uaword	.LLCFI825-.LLFB825
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE825:
.LLSFDE827:
	.uaword	.LLEFDE827-.LLASFDE827
.LLASFDE827:
	.uaword	.LLASFDE827-.LLframe1
	.uaword	.LLFB827
	.uaword	.LLFE827-.LLFB827
	.byte	0x4
	.uaword	.LLCFI827-.LLFB827
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE827:
.LLSFDE829:
	.uaword	.LLEFDE829-.LLASFDE829
.LLASFDE829:
	.uaword	.LLASFDE829-.LLframe1
	.uaword	.LLFB829
	.uaword	.LLFE829-.LLFB829
	.byte	0x4
	.uaword	.LLCFI829-.LLFB829
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE829:
.LLSFDE831:
	.uaword	.LLEFDE831-.LLASFDE831
.LLASFDE831:
	.uaword	.LLASFDE831-.LLframe1
	.uaword	.LLFB831
	.uaword	.LLFE831-.LLFB831
	.byte	0x4
	.uaword	.LLCFI831-.LLFB831
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE831:
.LLSFDE833:
	.uaword	.LLEFDE833-.LLASFDE833
.LLASFDE833:
	.uaword	.LLASFDE833-.LLframe1
	.uaword	.LLFB833
	.uaword	.LLFE833-.LLFB833
	.byte	0x4
	.uaword	.LLCFI833-.LLFB833
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE833:
.LLSFDE835:
	.uaword	.LLEFDE835-.LLASFDE835
.LLASFDE835:
	.uaword	.LLASFDE835-.LLframe1
	.uaword	.LLFB835
	.uaword	.LLFE835-.LLFB835
	.byte	0x4
	.uaword	.LLCFI835-.LLFB835
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE835:
.LLSFDE837:
	.uaword	.LLEFDE837-.LLASFDE837
.LLASFDE837:
	.uaword	.LLASFDE837-.LLframe1
	.uaword	.LLFB837
	.uaword	.LLFE837-.LLFB837
	.byte	0x4
	.uaword	.LLCFI837-.LLFB837
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE837:
.LLSFDE839:
	.uaword	.LLEFDE839-.LLASFDE839
.LLASFDE839:
	.uaword	.LLASFDE839-.LLframe1
	.uaword	.LLFB839
	.uaword	.LLFE839-.LLFB839
	.byte	0x4
	.uaword	.LLCFI839-.LLFB839
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE839:
.LLSFDE841:
	.uaword	.LLEFDE841-.LLASFDE841
.LLASFDE841:
	.uaword	.LLASFDE841-.LLframe1
	.uaword	.LLFB841
	.uaword	.LLFE841-.LLFB841
	.byte	0x4
	.uaword	.LLCFI841-.LLFB841
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE841:
.LLSFDE843:
	.uaword	.LLEFDE843-.LLASFDE843
.LLASFDE843:
	.uaword	.LLASFDE843-.LLframe1
	.uaword	.LLFB843
	.uaword	.LLFE843-.LLFB843
	.byte	0x4
	.uaword	.LLCFI843-.LLFB843
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE843:
.LLSFDE845:
	.uaword	.LLEFDE845-.LLASFDE845
.LLASFDE845:
	.uaword	.LLASFDE845-.LLframe1
	.uaword	.LLFB845
	.uaword	.LLFE845-.LLFB845
	.byte	0x4
	.uaword	.LLCFI845-.LLFB845
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE845:
.LLSFDE847:
	.uaword	.LLEFDE847-.LLASFDE847
.LLASFDE847:
	.uaword	.LLASFDE847-.LLframe1
	.uaword	.LLFB847
	.uaword	.LLFE847-.LLFB847
	.byte	0x4
	.uaword	.LLCFI847-.LLFB847
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE847:
.LLSFDE849:
	.uaword	.LLEFDE849-.LLASFDE849
.LLASFDE849:
	.uaword	.LLASFDE849-.LLframe1
	.uaword	.LLFB849
	.uaword	.LLFE849-.LLFB849
	.byte	0x4
	.uaword	.LLCFI849-.LLFB849
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE849:
.LLSFDE851:
	.uaword	.LLEFDE851-.LLASFDE851
.LLASFDE851:
	.uaword	.LLASFDE851-.LLframe1
	.uaword	.LLFB851
	.uaword	.LLFE851-.LLFB851
	.byte	0x4
	.uaword	.LLCFI851-.LLFB851
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE851:
.LLSFDE853:
	.uaword	.LLEFDE853-.LLASFDE853
.LLASFDE853:
	.uaword	.LLASFDE853-.LLframe1
	.uaword	.LLFB853
	.uaword	.LLFE853-.LLFB853
	.byte	0x4
	.uaword	.LLCFI853-.LLFB853
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE853:
.LLSFDE855:
	.uaword	.LLEFDE855-.LLASFDE855
.LLASFDE855:
	.uaword	.LLASFDE855-.LLframe1
	.uaword	.LLFB855
	.uaword	.LLFE855-.LLFB855
	.byte	0x4
	.uaword	.LLCFI855-.LLFB855
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE855:
.LLSFDE857:
	.uaword	.LLEFDE857-.LLASFDE857
.LLASFDE857:
	.uaword	.LLASFDE857-.LLframe1
	.uaword	.LLFB857
	.uaword	.LLFE857-.LLFB857
	.byte	0x4
	.uaword	.LLCFI857-.LLFB857
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE857:
.LLSFDE859:
	.uaword	.LLEFDE859-.LLASFDE859
.LLASFDE859:
	.uaword	.LLASFDE859-.LLframe1
	.uaword	.LLFB859
	.uaword	.LLFE859-.LLFB859
	.byte	0x4
	.uaword	.LLCFI859-.LLFB859
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE859:
.LLSFDE861:
	.uaword	.LLEFDE861-.LLASFDE861
.LLASFDE861:
	.uaword	.LLASFDE861-.LLframe1
	.uaword	.LLFB861
	.uaword	.LLFE861-.LLFB861
	.byte	0x4
	.uaword	.LLCFI861-.LLFB861
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE861:
.LLSFDE863:
	.uaword	.LLEFDE863-.LLASFDE863
.LLASFDE863:
	.uaword	.LLASFDE863-.LLframe1
	.uaword	.LLFB863
	.uaword	.LLFE863-.LLFB863
	.byte	0x4
	.uaword	.LLCFI863-.LLFB863
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE863:
.LLSFDE865:
	.uaword	.LLEFDE865-.LLASFDE865
.LLASFDE865:
	.uaword	.LLASFDE865-.LLframe1
	.uaword	.LLFB865
	.uaword	.LLFE865-.LLFB865
	.byte	0x4
	.uaword	.LLCFI865-.LLFB865
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE865:
.LLSFDE867:
	.uaword	.LLEFDE867-.LLASFDE867
.LLASFDE867:
	.uaword	.LLASFDE867-.LLframe1
	.uaword	.LLFB867
	.uaword	.LLFE867-.LLFB867
	.byte	0x4
	.uaword	.LLCFI867-.LLFB867
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE867:
.LLSFDE869:
	.uaword	.LLEFDE869-.LLASFDE869
.LLASFDE869:
	.uaword	.LLASFDE869-.LLframe1
	.uaword	.LLFB869
	.uaword	.LLFE869-.LLFB869
	.byte	0x4
	.uaword	.LLCFI869-.LLFB869
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE869:
.LLSFDE871:
	.uaword	.LLEFDE871-.LLASFDE871
.LLASFDE871:
	.uaword	.LLASFDE871-.LLframe1
	.uaword	.LLFB871
	.uaword	.LLFE871-.LLFB871
	.byte	0x4
	.uaword	.LLCFI871-.LLFB871
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE871:
.LLSFDE873:
	.uaword	.LLEFDE873-.LLASFDE873
.LLASFDE873:
	.uaword	.LLASFDE873-.LLframe1
	.uaword	.LLFB873
	.uaword	.LLFE873-.LLFB873
	.byte	0x4
	.uaword	.LLCFI873-.LLFB873
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE873:
.LLSFDE875:
	.uaword	.LLEFDE875-.LLASFDE875
.LLASFDE875:
	.uaword	.LLASFDE875-.LLframe1
	.uaword	.LLFB875
	.uaword	.LLFE875-.LLFB875
	.byte	0x4
	.uaword	.LLCFI875-.LLFB875
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE875:
.LLSFDE877:
	.uaword	.LLEFDE877-.LLASFDE877
.LLASFDE877:
	.uaword	.LLASFDE877-.LLframe1
	.uaword	.LLFB877
	.uaword	.LLFE877-.LLFB877
	.byte	0x4
	.uaword	.LLCFI877-.LLFB877
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE877:
.LLSFDE879:
	.uaword	.LLEFDE879-.LLASFDE879
.LLASFDE879:
	.uaword	.LLASFDE879-.LLframe1
	.uaword	.LLFB879
	.uaword	.LLFE879-.LLFB879
	.byte	0x4
	.uaword	.LLCFI879-.LLFB879
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE879:
.LLSFDE881:
	.uaword	.LLEFDE881-.LLASFDE881
.LLASFDE881:
	.uaword	.LLASFDE881-.LLframe1
	.uaword	.LLFB881
	.uaword	.LLFE881-.LLFB881
	.byte	0x4
	.uaword	.LLCFI881-.LLFB881
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE881:
.LLSFDE883:
	.uaword	.LLEFDE883-.LLASFDE883
.LLASFDE883:
	.uaword	.LLASFDE883-.LLframe1
	.uaword	.LLFB883
	.uaword	.LLFE883-.LLFB883
	.byte	0x4
	.uaword	.LLCFI883-.LLFB883
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE883:
.LLSFDE885:
	.uaword	.LLEFDE885-.LLASFDE885
.LLASFDE885:
	.uaword	.LLASFDE885-.LLframe1
	.uaword	.LLFB885
	.uaword	.LLFE885-.LLFB885
	.byte	0x4
	.uaword	.LLCFI885-.LLFB885
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE885:
.LLSFDE887:
	.uaword	.LLEFDE887-.LLASFDE887
.LLASFDE887:
	.uaword	.LLASFDE887-.LLframe1
	.uaword	.LLFB887
	.uaword	.LLFE887-.LLFB887
	.byte	0x4
	.uaword	.LLCFI887-.LLFB887
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE887:
.LLSFDE889:
	.uaword	.LLEFDE889-.LLASFDE889
.LLASFDE889:
	.uaword	.LLASFDE889-.LLframe1
	.uaword	.LLFB889
	.uaword	.LLFE889-.LLFB889
	.byte	0x4
	.uaword	.LLCFI889-.LLFB889
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE889:
.LLSFDE891:
	.uaword	.LLEFDE891-.LLASFDE891
.LLASFDE891:
	.uaword	.LLASFDE891-.LLframe1
	.uaword	.LLFB891
	.uaword	.LLFE891-.LLFB891
	.byte	0x4
	.uaword	.LLCFI891-.LLFB891
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE891:
.LLSFDE893:
	.uaword	.LLEFDE893-.LLASFDE893
.LLASFDE893:
	.uaword	.LLASFDE893-.LLframe1
	.uaword	.LLFB893
	.uaword	.LLFE893-.LLFB893
	.byte	0x4
	.uaword	.LLCFI893-.LLFB893
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE893:
.LLSFDE895:
	.uaword	.LLEFDE895-.LLASFDE895
.LLASFDE895:
	.uaword	.LLASFDE895-.LLframe1
	.uaword	.LLFB895
	.uaword	.LLFE895-.LLFB895
	.byte	0x4
	.uaword	.LLCFI895-.LLFB895
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE895:
.LLSFDE897:
	.uaword	.LLEFDE897-.LLASFDE897
.LLASFDE897:
	.uaword	.LLASFDE897-.LLframe1
	.uaword	.LLFB897
	.uaword	.LLFE897-.LLFB897
	.byte	0x4
	.uaword	.LLCFI897-.LLFB897
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE897:
.LLSFDE899:
	.uaword	.LLEFDE899-.LLASFDE899
.LLASFDE899:
	.uaword	.LLASFDE899-.LLframe1
	.uaword	.LLFB899
	.uaword	.LLFE899-.LLFB899
	.byte	0x4
	.uaword	.LLCFI899-.LLFB899
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE899:
.LLSFDE901:
	.uaword	.LLEFDE901-.LLASFDE901
.LLASFDE901:
	.uaword	.LLASFDE901-.LLframe1
	.uaword	.LLFB901
	.uaword	.LLFE901-.LLFB901
	.byte	0x4
	.uaword	.LLCFI901-.LLFB901
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE901:
.LLSFDE903:
	.uaword	.LLEFDE903-.LLASFDE903
.LLASFDE903:
	.uaword	.LLASFDE903-.LLframe1
	.uaword	.LLFB903
	.uaword	.LLFE903-.LLFB903
	.byte	0x4
	.uaword	.LLCFI903-.LLFB903
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE903:
.LLSFDE905:
	.uaword	.LLEFDE905-.LLASFDE905
.LLASFDE905:
	.uaword	.LLASFDE905-.LLframe1
	.uaword	.LLFB905
	.uaword	.LLFE905-.LLFB905
	.byte	0x4
	.uaword	.LLCFI905-.LLFB905
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE905:
.LLSFDE907:
	.uaword	.LLEFDE907-.LLASFDE907
.LLASFDE907:
	.uaword	.LLASFDE907-.LLframe1
	.uaword	.LLFB907
	.uaword	.LLFE907-.LLFB907
	.byte	0x4
	.uaword	.LLCFI907-.LLFB907
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE907:
.LLSFDE909:
	.uaword	.LLEFDE909-.LLASFDE909
.LLASFDE909:
	.uaword	.LLASFDE909-.LLframe1
	.uaword	.LLFB909
	.uaword	.LLFE909-.LLFB909
	.byte	0x4
	.uaword	.LLCFI909-.LLFB909
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE909:
.LLSFDE911:
	.uaword	.LLEFDE911-.LLASFDE911
.LLASFDE911:
	.uaword	.LLASFDE911-.LLframe1
	.uaword	.LLFB911
	.uaword	.LLFE911-.LLFB911
	.byte	0x4
	.uaword	.LLCFI911-.LLFB911
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE911:
.LLSFDE913:
	.uaword	.LLEFDE913-.LLASFDE913
.LLASFDE913:
	.uaword	.LLASFDE913-.LLframe1
	.uaword	.LLFB913
	.uaword	.LLFE913-.LLFB913
	.byte	0x4
	.uaword	.LLCFI913-.LLFB913
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE913:
.LLSFDE915:
	.uaword	.LLEFDE915-.LLASFDE915
.LLASFDE915:
	.uaword	.LLASFDE915-.LLframe1
	.uaword	.LLFB915
	.uaword	.LLFE915-.LLFB915
	.byte	0x4
	.uaword	.LLCFI915-.LLFB915
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE915:
.LLSFDE917:
	.uaword	.LLEFDE917-.LLASFDE917
.LLASFDE917:
	.uaword	.LLASFDE917-.LLframe1
	.uaword	.LLFB917
	.uaword	.LLFE917-.LLFB917
	.byte	0x4
	.uaword	.LLCFI917-.LLFB917
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE917:
.LLSFDE919:
	.uaword	.LLEFDE919-.LLASFDE919
.LLASFDE919:
	.uaword	.LLASFDE919-.LLframe1
	.uaword	.LLFB919
	.uaword	.LLFE919-.LLFB919
	.byte	0x4
	.uaword	.LLCFI919-.LLFB919
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE919:
.LLSFDE921:
	.uaword	.LLEFDE921-.LLASFDE921
.LLASFDE921:
	.uaword	.LLASFDE921-.LLframe1
	.uaword	.LLFB921
	.uaword	.LLFE921-.LLFB921
	.byte	0x4
	.uaword	.LLCFI921-.LLFB921
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE921:
.LLSFDE923:
	.uaword	.LLEFDE923-.LLASFDE923
.LLASFDE923:
	.uaword	.LLASFDE923-.LLframe1
	.uaword	.LLFB923
	.uaword	.LLFE923-.LLFB923
	.byte	0x4
	.uaword	.LLCFI923-.LLFB923
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE923:
.LLSFDE925:
	.uaword	.LLEFDE925-.LLASFDE925
.LLASFDE925:
	.uaword	.LLASFDE925-.LLframe1
	.uaword	.LLFB925
	.uaword	.LLFE925-.LLFB925
	.byte	0x4
	.uaword	.LLCFI925-.LLFB925
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE925:
.LLSFDE927:
	.uaword	.LLEFDE927-.LLASFDE927
.LLASFDE927:
	.uaword	.LLASFDE927-.LLframe1
	.uaword	.LLFB927
	.uaword	.LLFE927-.LLFB927
	.byte	0x4
	.uaword	.LLCFI927-.LLFB927
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE927:
.LLSFDE929:
	.uaword	.LLEFDE929-.LLASFDE929
.LLASFDE929:
	.uaword	.LLASFDE929-.LLframe1
	.uaword	.LLFB929
	.uaword	.LLFE929-.LLFB929
	.byte	0x4
	.uaword	.LLCFI929-.LLFB929
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE929:
.LLSFDE931:
	.uaword	.LLEFDE931-.LLASFDE931
.LLASFDE931:
	.uaword	.LLASFDE931-.LLframe1
	.uaword	.LLFB931
	.uaword	.LLFE931-.LLFB931
	.byte	0x4
	.uaword	.LLCFI931-.LLFB931
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE931:
.LLSFDE933:
	.uaword	.LLEFDE933-.LLASFDE933
.LLASFDE933:
	.uaword	.LLASFDE933-.LLframe1
	.uaword	.LLFB933
	.uaword	.LLFE933-.LLFB933
	.byte	0x4
	.uaword	.LLCFI933-.LLFB933
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE933:
.LLSFDE935:
	.uaword	.LLEFDE935-.LLASFDE935
.LLASFDE935:
	.uaword	.LLASFDE935-.LLframe1
	.uaword	.LLFB935
	.uaword	.LLFE935-.LLFB935
	.byte	0x4
	.uaword	.LLCFI935-.LLFB935
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE935:
.LLSFDE937:
	.uaword	.LLEFDE937-.LLASFDE937
.LLASFDE937:
	.uaword	.LLASFDE937-.LLframe1
	.uaword	.LLFB937
	.uaword	.LLFE937-.LLFB937
	.byte	0x4
	.uaword	.LLCFI937-.LLFB937
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE937:
.LLSFDE939:
	.uaword	.LLEFDE939-.LLASFDE939
.LLASFDE939:
	.uaword	.LLASFDE939-.LLframe1
	.uaword	.LLFB939
	.uaword	.LLFE939-.LLFB939
	.byte	0x4
	.uaword	.LLCFI939-.LLFB939
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE939:
.LLSFDE941:
	.uaword	.LLEFDE941-.LLASFDE941
.LLASFDE941:
	.uaword	.LLASFDE941-.LLframe1
	.uaword	.LLFB941
	.uaword	.LLFE941-.LLFB941
	.byte	0x4
	.uaword	.LLCFI941-.LLFB941
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE941:
.LLSFDE943:
	.uaword	.LLEFDE943-.LLASFDE943
.LLASFDE943:
	.uaword	.LLASFDE943-.LLframe1
	.uaword	.LLFB943
	.uaword	.LLFE943-.LLFB943
	.byte	0x4
	.uaword	.LLCFI943-.LLFB943
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE943:
.LLSFDE945:
	.uaword	.LLEFDE945-.LLASFDE945
.LLASFDE945:
	.uaword	.LLASFDE945-.LLframe1
	.uaword	.LLFB945
	.uaword	.LLFE945-.LLFB945
	.byte	0x4
	.uaword	.LLCFI945-.LLFB945
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE945:
.LLSFDE947:
	.uaword	.LLEFDE947-.LLASFDE947
.LLASFDE947:
	.uaword	.LLASFDE947-.LLframe1
	.uaword	.LLFB947
	.uaword	.LLFE947-.LLFB947
	.byte	0x4
	.uaword	.LLCFI947-.LLFB947
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE947:
.LLSFDE949:
	.uaword	.LLEFDE949-.LLASFDE949
.LLASFDE949:
	.uaword	.LLASFDE949-.LLframe1
	.uaword	.LLFB949
	.uaword	.LLFE949-.LLFB949
	.byte	0x4
	.uaword	.LLCFI949-.LLFB949
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE949:
.LLSFDE951:
	.uaword	.LLEFDE951-.LLASFDE951
.LLASFDE951:
	.uaword	.LLASFDE951-.LLframe1
	.uaword	.LLFB951
	.uaword	.LLFE951-.LLFB951
	.byte	0x4
	.uaword	.LLCFI951-.LLFB951
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE951:
.LLSFDE953:
	.uaword	.LLEFDE953-.LLASFDE953
.LLASFDE953:
	.uaword	.LLASFDE953-.LLframe1
	.uaword	.LLFB953
	.uaword	.LLFE953-.LLFB953
	.byte	0x4
	.uaword	.LLCFI953-.LLFB953
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE953:
.LLSFDE955:
	.uaword	.LLEFDE955-.LLASFDE955
.LLASFDE955:
	.uaword	.LLASFDE955-.LLframe1
	.uaword	.LLFB955
	.uaword	.LLFE955-.LLFB955
	.byte	0x4
	.uaword	.LLCFI955-.LLFB955
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE955:
.LLSFDE957:
	.uaword	.LLEFDE957-.LLASFDE957
.LLASFDE957:
	.uaword	.LLASFDE957-.LLframe1
	.uaword	.LLFB957
	.uaword	.LLFE957-.LLFB957
	.byte	0x4
	.uaword	.LLCFI957-.LLFB957
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE957:
.LLSFDE959:
	.uaword	.LLEFDE959-.LLASFDE959
.LLASFDE959:
	.uaword	.LLASFDE959-.LLframe1
	.uaword	.LLFB959
	.uaword	.LLFE959-.LLFB959
	.byte	0x4
	.uaword	.LLCFI959-.LLFB959
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE959:
.LLSFDE961:
	.uaword	.LLEFDE961-.LLASFDE961
.LLASFDE961:
	.uaword	.LLASFDE961-.LLframe1
	.uaword	.LLFB961
	.uaword	.LLFE961-.LLFB961
	.byte	0x4
	.uaword	.LLCFI961-.LLFB961
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE961:
.LLSFDE963:
	.uaword	.LLEFDE963-.LLASFDE963
.LLASFDE963:
	.uaword	.LLASFDE963-.LLframe1
	.uaword	.LLFB963
	.uaword	.LLFE963-.LLFB963
	.byte	0x4
	.uaword	.LLCFI963-.LLFB963
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE963:
.LLSFDE965:
	.uaword	.LLEFDE965-.LLASFDE965
.LLASFDE965:
	.uaword	.LLASFDE965-.LLframe1
	.uaword	.LLFB965
	.uaword	.LLFE965-.LLFB965
	.byte	0x4
	.uaword	.LLCFI965-.LLFB965
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE965:
.LLSFDE967:
	.uaword	.LLEFDE967-.LLASFDE967
.LLASFDE967:
	.uaword	.LLASFDE967-.LLframe1
	.uaword	.LLFB967
	.uaword	.LLFE967-.LLFB967
	.byte	0x4
	.uaword	.LLCFI967-.LLFB967
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE967:
.LLSFDE969:
	.uaword	.LLEFDE969-.LLASFDE969
.LLASFDE969:
	.uaword	.LLASFDE969-.LLframe1
	.uaword	.LLFB969
	.uaword	.LLFE969-.LLFB969
	.byte	0x4
	.uaword	.LLCFI969-.LLFB969
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE969:
.LLSFDE971:
	.uaword	.LLEFDE971-.LLASFDE971
.LLASFDE971:
	.uaword	.LLASFDE971-.LLframe1
	.uaword	.LLFB971
	.uaword	.LLFE971-.LLFB971
	.byte	0x4
	.uaword	.LLCFI971-.LLFB971
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE971:
.LLSFDE973:
	.uaword	.LLEFDE973-.LLASFDE973
.LLASFDE973:
	.uaword	.LLASFDE973-.LLframe1
	.uaword	.LLFB973
	.uaword	.LLFE973-.LLFB973
	.byte	0x4
	.uaword	.LLCFI973-.LLFB973
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE973:
.LLSFDE975:
	.uaword	.LLEFDE975-.LLASFDE975
.LLASFDE975:
	.uaword	.LLASFDE975-.LLframe1
	.uaword	.LLFB975
	.uaword	.LLFE975-.LLFB975
	.byte	0x4
	.uaword	.LLCFI975-.LLFB975
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE975:
.LLSFDE977:
	.uaword	.LLEFDE977-.LLASFDE977
.LLASFDE977:
	.uaword	.LLASFDE977-.LLframe1
	.uaword	.LLFB977
	.uaword	.LLFE977-.LLFB977
	.byte	0x4
	.uaword	.LLCFI977-.LLFB977
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE977:
.LLSFDE979:
	.uaword	.LLEFDE979-.LLASFDE979
.LLASFDE979:
	.uaword	.LLASFDE979-.LLframe1
	.uaword	.LLFB979
	.uaword	.LLFE979-.LLFB979
	.byte	0x4
	.uaword	.LLCFI979-.LLFB979
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE979:
.LLSFDE981:
	.uaword	.LLEFDE981-.LLASFDE981
.LLASFDE981:
	.uaword	.LLASFDE981-.LLframe1
	.uaword	.LLFB981
	.uaword	.LLFE981-.LLFB981
	.byte	0x4
	.uaword	.LLCFI981-.LLFB981
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE981:
.LLSFDE983:
	.uaword	.LLEFDE983-.LLASFDE983
.LLASFDE983:
	.uaword	.LLASFDE983-.LLframe1
	.uaword	.LLFB983
	.uaword	.LLFE983-.LLFB983
	.byte	0x4
	.uaword	.LLCFI983-.LLFB983
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE983:
.LLSFDE985:
	.uaword	.LLEFDE985-.LLASFDE985
.LLASFDE985:
	.uaword	.LLASFDE985-.LLframe1
	.uaword	.LLFB985
	.uaword	.LLFE985-.LLFB985
	.byte	0x4
	.uaword	.LLCFI985-.LLFB985
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE985:
.LLSFDE987:
	.uaword	.LLEFDE987-.LLASFDE987
.LLASFDE987:
	.uaword	.LLASFDE987-.LLframe1
	.uaword	.LLFB987
	.uaword	.LLFE987-.LLFB987
	.byte	0x4
	.uaword	.LLCFI987-.LLFB987
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE987:
.LLSFDE989:
	.uaword	.LLEFDE989-.LLASFDE989
.LLASFDE989:
	.uaword	.LLASFDE989-.LLframe1
	.uaword	.LLFB989
	.uaword	.LLFE989-.LLFB989
	.byte	0x4
	.uaword	.LLCFI989-.LLFB989
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE989:
.LLSFDE991:
	.uaword	.LLEFDE991-.LLASFDE991
.LLASFDE991:
	.uaword	.LLASFDE991-.LLframe1
	.uaword	.LLFB991
	.uaword	.LLFE991-.LLFB991
	.byte	0x4
	.uaword	.LLCFI991-.LLFB991
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE991:
.LLSFDE993:
	.uaword	.LLEFDE993-.LLASFDE993
.LLASFDE993:
	.uaword	.LLASFDE993-.LLframe1
	.uaword	.LLFB993
	.uaword	.LLFE993-.LLFB993
	.byte	0x4
	.uaword	.LLCFI993-.LLFB993
	.byte	0xd
	.byte	0x1e
	.byte	0x2d
	.byte	0x9
	.byte	0xf
	.byte	0x1f
	.align 4
.LLEFDE993:
	.ident	"GCC: (GNU) 4.5.1"
