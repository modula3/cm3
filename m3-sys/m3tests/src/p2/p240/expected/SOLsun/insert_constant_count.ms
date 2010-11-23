	.file	"insert_constant_count.mc"
	.section	".text"
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_0, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_0:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 32
	ble	%icc, .LL2
	 nop
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL2:
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	insert_constant_count__Word_insert_constant_count_0, .-insert_constant_count__Word_insert_constant_count_0
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_1, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_1:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 31
	ble	%icc, .LL4
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL4:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	insert_constant_count__Word_insert_constant_count_1, .-insert_constant_count__Word_insert_constant_count_1
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_2, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_2:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 30
	ble	%icc, .LL6
	 nop
	mov	1889, %o0
	call	_m3_fault, 0
	 nop
.LL6:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	3, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 3, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	insert_constant_count__Word_insert_constant_count_2, .-insert_constant_count__Word_insert_constant_count_2
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_3, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_3:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 29
	ble	%icc, .LL8
	 nop
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	7, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 7, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	insert_constant_count__Word_insert_constant_count_3, .-insert_constant_count__Word_insert_constant_count_3
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_4, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_4:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 28
	ble	%icc, .LL10
	 nop
	mov	2017, %o0
	call	_m3_fault, 0
	 nop
.LL10:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	15, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 15, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	insert_constant_count__Word_insert_constant_count_4, .-insert_constant_count__Word_insert_constant_count_4
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_5, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_5:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 27
	ble	%icc, .LL12
	 nop
	mov	2081, %o0
	call	_m3_fault, 0
	 nop
.LL12:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	31, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 31, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	insert_constant_count__Word_insert_constant_count_5, .-insert_constant_count__Word_insert_constant_count_5
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_6, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_6:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 26
	ble	%icc, .LL14
	 nop
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL14:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	63, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 63, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	insert_constant_count__Word_insert_constant_count_6, .-insert_constant_count__Word_insert_constant_count_6
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_7, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_7:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 25
	ble	%icc, .LL16
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL16:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	127, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 127, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	insert_constant_count__Word_insert_constant_count_7, .-insert_constant_count__Word_insert_constant_count_7
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_8, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_8:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 24
	ble	%icc, .LL18
	 nop
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL18:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	255, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 255, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	insert_constant_count__Word_insert_constant_count_8, .-insert_constant_count__Word_insert_constant_count_8
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_9, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_9:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 23
	ble	%icc, .LL20
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL20:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	511, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 511, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	insert_constant_count__Word_insert_constant_count_9, .-insert_constant_count__Word_insert_constant_count_9
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_10, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_10:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 22
	ble	%icc, .LL22
	 nop
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL22:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 1023, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	insert_constant_count__Word_insert_constant_count_10, .-insert_constant_count__Word_insert_constant_count_10
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_11, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_11:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 21
	ble	%icc, .LL24
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL24:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	2047, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 2047, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	insert_constant_count__Word_insert_constant_count_11, .-insert_constant_count__Word_insert_constant_count_11
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_12, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_12:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 20
	ble	%icc, .LL26
	 nop
	mov	2529, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	4095, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %g1
	and	%g1, 4095, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	insert_constant_count__Word_insert_constant_count_12, .-insert_constant_count__Word_insert_constant_count_12
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_13, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_13:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 19
	ble	%icc, .LL28
	 nop
	mov	2593, %o0
	call	_m3_fault, 0
	 nop
.LL28:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(7168), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	insert_constant_count__Word_insert_constant_count_13, .-insert_constant_count__Word_insert_constant_count_13
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_14, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_14:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 18
	ble	%icc, .LL30
	 nop
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL30:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(15360), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	insert_constant_count__Word_insert_constant_count_14, .-insert_constant_count__Word_insert_constant_count_14
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_15, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_15:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 17
	ble	%icc, .LL32
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL32:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	insert_constant_count__Word_insert_constant_count_15, .-insert_constant_count__Word_insert_constant_count_15
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_16, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 16
	ble	%icc, .LL34
	 nop
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL34:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	insert_constant_count__Word_insert_constant_count_16, .-insert_constant_count__Word_insert_constant_count_16
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_17, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_17:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 15
	ble	%icc, .LL36
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL36:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(130048), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	insert_constant_count__Word_insert_constant_count_17, .-insert_constant_count__Word_insert_constant_count_17
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_18, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_18:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 14
	ble	%icc, .LL38
	 nop
	mov	2913, %o0
	call	_m3_fault, 0
	 nop
.LL38:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(261120), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	insert_constant_count__Word_insert_constant_count_18, .-insert_constant_count__Word_insert_constant_count_18
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_19, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_19:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 13
	ble	%icc, .LL40
	 nop
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL40:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(523264), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	insert_constant_count__Word_insert_constant_count_19, .-insert_constant_count__Word_insert_constant_count_19
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_20, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_20:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 12
	ble	%icc, .LL42
	 nop
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL42:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(1047552), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	insert_constant_count__Word_insert_constant_count_20, .-insert_constant_count__Word_insert_constant_count_20
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_21, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_21:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 11
	ble	%icc, .LL44
	 nop
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL44:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(2096128), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	insert_constant_count__Word_insert_constant_count_21, .-insert_constant_count__Word_insert_constant_count_21
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_22, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_22:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 10
	ble	%icc, .LL46
	 nop
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL46:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(4193280), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	insert_constant_count__Word_insert_constant_count_22, .-insert_constant_count__Word_insert_constant_count_22
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_23, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_23:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 9
	ble	%icc, .LL48
	 nop
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL48:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(8387584), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	insert_constant_count__Word_insert_constant_count_23, .-insert_constant_count__Word_insert_constant_count_23
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_24, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_24:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 8
	ble	%icc, .LL50
	 nop
	mov	3297, %o0
	call	_m3_fault, 0
	 nop
.LL50:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(16776192), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	insert_constant_count__Word_insert_constant_count_24, .-insert_constant_count__Word_insert_constant_count_24
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_25, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_25:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 7
	ble	%icc, .LL52
	 nop
	mov	3361, %o0
	call	_m3_fault, 0
	 nop
.LL52:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(33553408), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE25:
	.size	insert_constant_count__Word_insert_constant_count_25, .-insert_constant_count__Word_insert_constant_count_25
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_26, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_26:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 6
	ble	%icc, .LL54
	 nop
	mov	3425, %o0
	call	_m3_fault, 0
	 nop
.LL54:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(67107840), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	insert_constant_count__Word_insert_constant_count_26, .-insert_constant_count__Word_insert_constant_count_26
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_27, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_27:
.LLFB27:
	save	%sp, -96, %sp
.LLCFI27:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 5
	ble	%icc, .LL56
	 nop
	mov	3489, %o0
	call	_m3_fault, 0
	 nop
.LL56:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(134216704), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE27:
	.size	insert_constant_count__Word_insert_constant_count_27, .-insert_constant_count__Word_insert_constant_count_27
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_28, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_28:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 4
	ble	%icc, .LL58
	 nop
	mov	3553, %o0
	call	_m3_fault, 0
	 nop
.LL58:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(268434432), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(268434432), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	insert_constant_count__Word_insert_constant_count_28, .-insert_constant_count__Word_insert_constant_count_28
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_29, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_29:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 3
	ble	%icc, .LL60
	 nop
	mov	3617, %o0
	call	_m3_fault, 0
	 nop
.LL60:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(536869888), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(536869888), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	insert_constant_count__Word_insert_constant_count_29, .-insert_constant_count__Word_insert_constant_count_29
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_30, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_30:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 2
	ble	%icc, .LL62
	 nop
	mov	3681, %o0
	call	_m3_fault, 0
	 nop
.LL62:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(1073740800), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(1073740800), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	insert_constant_count__Word_insert_constant_count_30, .-insert_constant_count__Word_insert_constant_count_30
	.align 4
	.type	insert_constant_count__Word_insert_constant_count_31, #function
	.proc	04
insert_constant_count__Word_insert_constant_count_31:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 1
	ble	%icc, .LL64
	 nop
	mov	3745, %o0
	call	_m3_fault, 0
	 nop
.LL64:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(2147482624), %o5
	or	%o5, 1023, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	ld	[%fp+72], %o4
	sethi	%hi(2147482624), %g1
	or	%g1, 1023, %g1
	and	%o4, %g1, %o4
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sll	%o4, %g1, %g1
	or	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	insert_constant_count__Word_insert_constant_count_31, .-insert_constant_count__Word_insert_constant_count_31
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_0, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_0:
.LLFB32:
	save	%sp, -112, %sp
.LLCFI32:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 64
	ble	%icc, .LL66
	 nop
	mov	3809, %o0
	call	_m3_fault, 0
	 nop
.LL66:
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE32:
	.size	insert_constant_count__Long_insert_constant_count_0, .-insert_constant_count__Long_insert_constant_count_0
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_1, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_1:
.LLFB33:
	save	%sp, -112, %sp
.LLCFI33:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 63
	ble	%icc, .LL68
	 nop
	mov	3873, %o0
	call	_m3_fault, 0
	 nop
.LL68:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	1, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE33:
	.size	insert_constant_count__Long_insert_constant_count_1, .-insert_constant_count__Long_insert_constant_count_1
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_2, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_2:
.LLFB34:
	save	%sp, -112, %sp
.LLCFI34:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 62
	ble	%icc, .LL70
	 nop
	mov	3937, %o0
	call	_m3_fault, 0
	 nop
.LL70:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	3, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 3, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE34:
	.size	insert_constant_count__Long_insert_constant_count_2, .-insert_constant_count__Long_insert_constant_count_2
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_3, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_3:
.LLFB35:
	save	%sp, -112, %sp
.LLCFI35:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 61
	ble	%icc, .LL72
	 nop
	mov	4001, %o0
	call	_m3_fault, 0
	 nop
.LL72:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	7, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 7, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE35:
	.size	insert_constant_count__Long_insert_constant_count_3, .-insert_constant_count__Long_insert_constant_count_3
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_4, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_4:
.LLFB36:
	save	%sp, -112, %sp
.LLCFI36:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 60
	ble	%icc, .LL74
	 nop
	mov	4065, %o0
	call	_m3_fault, 0
	 nop
.LL74:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	15, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 15, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	insert_constant_count__Long_insert_constant_count_4, .-insert_constant_count__Long_insert_constant_count_4
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_5, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_5:
.LLFB37:
	save	%sp, -112, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 59
	ble	%icc, .LL76
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL76:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	31, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 31, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	insert_constant_count__Long_insert_constant_count_5, .-insert_constant_count__Long_insert_constant_count_5
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_6, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_6:
.LLFB38:
	save	%sp, -112, %sp
.LLCFI38:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 58
	ble	%icc, .LL78
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL78:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	63, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 63, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	insert_constant_count__Long_insert_constant_count_6, .-insert_constant_count__Long_insert_constant_count_6
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_7, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_7:
.LLFB39:
	save	%sp, -112, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 57
	ble	%icc, .LL80
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL80:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	127, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 127, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	insert_constant_count__Long_insert_constant_count_7, .-insert_constant_count__Long_insert_constant_count_7
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_8, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_8:
.LLFB40:
	save	%sp, -112, %sp
.LLCFI40:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 56
	ble	%icc, .LL82
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL82:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	255, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 255, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE40:
	.size	insert_constant_count__Long_insert_constant_count_8, .-insert_constant_count__Long_insert_constant_count_8
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_9, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_9:
.LLFB41:
	save	%sp, -112, %sp
.LLCFI41:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 55
	ble	%icc, .LL84
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL84:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	511, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 511, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE41:
	.size	insert_constant_count__Long_insert_constant_count_9, .-insert_constant_count__Long_insert_constant_count_9
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_10, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_10:
.LLFB42:
	save	%sp, -112, %sp
.LLCFI42:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 54
	ble	%icc, .LL86
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL86:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 1023, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE42:
	.size	insert_constant_count__Long_insert_constant_count_10, .-insert_constant_count__Long_insert_constant_count_10
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_11, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_11:
.LLFB43:
	save	%sp, -112, %sp
.LLCFI43:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 53
	ble	%icc, .LL88
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL88:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	2047, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 2047, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE43:
	.size	insert_constant_count__Long_insert_constant_count_11, .-insert_constant_count__Long_insert_constant_count_11
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_12, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_12:
.LLFB44:
	save	%sp, -112, %sp
.LLCFI44:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 52
	ble	%icc, .LL90
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL90:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	mov	4095, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	and	%o4, 0, %o4
	and	%o5, 4095, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE44:
	.size	insert_constant_count__Long_insert_constant_count_12, .-insert_constant_count__Long_insert_constant_count_12
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_13, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_13:
.LLFB45:
	save	%sp, -112, %sp
.LLCFI45:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 51
	ble	%icc, .LL92
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL92:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(7168), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE45:
	.size	insert_constant_count__Long_insert_constant_count_13, .-insert_constant_count__Long_insert_constant_count_13
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_14, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_14:
.LLFB46:
	save	%sp, -112, %sp
.LLCFI46:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 50
	ble	%icc, .LL94
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL94:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(15360), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE46:
	.size	insert_constant_count__Long_insert_constant_count_14, .-insert_constant_count__Long_insert_constant_count_14
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_15, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_15:
.LLFB47:
	save	%sp, -112, %sp
.LLCFI47:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 49
	ble	%icc, .LL96
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL96:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(31744), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE47:
	.size	insert_constant_count__Long_insert_constant_count_15, .-insert_constant_count__Long_insert_constant_count_15
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_16, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_16:
.LLFB48:
	save	%sp, -112, %sp
.LLCFI48:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 48
	ble	%icc, .LL98
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL98:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(64512), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	insert_constant_count__Long_insert_constant_count_16, .-insert_constant_count__Long_insert_constant_count_16
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_17, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_17:
.LLFB49:
	save	%sp, -112, %sp
.LLCFI49:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 47
	ble	%icc, .LL100
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL100:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(130048), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	insert_constant_count__Long_insert_constant_count_17, .-insert_constant_count__Long_insert_constant_count_17
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_18, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_18:
.LLFB50:
	save	%sp, -112, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 46
	ble	%icc, .LL102
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL102:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(261120), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	insert_constant_count__Long_insert_constant_count_18, .-insert_constant_count__Long_insert_constant_count_18
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_19, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_19:
.LLFB51:
	save	%sp, -112, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 45
	ble	%icc, .LL104
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL104:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(523264), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	insert_constant_count__Long_insert_constant_count_19, .-insert_constant_count__Long_insert_constant_count_19
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_20, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_20:
.LLFB52:
	save	%sp, -112, %sp
.LLCFI52:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 44
	ble	%icc, .LL106
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL106:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(1047552), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	insert_constant_count__Long_insert_constant_count_20, .-insert_constant_count__Long_insert_constant_count_20
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_21, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_21:
.LLFB53:
	save	%sp, -112, %sp
.LLCFI53:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 43
	ble	%icc, .LL108
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL108:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(2096128), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	insert_constant_count__Long_insert_constant_count_21, .-insert_constant_count__Long_insert_constant_count_21
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_22, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_22:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 42
	ble	%icc, .LL110
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL110:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(4193280), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	insert_constant_count__Long_insert_constant_count_22, .-insert_constant_count__Long_insert_constant_count_22
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_23, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_23:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 41
	ble	%icc, .LL112
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL112:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(8387584), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	insert_constant_count__Long_insert_constant_count_23, .-insert_constant_count__Long_insert_constant_count_23
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_24, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_24:
.LLFB56:
	save	%sp, -112, %sp
.LLCFI56:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 40
	ble	%icc, .LL114
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL114:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(16776192), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	insert_constant_count__Long_insert_constant_count_24, .-insert_constant_count__Long_insert_constant_count_24
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_25, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_25:
.LLFB57:
	save	%sp, -112, %sp
.LLCFI57:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 39
	ble	%icc, .LL116
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL116:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(33553408), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	insert_constant_count__Long_insert_constant_count_25, .-insert_constant_count__Long_insert_constant_count_25
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_26, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_26:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 38
	ble	%icc, .LL118
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL118:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(67107840), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	insert_constant_count__Long_insert_constant_count_26, .-insert_constant_count__Long_insert_constant_count_26
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_27, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_27:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 37
	ble	%icc, .LL120
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL120:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(134216704), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	insert_constant_count__Long_insert_constant_count_27, .-insert_constant_count__Long_insert_constant_count_27
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_28, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_28:
.LLFB60:
	save	%sp, -112, %sp
.LLCFI60:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 36
	ble	%icc, .LL122
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL122:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(268434432), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(268434432), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	insert_constant_count__Long_insert_constant_count_28, .-insert_constant_count__Long_insert_constant_count_28
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_29, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_29:
.LLFB61:
	save	%sp, -112, %sp
.LLCFI61:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 35
	ble	%icc, .LL124
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL124:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(536869888), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(536869888), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	insert_constant_count__Long_insert_constant_count_29, .-insert_constant_count__Long_insert_constant_count_29
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_30, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_30:
.LLFB62:
	save	%sp, -112, %sp
.LLCFI62:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 34
	ble	%icc, .LL126
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL126:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(1073740800), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(1073740800), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	insert_constant_count__Long_insert_constant_count_30, .-insert_constant_count__Long_insert_constant_count_30
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_31, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_31:
.LLFB63:
	save	%sp, -112, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 33
	ble	%icc, .LL128
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL128:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(2147482624), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	mov	0, %o4
	sethi	%hi(2147482624), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	insert_constant_count__Long_insert_constant_count_31, .-insert_constant_count__Long_insert_constant_count_31
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_32, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_32:
.LLFB64:
	save	%sp, -112, %sp
.LLCFI64:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 32
	ble	%icc, .LL130
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL130:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	0, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	0, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	insert_constant_count__Long_insert_constant_count_32, .-insert_constant_count__Long_insert_constant_count_32
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_33, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_33:
.LLFB65:
	save	%sp, -112, %sp
.LLCFI65:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 31
	ble	%icc, .LL132
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL132:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	1, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	1, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	insert_constant_count__Long_insert_constant_count_33, .-insert_constant_count__Long_insert_constant_count_33
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_34, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_34:
.LLFB66:
	save	%sp, -112, %sp
.LLCFI66:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 30
	ble	%icc, .LL134
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL134:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	3, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	3, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	insert_constant_count__Long_insert_constant_count_34, .-insert_constant_count__Long_insert_constant_count_34
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_35, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_35:
.LLFB67:
	save	%sp, -112, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 29
	ble	%icc, .LL136
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL136:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	7, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	7, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	insert_constant_count__Long_insert_constant_count_35, .-insert_constant_count__Long_insert_constant_count_35
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_36, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_36:
.LLFB68:
	save	%sp, -112, %sp
.LLCFI68:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 28
	ble	%icc, .LL138
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL138:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	15, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	15, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	insert_constant_count__Long_insert_constant_count_36, .-insert_constant_count__Long_insert_constant_count_36
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_37, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_37:
.LLFB69:
	save	%sp, -112, %sp
.LLCFI69:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 27
	ble	%icc, .LL140
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL140:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	31, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	31, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	insert_constant_count__Long_insert_constant_count_37, .-insert_constant_count__Long_insert_constant_count_37
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_38, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_38:
.LLFB70:
	save	%sp, -112, %sp
.LLCFI70:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 26
	ble	%icc, .LL142
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL142:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	63, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	63, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	insert_constant_count__Long_insert_constant_count_38, .-insert_constant_count__Long_insert_constant_count_38
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_39, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_39:
.LLFB71:
	save	%sp, -112, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 25
	ble	%icc, .LL144
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL144:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	127, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	127, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	insert_constant_count__Long_insert_constant_count_39, .-insert_constant_count__Long_insert_constant_count_39
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_40, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_40:
.LLFB72:
	save	%sp, -112, %sp
.LLCFI72:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 24
	ble	%icc, .LL146
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL146:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	255, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	255, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	insert_constant_count__Long_insert_constant_count_40, .-insert_constant_count__Long_insert_constant_count_40
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_41, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_41:
.LLFB73:
	save	%sp, -112, %sp
.LLCFI73:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 23
	ble	%icc, .LL148
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL148:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	511, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	511, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	insert_constant_count__Long_insert_constant_count_41, .-insert_constant_count__Long_insert_constant_count_41
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_42, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_42:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 22
	ble	%icc, .LL150
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL150:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	1023, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	insert_constant_count__Long_insert_constant_count_42, .-insert_constant_count__Long_insert_constant_count_42
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_43, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_43:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 21
	ble	%icc, .LL152
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL152:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	2047, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	2047, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	insert_constant_count__Long_insert_constant_count_43, .-insert_constant_count__Long_insert_constant_count_43
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_44, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_44:
.LLFB76:
	save	%sp, -112, %sp
.LLCFI76:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 20
	ble	%icc, .LL154
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL154:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	mov	4095, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o4
	mov	4095, %o0
	sethi	%hi(4294966272), %o1
	or	%o1, 1023, %o1
	and	%o4, %o0, %o4
	and	%o5, %o1, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	insert_constant_count__Long_insert_constant_count_44, .-insert_constant_count__Long_insert_constant_count_44
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_45, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_45:
.LLFB77:
	save	%sp, -112, %sp
.LLCFI77:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 19
	ble	%icc, .LL156
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL156:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(7168), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(7168), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	insert_constant_count__Long_insert_constant_count_45, .-insert_constant_count__Long_insert_constant_count_45
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_46, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_46:
.LLFB78:
	save	%sp, -112, %sp
.LLCFI78:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 18
	ble	%icc, .LL158
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL158:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(15360), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(15360), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	insert_constant_count__Long_insert_constant_count_46, .-insert_constant_count__Long_insert_constant_count_46
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_47, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_47:
.LLFB79:
	save	%sp, -112, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 17
	ble	%icc, .LL160
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL160:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(31744), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(31744), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	insert_constant_count__Long_insert_constant_count_47, .-insert_constant_count__Long_insert_constant_count_47
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_48, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_48:
.LLFB80:
	save	%sp, -112, %sp
.LLCFI80:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 16
	ble	%icc, .LL162
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL162:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(64512), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(64512), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	insert_constant_count__Long_insert_constant_count_48, .-insert_constant_count__Long_insert_constant_count_48
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_49, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_49:
.LLFB81:
	save	%sp, -112, %sp
.LLCFI81:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 15
	ble	%icc, .LL164
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL164:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(130048), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(130048), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	insert_constant_count__Long_insert_constant_count_49, .-insert_constant_count__Long_insert_constant_count_49
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_50, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_50:
.LLFB82:
	save	%sp, -112, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 14
	ble	%icc, .LL166
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL166:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(261120), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(261120), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	insert_constant_count__Long_insert_constant_count_50, .-insert_constant_count__Long_insert_constant_count_50
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_51, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_51:
.LLFB83:
	save	%sp, -112, %sp
.LLCFI83:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 13
	ble	%icc, .LL168
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL168:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(523264), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(523264), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	insert_constant_count__Long_insert_constant_count_51, .-insert_constant_count__Long_insert_constant_count_51
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_52, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_52:
.LLFB84:
	save	%sp, -112, %sp
.LLCFI84:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 12
	ble	%icc, .LL170
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL170:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(1047552), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(1047552), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	insert_constant_count__Long_insert_constant_count_52, .-insert_constant_count__Long_insert_constant_count_52
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_53, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_53:
.LLFB85:
	save	%sp, -112, %sp
.LLCFI85:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 11
	ble	%icc, .LL172
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL172:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(2096128), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(2096128), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	insert_constant_count__Long_insert_constant_count_53, .-insert_constant_count__Long_insert_constant_count_53
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_54, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_54:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 10
	ble	%icc, .LL174
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL174:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(4193280), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(4193280), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	insert_constant_count__Long_insert_constant_count_54, .-insert_constant_count__Long_insert_constant_count_54
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_55, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_55:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 9
	ble	%icc, .LL176
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL176:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(8387584), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(8387584), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	insert_constant_count__Long_insert_constant_count_55, .-insert_constant_count__Long_insert_constant_count_55
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_56, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_56:
.LLFB88:
	save	%sp, -112, %sp
.LLCFI88:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 8
	ble	%icc, .LL178
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL178:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(16776192), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(16776192), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	insert_constant_count__Long_insert_constant_count_56, .-insert_constant_count__Long_insert_constant_count_56
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_57, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_57:
.LLFB89:
	save	%sp, -112, %sp
.LLCFI89:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 7
	ble	%icc, .LL180
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL180:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(33553408), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(33553408), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	insert_constant_count__Long_insert_constant_count_57, .-insert_constant_count__Long_insert_constant_count_57
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_58, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_58:
.LLFB90:
	save	%sp, -112, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 6
	ble	%icc, .LL182
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL182:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(67107840), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(67107840), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	insert_constant_count__Long_insert_constant_count_58, .-insert_constant_count__Long_insert_constant_count_58
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_59, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_59:
.LLFB91:
	save	%sp, -112, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 5
	ble	%icc, .LL184
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL184:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(134216704), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(134216704), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	insert_constant_count__Long_insert_constant_count_59, .-insert_constant_count__Long_insert_constant_count_59
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_60, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_60:
.LLFB92:
	save	%sp, -112, %sp
.LLCFI92:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 4
	ble	%icc, .LL186
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL186:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(268434432), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(268434432), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	insert_constant_count__Long_insert_constant_count_60, .-insert_constant_count__Long_insert_constant_count_60
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_61, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_61:
.LLFB93:
	save	%sp, -112, %sp
.LLCFI93:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 3
	ble	%icc, .LL188
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL188:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(536869888), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(536869888), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	insert_constant_count__Long_insert_constant_count_61, .-insert_constant_count__Long_insert_constant_count_61
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_62, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_62:
.LLFB94:
	save	%sp, -112, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 2
	ble	%icc, .LL190
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL190:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(1073740800), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(1073740800), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	insert_constant_count__Long_insert_constant_count_62, .-insert_constant_count__Long_insert_constant_count_62
	.align 4
	.type	insert_constant_count__Long_insert_constant_count_63, #function
	.proc	05
insert_constant_count__Long_insert_constant_count_63:
.LLFB95:
	save	%sp, -112, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	cmp	%g1, 1
	ble	%icc, .LL192
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL192:
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sethi	%hi(2147482624), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	ldd	[%fp-16], %o0
	sethi	%hi(2147482624), %o4
	or	%o4, 1023, %o4
	sethi	%hi(4294966272), %o5
	or	%o5, 1023, %o5
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	ld	[%g1+140], %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	insert_constant_count__Long_insert_constant_count_63, .-insert_constant_count__Long_insert_constant_count_63
	.align 4
	.global insert_constant_count_M3
	.type	insert_constant_count_M3, #function
	.proc	0120
insert_constant_count_M3:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	st	%i0, [%fp+68]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	insert_constant_count_M3, .-insert_constant_count_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	st	%i0, [%fp+68]
	sethi	%hi(MM_insert_constant_count), %g1
	or	%g1, %lo(MM_insert_constant_count), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE97:
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 3704
L_1:
	.ascii	"insert_constant_count_M3"
	.skip 1
	.ascii	"Long_insert_constant_count_63"
	.skip 1
	.ascii	"Long_insert_constant_count_62"
	.skip 1
	.ascii	"Long_insert_constant_count_61"
	.skip 1
	.ascii	"Long_insert_constant_count_60"
	.skip 1
	.ascii	"Long_insert_constant_count_59"
	.skip 1
	.ascii	"Long_insert_constant_count_58"
	.skip 1
	.ascii	"Long_insert_constant_count_57"
	.skip 1
	.ascii	"Long_insert_constant_count_56"
	.skip 1
	.ascii	"Long_insert_constant_count_55"
	.skip 1
	.ascii	"Long_insert_constant_count_54"
	.skip 1
	.ascii	"Long_insert_constant_count_53"
	.skip 1
	.ascii	"Long_insert_constant_count_52"
	.skip 1
	.ascii	"Long_insert_constant_count_51"
	.skip 1
	.ascii	"Long_insert_constant_count_50"
	.skip 1
	.ascii	"Long_insert_constant_count_49"
	.skip 1
	.ascii	"Long_insert_constant_count_48"
	.skip 1
	.ascii	"Long_insert_constant_count_47"
	.skip 1
	.ascii	"Long_insert_constant_count_46"
	.skip 1
	.ascii	"Long_insert_constant_count_45"
	.skip 1
	.ascii	"Long_insert_constant_count_44"
	.skip 1
	.ascii	"Long_insert_constant_count_43"
	.skip 1
	.ascii	"Long_insert_constant_count_42"
	.skip 1
	.ascii	"Long_insert_constant_count_41"
	.skip 1
	.ascii	"Long_insert_constant_count_40"
	.skip 1
	.ascii	"Long_insert_constant_count_39"
	.skip 1
	.ascii	"Long_insert_constant_count_38"
	.skip 1
	.ascii	"Long_insert_constant_count_37"
	.skip 1
	.ascii	"Long_insert_constant_count_36"
	.skip 1
	.ascii	"Long_insert_constant_count_35"
	.skip 1
	.ascii	"Long_insert_constant_count_34"
	.skip 1
	.ascii	"Long_insert_constant_count_33"
	.skip 1
	.ascii	"Long_insert_constant_count_32"
	.skip 1
	.ascii	"Long_insert_constant_count_31"
	.skip 1
	.ascii	"Long_insert_constant_count_30"
	.skip 1
	.ascii	"Long_insert_constant_count_29"
	.skip 1
	.ascii	"Long_insert_constant_count_28"
	.skip 1
	.ascii	"Long_insert_constant_count_27"
	.skip 1
	.ascii	"Long_insert_constant_count_26"
	.skip 1
	.ascii	"Long_insert_constant_count_25"
	.skip 1
	.ascii	"Long_insert_constant_count_24"
	.skip 1
	.ascii	"Long_insert_constant_count_23"
	.skip 1
	.ascii	"Long_insert_constant_count_22"
	.skip 1
	.ascii	"Long_insert_constant_count_21"
	.skip 1
	.ascii	"Long_insert_constant_count_20"
	.skip 1
	.ascii	"Long_insert_constant_count_19"
	.skip 1
	.ascii	"Long_insert_constant_count_18"
	.skip 1
	.ascii	"Long_insert_constant_count_17"
	.skip 1
	.ascii	"Long_insert_constant_count_16"
	.skip 1
	.ascii	"Long_insert_constant_count_15"
	.skip 1
	.ascii	"Long_insert_constant_count_14"
	.skip 1
	.ascii	"Long_insert_constant_count_13"
	.skip 1
	.ascii	"Long_insert_constant_count_12"
	.skip 1
	.ascii	"Long_insert_constant_count_11"
	.skip 1
	.ascii	"Long_insert_constant_count_10"
	.skip 1
	.ascii	"Long_insert_constant_count_9"
	.skip 1
	.ascii	"Long_insert_constant_count_8"
	.skip 1
	.ascii	"Long_insert_constant_count_7"
	.skip 1
	.ascii	"Long_insert_constant_count_6"
	.skip 1
	.ascii	"Long_insert_constant_count_5"
	.skip 1
	.ascii	"Long_insert_constant_count_4"
	.skip 1
	.ascii	"Long_insert_constant_count_3"
	.skip 1
	.ascii	"Long_insert_constant_count_2"
	.skip 1
	.ascii	"Long_insert_constant_count_1"
	.skip 1
	.ascii	"Long_insert_constant_count_0"
	.skip 1
	.ascii	"Word_insert_constant_count_31"
	.skip 1
	.ascii	"Word_insert_constant_count_30"
	.skip 1
	.ascii	"Word_insert_constant_count_29"
	.skip 1
	.ascii	"Word_insert_constant_count_28"
	.skip 1
	.ascii	"Word_insert_constant_count_27"
	.skip 1
	.ascii	"Word_insert_constant_count_26"
	.skip 1
	.ascii	"Word_insert_constant_count_25"
	.skip 1
	.ascii	"Word_insert_constant_count_24"
	.skip 1
	.ascii	"Word_insert_constant_count_23"
	.skip 1
	.ascii	"Word_insert_constant_count_22"
	.skip 1
	.ascii	"Word_insert_constant_count_21"
	.skip 1
	.ascii	"Word_insert_constant_count_20"
	.skip 1
	.ascii	"Word_insert_constant_count_19"
	.skip 1
	.ascii	"Word_insert_constant_count_18"
	.skip 1
	.ascii	"Word_insert_constant_count_17"
	.skip 1
	.ascii	"Word_insert_constant_count_16"
	.skip 1
	.ascii	"Word_insert_constant_count_15"
	.skip 1
	.ascii	"Word_insert_constant_count_14"
	.skip 1
	.ascii	"Word_insert_constant_count_13"
	.skip 1
	.ascii	"Word_insert_constant_count_12"
	.skip 1
	.ascii	"Word_insert_constant_count_11"
	.skip 1
	.ascii	"Word_insert_constant_count_10"
	.skip 1
	.ascii	"Word_insert_constant_count_9"
	.skip 1
	.ascii	"Word_insert_constant_count_8"
	.skip 1
	.ascii	"Word_insert_constant_count_7"
	.skip 1
	.ascii	"Word_insert_constant_count_6"
	.skip 1
	.ascii	"Word_insert_constant_count_5"
	.skip 1
	.ascii	"Word_insert_constant_count_4"
	.skip 1
	.ascii	"Word_insert_constant_count_3"
	.skip 1
	.ascii	"Word_insert_constant_count_2"
	.skip 1
	.ascii	"Word_insert_constant_count_1"
	.skip 1
	.ascii	"Word_insert_constant_count_0"
	.skip 4
	.long	insert_constant_count_M3
	.long	L_1
	.long	insert_constant_count__Long_insert_constant_count_63
	.long	L_1+25
	.long	insert_constant_count__Long_insert_constant_count_62
	.long	L_1+55
	.long	insert_constant_count__Long_insert_constant_count_61
	.long	L_1+85
	.long	insert_constant_count__Long_insert_constant_count_60
	.long	L_1+115
	.long	insert_constant_count__Long_insert_constant_count_59
	.long	L_1+145
	.long	insert_constant_count__Long_insert_constant_count_58
	.long	L_1+175
	.long	insert_constant_count__Long_insert_constant_count_57
	.long	L_1+205
	.long	insert_constant_count__Long_insert_constant_count_56
	.long	L_1+235
	.long	insert_constant_count__Long_insert_constant_count_55
	.long	L_1+265
	.long	insert_constant_count__Long_insert_constant_count_54
	.long	L_1+295
	.long	insert_constant_count__Long_insert_constant_count_53
	.long	L_1+325
	.long	insert_constant_count__Long_insert_constant_count_52
	.long	L_1+355
	.long	insert_constant_count__Long_insert_constant_count_51
	.long	L_1+385
	.long	insert_constant_count__Long_insert_constant_count_50
	.long	L_1+415
	.long	insert_constant_count__Long_insert_constant_count_49
	.long	L_1+445
	.long	insert_constant_count__Long_insert_constant_count_48
	.long	L_1+475
	.long	insert_constant_count__Long_insert_constant_count_47
	.long	L_1+505
	.long	insert_constant_count__Long_insert_constant_count_46
	.long	L_1+535
	.long	insert_constant_count__Long_insert_constant_count_45
	.long	L_1+565
	.long	insert_constant_count__Long_insert_constant_count_44
	.long	L_1+595
	.long	insert_constant_count__Long_insert_constant_count_43
	.long	L_1+625
	.long	insert_constant_count__Long_insert_constant_count_42
	.long	L_1+655
	.long	insert_constant_count__Long_insert_constant_count_41
	.long	L_1+685
	.long	insert_constant_count__Long_insert_constant_count_40
	.long	L_1+715
	.long	insert_constant_count__Long_insert_constant_count_39
	.long	L_1+745
	.long	insert_constant_count__Long_insert_constant_count_38
	.long	L_1+775
	.long	insert_constant_count__Long_insert_constant_count_37
	.long	L_1+805
	.long	insert_constant_count__Long_insert_constant_count_36
	.long	L_1+835
	.long	insert_constant_count__Long_insert_constant_count_35
	.long	L_1+865
	.long	insert_constant_count__Long_insert_constant_count_34
	.long	L_1+895
	.long	insert_constant_count__Long_insert_constant_count_33
	.long	L_1+925
	.long	insert_constant_count__Long_insert_constant_count_32
	.long	L_1+955
	.long	insert_constant_count__Long_insert_constant_count_31
	.long	L_1+985
	.long	insert_constant_count__Long_insert_constant_count_30
	.long	L_1+1015
	.long	insert_constant_count__Long_insert_constant_count_29
	.long	L_1+1045
	.long	insert_constant_count__Long_insert_constant_count_28
	.long	L_1+1075
	.long	insert_constant_count__Long_insert_constant_count_27
	.long	L_1+1105
	.long	insert_constant_count__Long_insert_constant_count_26
	.long	L_1+1135
	.long	insert_constant_count__Long_insert_constant_count_25
	.long	L_1+1165
	.long	insert_constant_count__Long_insert_constant_count_24
	.long	L_1+1195
	.long	insert_constant_count__Long_insert_constant_count_23
	.long	L_1+1225
	.long	insert_constant_count__Long_insert_constant_count_22
	.long	L_1+1255
	.long	insert_constant_count__Long_insert_constant_count_21
	.long	L_1+1285
	.long	insert_constant_count__Long_insert_constant_count_20
	.long	L_1+1315
	.long	insert_constant_count__Long_insert_constant_count_19
	.long	L_1+1345
	.long	insert_constant_count__Long_insert_constant_count_18
	.long	L_1+1375
	.long	insert_constant_count__Long_insert_constant_count_17
	.long	L_1+1405
	.long	insert_constant_count__Long_insert_constant_count_16
	.long	L_1+1435
	.long	insert_constant_count__Long_insert_constant_count_15
	.long	L_1+1465
	.long	insert_constant_count__Long_insert_constant_count_14
	.long	L_1+1495
	.long	insert_constant_count__Long_insert_constant_count_13
	.long	L_1+1525
	.long	insert_constant_count__Long_insert_constant_count_12
	.long	L_1+1555
	.long	insert_constant_count__Long_insert_constant_count_11
	.long	L_1+1585
	.long	insert_constant_count__Long_insert_constant_count_10
	.long	L_1+1615
	.long	insert_constant_count__Long_insert_constant_count_9
	.long	L_1+1645
	.long	insert_constant_count__Long_insert_constant_count_8
	.long	L_1+1674
	.long	insert_constant_count__Long_insert_constant_count_7
	.long	L_1+1703
	.long	insert_constant_count__Long_insert_constant_count_6
	.long	L_1+1732
	.long	insert_constant_count__Long_insert_constant_count_5
	.long	L_1+1761
	.long	insert_constant_count__Long_insert_constant_count_4
	.long	L_1+1790
	.long	insert_constant_count__Long_insert_constant_count_3
	.long	L_1+1819
	.long	insert_constant_count__Long_insert_constant_count_2
	.long	L_1+1848
	.long	insert_constant_count__Long_insert_constant_count_1
	.long	L_1+1877
	.long	insert_constant_count__Long_insert_constant_count_0
	.long	L_1+1906
	.long	insert_constant_count__Word_insert_constant_count_31
	.long	L_1+1935
	.long	insert_constant_count__Word_insert_constant_count_30
	.long	L_1+1965
	.long	insert_constant_count__Word_insert_constant_count_29
	.long	L_1+1995
	.long	insert_constant_count__Word_insert_constant_count_28
	.long	L_1+2025
	.long	insert_constant_count__Word_insert_constant_count_27
	.long	L_1+2055
	.long	insert_constant_count__Word_insert_constant_count_26
	.long	L_1+2085
	.long	insert_constant_count__Word_insert_constant_count_25
	.long	L_1+2115
	.long	insert_constant_count__Word_insert_constant_count_24
	.long	L_1+2145
	.long	insert_constant_count__Word_insert_constant_count_23
	.long	L_1+2175
	.long	insert_constant_count__Word_insert_constant_count_22
	.long	L_1+2205
	.long	insert_constant_count__Word_insert_constant_count_21
	.long	L_1+2235
	.long	insert_constant_count__Word_insert_constant_count_20
	.long	L_1+2265
	.long	insert_constant_count__Word_insert_constant_count_19
	.long	L_1+2295
	.long	insert_constant_count__Word_insert_constant_count_18
	.long	L_1+2325
	.long	insert_constant_count__Word_insert_constant_count_17
	.long	L_1+2355
	.long	insert_constant_count__Word_insert_constant_count_16
	.long	L_1+2385
	.long	insert_constant_count__Word_insert_constant_count_15
	.long	L_1+2415
	.long	insert_constant_count__Word_insert_constant_count_14
	.long	L_1+2445
	.long	insert_constant_count__Word_insert_constant_count_13
	.long	L_1+2475
	.long	insert_constant_count__Word_insert_constant_count_12
	.long	L_1+2505
	.long	insert_constant_count__Word_insert_constant_count_11
	.long	L_1+2535
	.long	insert_constant_count__Word_insert_constant_count_10
	.long	L_1+2565
	.long	insert_constant_count__Word_insert_constant_count_9
	.long	L_1+2595
	.long	insert_constant_count__Word_insert_constant_count_8
	.long	L_1+2624
	.long	insert_constant_count__Word_insert_constant_count_7
	.long	L_1+2653
	.long	insert_constant_count__Word_insert_constant_count_6
	.long	L_1+2682
	.long	insert_constant_count__Word_insert_constant_count_5
	.long	L_1+2711
	.long	insert_constant_count__Word_insert_constant_count_4
	.long	L_1+2740
	.long	insert_constant_count__Word_insert_constant_count_3
	.long	L_1+2769
	.long	insert_constant_count__Word_insert_constant_count_2
	.long	L_1+2798
	.long	insert_constant_count__Word_insert_constant_count_1
	.long	L_1+2827
	.long	insert_constant_count__Word_insert_constant_count_0
	.long	L_1+2856
	.skip 4
	.ascii	"../SOLsun/insert_constant_count.m3"
	.skip 2
	.align 8
	.type	MM_insert_constant_count, #object
	.size	MM_insert_constant_count, 208
MM_insert_constant_count:
	.long	L_1+3668
	.skip 16
	.long	L_1+2888
	.skip 12
	.long	MM_insert_constant_count+148
	.skip 4
	.long	insert_constant_count_M3
	.long	3
	.half	1009
	.skip 2
	.long	0
	.long	1010
	.long	0
	.long	1011
	.long	1012
	.half	1013
	.skip 2
	.long	1014
	.skip 4
	.long	0
	.long	1015
	.long	1016
	.skip 4
	.long	1083164880
	.long	2089072093
	.byte	-5
	.skip 7
	.long	0
	.long	1020
	.byte	-3
	.skip 3
	.long	1022
	.long	1149224590
	.skip 12
	.long	insert_constant_count_I3
	.long	MM_insert_constant_count+160
	.skip 4
	.long	Long_I3
	.long	MM_insert_constant_count+172
	.skip 4
	.long	Word_I3
	.long	MM_insert_constant_count+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_insert_constant_count+196
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
	.uaword	.LLFB48
	.uaword	.LLFE48-.LLFB48
	.byte	0x4
	.uaword	.LLCFI48-.LLFB48
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
.LLEFDE99:
.LLSFDE101:
	.uaword	.LLEFDE101-.LLASFDE101
.LLASFDE101:
	.uaword	.LLASFDE101-.LLframe1
	.uaword	.LLFB50
	.uaword	.LLFE50-.LLFB50
	.byte	0x4
	.uaword	.LLCFI50-.LLFB50
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
.LLEFDE103:
.LLSFDE105:
	.uaword	.LLEFDE105-.LLASFDE105
.LLASFDE105:
	.uaword	.LLASFDE105-.LLframe1
	.uaword	.LLFB52
	.uaword	.LLFE52-.LLFB52
	.byte	0x4
	.uaword	.LLCFI52-.LLFB52
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
.LLEFDE107:
.LLSFDE109:
	.uaword	.LLEFDE109-.LLASFDE109
.LLASFDE109:
	.uaword	.LLASFDE109-.LLframe1
	.uaword	.LLFB54
	.uaword	.LLFE54-.LLFB54
	.byte	0x4
	.uaword	.LLCFI54-.LLFB54
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
.LLEFDE111:
.LLSFDE113:
	.uaword	.LLEFDE113-.LLASFDE113
.LLASFDE113:
	.uaword	.LLASFDE113-.LLframe1
	.uaword	.LLFB56
	.uaword	.LLFE56-.LLFB56
	.byte	0x4
	.uaword	.LLCFI56-.LLFB56
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
.LLEFDE115:
.LLSFDE117:
	.uaword	.LLEFDE117-.LLASFDE117
.LLASFDE117:
	.uaword	.LLASFDE117-.LLframe1
	.uaword	.LLFB58
	.uaword	.LLFE58-.LLFB58
	.byte	0x4
	.uaword	.LLCFI58-.LLFB58
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
.LLEFDE119:
.LLSFDE121:
	.uaword	.LLEFDE121-.LLASFDE121
.LLASFDE121:
	.uaword	.LLASFDE121-.LLframe1
	.uaword	.LLFB60
	.uaword	.LLFE60-.LLFB60
	.byte	0x4
	.uaword	.LLCFI60-.LLFB60
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
.LLEFDE123:
.LLSFDE125:
	.uaword	.LLEFDE125-.LLASFDE125
.LLASFDE125:
	.uaword	.LLASFDE125-.LLframe1
	.uaword	.LLFB62
	.uaword	.LLFE62-.LLFB62
	.byte	0x4
	.uaword	.LLCFI62-.LLFB62
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
.LLEFDE127:
.LLSFDE129:
	.uaword	.LLEFDE129-.LLASFDE129
.LLASFDE129:
	.uaword	.LLASFDE129-.LLframe1
	.uaword	.LLFB64
	.uaword	.LLFE64-.LLFB64
	.byte	0x4
	.uaword	.LLCFI64-.LLFB64
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
.LLEFDE131:
.LLSFDE133:
	.uaword	.LLEFDE133-.LLASFDE133
.LLASFDE133:
	.uaword	.LLASFDE133-.LLframe1
	.uaword	.LLFB66
	.uaword	.LLFE66-.LLFB66
	.byte	0x4
	.uaword	.LLCFI66-.LLFB66
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
.LLEFDE135:
.LLSFDE137:
	.uaword	.LLEFDE137-.LLASFDE137
.LLASFDE137:
	.uaword	.LLASFDE137-.LLframe1
	.uaword	.LLFB68
	.uaword	.LLFE68-.LLFB68
	.byte	0x4
	.uaword	.LLCFI68-.LLFB68
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
.LLEFDE139:
.LLSFDE141:
	.uaword	.LLEFDE141-.LLASFDE141
.LLASFDE141:
	.uaword	.LLASFDE141-.LLframe1
	.uaword	.LLFB70
	.uaword	.LLFE70-.LLFB70
	.byte	0x4
	.uaword	.LLCFI70-.LLFB70
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
.LLEFDE143:
.LLSFDE145:
	.uaword	.LLEFDE145-.LLASFDE145
.LLASFDE145:
	.uaword	.LLASFDE145-.LLframe1
	.uaword	.LLFB72
	.uaword	.LLFE72-.LLFB72
	.byte	0x4
	.uaword	.LLCFI72-.LLFB72
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
.LLEFDE147:
.LLSFDE149:
	.uaword	.LLEFDE149-.LLASFDE149
.LLASFDE149:
	.uaword	.LLASFDE149-.LLframe1
	.uaword	.LLFB74
	.uaword	.LLFE74-.LLFB74
	.byte	0x4
	.uaword	.LLCFI74-.LLFB74
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
.LLEFDE151:
.LLSFDE153:
	.uaword	.LLEFDE153-.LLASFDE153
.LLASFDE153:
	.uaword	.LLASFDE153-.LLframe1
	.uaword	.LLFB76
	.uaword	.LLFE76-.LLFB76
	.byte	0x4
	.uaword	.LLCFI76-.LLFB76
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
.LLEFDE155:
.LLSFDE157:
	.uaword	.LLEFDE157-.LLASFDE157
.LLASFDE157:
	.uaword	.LLASFDE157-.LLframe1
	.uaword	.LLFB78
	.uaword	.LLFE78-.LLFB78
	.byte	0x4
	.uaword	.LLCFI78-.LLFB78
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
.LLEFDE159:
.LLSFDE161:
	.uaword	.LLEFDE161-.LLASFDE161
.LLASFDE161:
	.uaword	.LLASFDE161-.LLframe1
	.uaword	.LLFB80
	.uaword	.LLFE80-.LLFB80
	.byte	0x4
	.uaword	.LLCFI80-.LLFB80
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
.LLEFDE163:
.LLSFDE165:
	.uaword	.LLEFDE165-.LLASFDE165
.LLASFDE165:
	.uaword	.LLASFDE165-.LLframe1
	.uaword	.LLFB82
	.uaword	.LLFE82-.LLFB82
	.byte	0x4
	.uaword	.LLCFI82-.LLFB82
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
.LLEFDE167:
.LLSFDE169:
	.uaword	.LLEFDE169-.LLASFDE169
.LLASFDE169:
	.uaword	.LLASFDE169-.LLframe1
	.uaword	.LLFB84
	.uaword	.LLFE84-.LLFB84
	.byte	0x4
	.uaword	.LLCFI84-.LLFB84
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
.LLEFDE171:
.LLSFDE173:
	.uaword	.LLEFDE173-.LLASFDE173
.LLASFDE173:
	.uaword	.LLASFDE173-.LLframe1
	.uaword	.LLFB86
	.uaword	.LLFE86-.LLFB86
	.byte	0x4
	.uaword	.LLCFI86-.LLFB86
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
.LLEFDE175:
.LLSFDE177:
	.uaword	.LLEFDE177-.LLASFDE177
.LLASFDE177:
	.uaword	.LLASFDE177-.LLframe1
	.uaword	.LLFB88
	.uaword	.LLFE88-.LLFB88
	.byte	0x4
	.uaword	.LLCFI88-.LLFB88
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
.LLEFDE179:
.LLSFDE181:
	.uaword	.LLEFDE181-.LLASFDE181
.LLASFDE181:
	.uaword	.LLASFDE181-.LLframe1
	.uaword	.LLFB90
	.uaword	.LLFE90-.LLFB90
	.byte	0x4
	.uaword	.LLCFI90-.LLFB90
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
.LLEFDE183:
.LLSFDE185:
	.uaword	.LLEFDE185-.LLASFDE185
.LLASFDE185:
	.uaword	.LLASFDE185-.LLframe1
	.uaword	.LLFB92
	.uaword	.LLFE92-.LLFB92
	.byte	0x4
	.uaword	.LLCFI92-.LLFB92
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
.LLEFDE187:
.LLSFDE189:
	.uaword	.LLEFDE189-.LLASFDE189
.LLASFDE189:
	.uaword	.LLASFDE189-.LLframe1
	.uaword	.LLFB94
	.uaword	.LLFE94-.LLFB94
	.byte	0x4
	.uaword	.LLCFI94-.LLFB94
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
.LLEFDE191:
.LLSFDE193:
	.uaword	.LLEFDE193-.LLASFDE193
.LLASFDE193:
	.uaword	.LLASFDE193-.LLframe1
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
.LLEFDE193:
	.ident	"GCC: (GNU) 4.5.1"
