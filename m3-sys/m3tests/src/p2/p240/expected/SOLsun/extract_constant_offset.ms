	.file	"extract_constant_offset.mc"
	.section	".text"
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_0, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_0:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	ble	%icc, .LL2
	 nop
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL2:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL3
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_0, .-extract_constant_offset__Word_extract_constant_offset_0
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_1, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_1:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 31
	ble	%icc, .LL6
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL6:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL7
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	31, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_1, .-extract_constant_offset__Word_extract_constant_offset_1
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_2, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_2:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 30
	ble	%icc, .LL10
	 nop
	mov	1889, %o0
	call	_m3_fault, 0
	 nop
.LL10:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL11
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	30, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL12
	 nop
.LL11:
	mov	0, %g1
.LL12:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	extract_constant_offset__Word_extract_constant_offset_2, .-extract_constant_offset__Word_extract_constant_offset_2
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_3, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_3:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 29
	ble	%icc, .LL14
	 nop
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL14:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL15
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	29, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL16
	 nop
.LL15:
	mov	0, %g1
.LL16:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	extract_constant_offset__Word_extract_constant_offset_3, .-extract_constant_offset__Word_extract_constant_offset_3
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_4, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_4:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 28
	ble	%icc, .LL18
	 nop
	mov	2017, %o0
	call	_m3_fault, 0
	 nop
.LL18:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL19
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	28, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL20
	 nop
.LL19:
	mov	0, %g1
.LL20:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	extract_constant_offset__Word_extract_constant_offset_4, .-extract_constant_offset__Word_extract_constant_offset_4
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_5, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_5:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 27
	ble	%icc, .LL22
	 nop
	mov	2081, %o0
	call	_m3_fault, 0
	 nop
.LL22:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL23
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	27, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL24
	 nop
.LL23:
	mov	0, %g1
.LL24:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	extract_constant_offset__Word_extract_constant_offset_5, .-extract_constant_offset__Word_extract_constant_offset_5
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_6, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_6:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 26
	ble	%icc, .LL26
	 nop
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL27
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	26, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_6, .-extract_constant_offset__Word_extract_constant_offset_6
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_7, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_7:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 25
	ble	%icc, .LL30
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL30:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL31
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	25, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_7, .-extract_constant_offset__Word_extract_constant_offset_7
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_8, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_8:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 24
	ble	%icc, .LL34
	 nop
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL34:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL35
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	24, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_8, .-extract_constant_offset__Word_extract_constant_offset_8
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_9, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_9:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 23
	ble	%icc, .LL38
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL38:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL39
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	23, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_9, .-extract_constant_offset__Word_extract_constant_offset_9
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_10, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_10:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 22
	ble	%icc, .LL42
	 nop
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL42:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL43
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	22, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_10, .-extract_constant_offset__Word_extract_constant_offset_10
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_11, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_11:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 21
	ble	%icc, .LL46
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL46:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL47
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	21, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_11, .-extract_constant_offset__Word_extract_constant_offset_11
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_12, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_12:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 20
	ble	%icc, .LL50
	 nop
	mov	2529, %o0
	call	_m3_fault, 0
	 nop
.LL50:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL51
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	20, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL52
	 nop
.LL51:
	mov	0, %g1
.LL52:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	extract_constant_offset__Word_extract_constant_offset_12, .-extract_constant_offset__Word_extract_constant_offset_12
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_13, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_13:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 19
	ble	%icc, .LL54
	 nop
	mov	2593, %o0
	call	_m3_fault, 0
	 nop
.LL54:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL55
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	19, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL56
	 nop
.LL55:
	mov	0, %g1
.LL56:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	extract_constant_offset__Word_extract_constant_offset_13, .-extract_constant_offset__Word_extract_constant_offset_13
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_14, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_14:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 18
	ble	%icc, .LL58
	 nop
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL58:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL59
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	18, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_14, .-extract_constant_offset__Word_extract_constant_offset_14
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_15, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_15:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 17
	ble	%icc, .LL62
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL62:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL63
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	17, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_15, .-extract_constant_offset__Word_extract_constant_offset_15
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_16, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 16
	ble	%icc, .LL66
	 nop
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL66:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL67
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	16, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_16, .-extract_constant_offset__Word_extract_constant_offset_16
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_17, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_17:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 15
	ble	%icc, .LL70
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL70:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL71
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	15, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_17, .-extract_constant_offset__Word_extract_constant_offset_17
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_18, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_18:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 14
	ble	%icc, .LL74
	 nop
	mov	2913, %o0
	call	_m3_fault, 0
	 nop
.LL74:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL75
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	14, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL76
	 nop
.LL75:
	mov	0, %g1
.LL76:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	extract_constant_offset__Word_extract_constant_offset_18, .-extract_constant_offset__Word_extract_constant_offset_18
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_19, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_19:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 13
	ble	%icc, .LL78
	 nop
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL78:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL79
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	13, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL80
	 nop
.LL79:
	mov	0, %g1
.LL80:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	extract_constant_offset__Word_extract_constant_offset_19, .-extract_constant_offset__Word_extract_constant_offset_19
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_20, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_20:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 12
	ble	%icc, .LL82
	 nop
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL82:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL83
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	12, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_20, .-extract_constant_offset__Word_extract_constant_offset_20
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_21, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_21:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 11
	ble	%icc, .LL86
	 nop
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL86:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL87
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	11, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_21, .-extract_constant_offset__Word_extract_constant_offset_21
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_22, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_22:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 10
	ble	%icc, .LL90
	 nop
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL90:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL91
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	10, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_22, .-extract_constant_offset__Word_extract_constant_offset_22
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_23, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_23:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 9
	ble	%icc, .LL94
	 nop
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL94:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL95
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	9, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
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
	.size	extract_constant_offset__Word_extract_constant_offset_23, .-extract_constant_offset__Word_extract_constant_offset_23
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_24, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_24:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 8
	ble	%icc, .LL98
	 nop
	mov	3297, %o0
	call	_m3_fault, 0
	 nop
.LL98:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL99
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	8, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL100
	 nop
.LL99:
	mov	0, %g1
.LL100:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	extract_constant_offset__Word_extract_constant_offset_24, .-extract_constant_offset__Word_extract_constant_offset_24
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_25, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_25:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 7
	ble	%icc, .LL102
	 nop
	mov	3361, %o0
	call	_m3_fault, 0
	 nop
.LL102:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL103
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	7, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL104
	 nop
.LL103:
	mov	0, %g1
.LL104:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE25:
	.size	extract_constant_offset__Word_extract_constant_offset_25, .-extract_constant_offset__Word_extract_constant_offset_25
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_26, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_26:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 6
	ble	%icc, .LL106
	 nop
	mov	3425, %o0
	call	_m3_fault, 0
	 nop
.LL106:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL107
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	6, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL108
	 nop
.LL107:
	mov	0, %g1
.LL108:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	extract_constant_offset__Word_extract_constant_offset_26, .-extract_constant_offset__Word_extract_constant_offset_26
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_27, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_27:
.LLFB27:
	save	%sp, -96, %sp
.LLCFI27:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 5
	ble	%icc, .LL110
	 nop
	mov	3489, %o0
	call	_m3_fault, 0
	 nop
.LL110:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL111
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	5, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL112
	 nop
.LL111:
	mov	0, %g1
.LL112:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE27:
	.size	extract_constant_offset__Word_extract_constant_offset_27, .-extract_constant_offset__Word_extract_constant_offset_27
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_28, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_28:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 4
	ble	%icc, .LL114
	 nop
	mov	3553, %o0
	call	_m3_fault, 0
	 nop
.LL114:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL115
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	4, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL116
	 nop
.LL115:
	mov	0, %g1
.LL116:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	extract_constant_offset__Word_extract_constant_offset_28, .-extract_constant_offset__Word_extract_constant_offset_28
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_29, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_29:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 3
	ble	%icc, .LL118
	 nop
	mov	3617, %o0
	call	_m3_fault, 0
	 nop
.LL118:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL119
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	3, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL120
	 nop
.LL119:
	mov	0, %g1
.LL120:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	extract_constant_offset__Word_extract_constant_offset_29, .-extract_constant_offset__Word_extract_constant_offset_29
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_30, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_30:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 2
	ble	%icc, .LL122
	 nop
	mov	3681, %o0
	call	_m3_fault, 0
	 nop
.LL122:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL123
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	2, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL124
	 nop
.LL123:
	mov	0, %g1
.LL124:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	extract_constant_offset__Word_extract_constant_offset_30, .-extract_constant_offset__Word_extract_constant_offset_30
	.align 4
	.type	extract_constant_offset__Word_extract_constant_offset_31, #function
	.proc	04
extract_constant_offset__Word_extract_constant_offset_31:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 1
	ble	%icc, .LL126
	 nop
	mov	3745, %o0
	call	_m3_fault, 0
	 nop
.LL126:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL127
	 nop
	ld	[%fp+68], %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	1, %o4
	sub	%o4, %g1, %g1
	sll	%o5, %g1, %o5
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o4
	sub	%o4, %g1, %g1
	srl	%o5, %g1, %g1
	ba,pt	%xcc, .LL128
	 nop
.LL127:
	mov	0, %g1
.LL128:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	extract_constant_offset__Word_extract_constant_offset_31, .-extract_constant_offset__Word_extract_constant_offset_31
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_0, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_0:
.LLFB32:
	save	%sp, -104, %sp
.LLCFI32:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	ble	%icc, .LL130
	 nop
	mov	3809, %o0
	call	_m3_fault, 0
	 nop
.LL130:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL131
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL132
	 nop
.LL131:
	mov	0, %o4
	mov	0, %o5
.LL132:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE32:
	.size	extract_constant_offset__Long_extract_constant_offset_0, .-extract_constant_offset__Long_extract_constant_offset_0
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_1, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_1:
.LLFB33:
	save	%sp, -104, %sp
.LLCFI33:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 63
	ble	%icc, .LL134
	 nop
	mov	3873, %o0
	call	_m3_fault, 0
	 nop
.LL134:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL135
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	63, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL136
	 nop
.LL135:
	mov	0, %o4
	mov	0, %o5
.LL136:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE33:
	.size	extract_constant_offset__Long_extract_constant_offset_1, .-extract_constant_offset__Long_extract_constant_offset_1
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_2, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_2:
.LLFB34:
	save	%sp, -104, %sp
.LLCFI34:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 62
	ble	%icc, .LL138
	 nop
	mov	3937, %o0
	call	_m3_fault, 0
	 nop
.LL138:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL139
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	62, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL140
	 nop
.LL139:
	mov	0, %o4
	mov	0, %o5
.LL140:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE34:
	.size	extract_constant_offset__Long_extract_constant_offset_2, .-extract_constant_offset__Long_extract_constant_offset_2
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_3, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_3:
.LLFB35:
	save	%sp, -104, %sp
.LLCFI35:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 61
	ble	%icc, .LL142
	 nop
	mov	4001, %o0
	call	_m3_fault, 0
	 nop
.LL142:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL143
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	61, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL144
	 nop
.LL143:
	mov	0, %o4
	mov	0, %o5
.LL144:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE35:
	.size	extract_constant_offset__Long_extract_constant_offset_3, .-extract_constant_offset__Long_extract_constant_offset_3
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_4, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_4:
.LLFB36:
	save	%sp, -104, %sp
.LLCFI36:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 60
	ble	%icc, .LL146
	 nop
	mov	4065, %o0
	call	_m3_fault, 0
	 nop
.LL146:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL147
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	60, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL148
	 nop
.LL147:
	mov	0, %o4
	mov	0, %o5
.LL148:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	extract_constant_offset__Long_extract_constant_offset_4, .-extract_constant_offset__Long_extract_constant_offset_4
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_5, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_5:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 59
	ble	%icc, .LL150
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL150:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL151
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	59, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL152
	 nop
.LL151:
	mov	0, %o4
	mov	0, %o5
.LL152:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	extract_constant_offset__Long_extract_constant_offset_5, .-extract_constant_offset__Long_extract_constant_offset_5
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_6, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_6:
.LLFB38:
	save	%sp, -104, %sp
.LLCFI38:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 58
	ble	%icc, .LL154
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL154:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL155
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	58, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL156
	 nop
.LL155:
	mov	0, %o4
	mov	0, %o5
.LL156:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	extract_constant_offset__Long_extract_constant_offset_6, .-extract_constant_offset__Long_extract_constant_offset_6
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_7, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_7:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 57
	ble	%icc, .LL158
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL158:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL159
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	57, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL160
	 nop
.LL159:
	mov	0, %o4
	mov	0, %o5
.LL160:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	extract_constant_offset__Long_extract_constant_offset_7, .-extract_constant_offset__Long_extract_constant_offset_7
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_8, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_8:
.LLFB40:
	save	%sp, -104, %sp
.LLCFI40:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 56
	ble	%icc, .LL162
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL162:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL163
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	56, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL164
	 nop
.LL163:
	mov	0, %o4
	mov	0, %o5
.LL164:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE40:
	.size	extract_constant_offset__Long_extract_constant_offset_8, .-extract_constant_offset__Long_extract_constant_offset_8
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_9, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_9:
.LLFB41:
	save	%sp, -104, %sp
.LLCFI41:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 55
	ble	%icc, .LL166
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL166:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL167
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	55, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL168
	 nop
.LL167:
	mov	0, %o4
	mov	0, %o5
.LL168:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE41:
	.size	extract_constant_offset__Long_extract_constant_offset_9, .-extract_constant_offset__Long_extract_constant_offset_9
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_10, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_10:
.LLFB42:
	save	%sp, -104, %sp
.LLCFI42:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 54
	ble	%icc, .LL170
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL170:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL171
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	54, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
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
.LLFE42:
	.size	extract_constant_offset__Long_extract_constant_offset_10, .-extract_constant_offset__Long_extract_constant_offset_10
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_11, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_11:
.LLFB43:
	save	%sp, -104, %sp
.LLCFI43:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 53
	ble	%icc, .LL174
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL174:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL175
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	53, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
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
.LLFE43:
	.size	extract_constant_offset__Long_extract_constant_offset_11, .-extract_constant_offset__Long_extract_constant_offset_11
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_12, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_12:
.LLFB44:
	save	%sp, -104, %sp
.LLCFI44:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 52
	ble	%icc, .LL178
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL178:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL179
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	52, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
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
.LLFE44:
	.size	extract_constant_offset__Long_extract_constant_offset_12, .-extract_constant_offset__Long_extract_constant_offset_12
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_13, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_13:
.LLFB45:
	save	%sp, -104, %sp
.LLCFI45:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 51
	ble	%icc, .LL182
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL182:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL183
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	51, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL184
	 nop
.LL183:
	mov	0, %o4
	mov	0, %o5
.LL184:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE45:
	.size	extract_constant_offset__Long_extract_constant_offset_13, .-extract_constant_offset__Long_extract_constant_offset_13
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_14, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_14:
.LLFB46:
	save	%sp, -104, %sp
.LLCFI46:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 50
	ble	%icc, .LL186
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL186:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL187
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	50, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL188
	 nop
.LL187:
	mov	0, %o4
	mov	0, %o5
.LL188:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE46:
	.size	extract_constant_offset__Long_extract_constant_offset_14, .-extract_constant_offset__Long_extract_constant_offset_14
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_15, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_15:
.LLFB47:
	save	%sp, -104, %sp
.LLCFI47:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 49
	ble	%icc, .LL190
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL190:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL191
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	49, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL192
	 nop
.LL191:
	mov	0, %o4
	mov	0, %o5
.LL192:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE47:
	.size	extract_constant_offset__Long_extract_constant_offset_15, .-extract_constant_offset__Long_extract_constant_offset_15
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_16, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_16:
.LLFB48:
	save	%sp, -104, %sp
.LLCFI48:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 48
	ble	%icc, .LL194
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL194:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL195
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	48, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL196
	 nop
.LL195:
	mov	0, %o4
	mov	0, %o5
.LL196:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	extract_constant_offset__Long_extract_constant_offset_16, .-extract_constant_offset__Long_extract_constant_offset_16
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_17, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_17:
.LLFB49:
	save	%sp, -104, %sp
.LLCFI49:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 47
	ble	%icc, .LL198
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL198:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL199
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	47, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL200
	 nop
.LL199:
	mov	0, %o4
	mov	0, %o5
.LL200:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	extract_constant_offset__Long_extract_constant_offset_17, .-extract_constant_offset__Long_extract_constant_offset_17
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_18, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_18:
.LLFB50:
	save	%sp, -104, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 46
	ble	%icc, .LL202
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL202:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL203
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	46, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL204
	 nop
.LL203:
	mov	0, %o4
	mov	0, %o5
.LL204:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	extract_constant_offset__Long_extract_constant_offset_18, .-extract_constant_offset__Long_extract_constant_offset_18
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_19, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_19:
.LLFB51:
	save	%sp, -104, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 45
	ble	%icc, .LL206
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL206:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL207
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	45, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL208
	 nop
.LL207:
	mov	0, %o4
	mov	0, %o5
.LL208:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	extract_constant_offset__Long_extract_constant_offset_19, .-extract_constant_offset__Long_extract_constant_offset_19
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_20, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_20:
.LLFB52:
	save	%sp, -104, %sp
.LLCFI52:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 44
	ble	%icc, .LL210
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL210:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL211
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	44, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL212
	 nop
.LL211:
	mov	0, %o4
	mov	0, %o5
.LL212:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	extract_constant_offset__Long_extract_constant_offset_20, .-extract_constant_offset__Long_extract_constant_offset_20
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_21, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_21:
.LLFB53:
	save	%sp, -104, %sp
.LLCFI53:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 43
	ble	%icc, .LL214
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL214:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL215
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	43, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL216
	 nop
.LL215:
	mov	0, %o4
	mov	0, %o5
.LL216:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	extract_constant_offset__Long_extract_constant_offset_21, .-extract_constant_offset__Long_extract_constant_offset_21
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_22, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_22:
.LLFB54:
	save	%sp, -104, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 42
	ble	%icc, .LL218
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL218:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL219
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	42, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL220
	 nop
.LL219:
	mov	0, %o4
	mov	0, %o5
.LL220:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	extract_constant_offset__Long_extract_constant_offset_22, .-extract_constant_offset__Long_extract_constant_offset_22
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_23, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_23:
.LLFB55:
	save	%sp, -104, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 41
	ble	%icc, .LL222
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL222:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL223
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	41, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL224
	 nop
.LL223:
	mov	0, %o4
	mov	0, %o5
.LL224:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	extract_constant_offset__Long_extract_constant_offset_23, .-extract_constant_offset__Long_extract_constant_offset_23
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_24, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_24:
.LLFB56:
	save	%sp, -104, %sp
.LLCFI56:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 40
	ble	%icc, .LL226
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL226:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL227
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	40, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL228
	 nop
.LL227:
	mov	0, %o4
	mov	0, %o5
.LL228:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	extract_constant_offset__Long_extract_constant_offset_24, .-extract_constant_offset__Long_extract_constant_offset_24
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_25, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_25:
.LLFB57:
	save	%sp, -104, %sp
.LLCFI57:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 39
	ble	%icc, .LL230
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL230:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL231
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	39, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL232
	 nop
.LL231:
	mov	0, %o4
	mov	0, %o5
.LL232:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	extract_constant_offset__Long_extract_constant_offset_25, .-extract_constant_offset__Long_extract_constant_offset_25
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_26, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_26:
.LLFB58:
	save	%sp, -104, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 38
	ble	%icc, .LL234
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL234:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL235
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	38, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL236
	 nop
.LL235:
	mov	0, %o4
	mov	0, %o5
.LL236:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	extract_constant_offset__Long_extract_constant_offset_26, .-extract_constant_offset__Long_extract_constant_offset_26
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_27, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_27:
.LLFB59:
	save	%sp, -104, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 37
	ble	%icc, .LL238
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL238:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL239
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	37, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL240
	 nop
.LL239:
	mov	0, %o4
	mov	0, %o5
.LL240:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	extract_constant_offset__Long_extract_constant_offset_27, .-extract_constant_offset__Long_extract_constant_offset_27
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_28, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_28:
.LLFB60:
	save	%sp, -104, %sp
.LLCFI60:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 36
	ble	%icc, .LL242
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL242:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL243
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	36, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL244
	 nop
.LL243:
	mov	0, %o4
	mov	0, %o5
.LL244:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	extract_constant_offset__Long_extract_constant_offset_28, .-extract_constant_offset__Long_extract_constant_offset_28
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_29, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_29:
.LLFB61:
	save	%sp, -104, %sp
.LLCFI61:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 35
	ble	%icc, .LL246
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL246:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL247
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	35, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL248
	 nop
.LL247:
	mov	0, %o4
	mov	0, %o5
.LL248:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	extract_constant_offset__Long_extract_constant_offset_29, .-extract_constant_offset__Long_extract_constant_offset_29
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_30, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_30:
.LLFB62:
	save	%sp, -104, %sp
.LLCFI62:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 34
	ble	%icc, .LL250
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL250:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL251
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	34, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL252
	 nop
.LL251:
	mov	0, %o4
	mov	0, %o5
.LL252:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	extract_constant_offset__Long_extract_constant_offset_30, .-extract_constant_offset__Long_extract_constant_offset_30
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_31, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_31:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 33
	ble	%icc, .LL254
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL254:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL255
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	33, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL256
	 nop
.LL255:
	mov	0, %o4
	mov	0, %o5
.LL256:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	extract_constant_offset__Long_extract_constant_offset_31, .-extract_constant_offset__Long_extract_constant_offset_31
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_32, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_32:
.LLFB64:
	save	%sp, -104, %sp
.LLCFI64:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	ble	%icc, .LL258
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL258:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL259
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	32, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL260
	 nop
.LL259:
	mov	0, %o4
	mov	0, %o5
.LL260:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	extract_constant_offset__Long_extract_constant_offset_32, .-extract_constant_offset__Long_extract_constant_offset_32
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_33, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_33:
.LLFB65:
	save	%sp, -104, %sp
.LLCFI65:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 31
	ble	%icc, .LL262
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL262:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL263
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	31, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL264
	 nop
.LL263:
	mov	0, %o4
	mov	0, %o5
.LL264:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	extract_constant_offset__Long_extract_constant_offset_33, .-extract_constant_offset__Long_extract_constant_offset_33
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_34, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_34:
.LLFB66:
	save	%sp, -104, %sp
.LLCFI66:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 30
	ble	%icc, .LL266
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL266:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL267
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	30, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL268
	 nop
.LL267:
	mov	0, %o4
	mov	0, %o5
.LL268:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	extract_constant_offset__Long_extract_constant_offset_34, .-extract_constant_offset__Long_extract_constant_offset_34
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_35, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_35:
.LLFB67:
	save	%sp, -104, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 29
	ble	%icc, .LL270
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL270:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL271
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	29, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL272
	 nop
.LL271:
	mov	0, %o4
	mov	0, %o5
.LL272:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	extract_constant_offset__Long_extract_constant_offset_35, .-extract_constant_offset__Long_extract_constant_offset_35
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_36, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_36:
.LLFB68:
	save	%sp, -104, %sp
.LLCFI68:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 28
	ble	%icc, .LL274
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL274:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL275
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	28, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL276
	 nop
.LL275:
	mov	0, %o4
	mov	0, %o5
.LL276:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	extract_constant_offset__Long_extract_constant_offset_36, .-extract_constant_offset__Long_extract_constant_offset_36
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_37, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_37:
.LLFB69:
	save	%sp, -104, %sp
.LLCFI69:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 27
	ble	%icc, .LL278
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL278:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL279
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	27, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL280
	 nop
.LL279:
	mov	0, %o4
	mov	0, %o5
.LL280:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	extract_constant_offset__Long_extract_constant_offset_37, .-extract_constant_offset__Long_extract_constant_offset_37
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_38, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_38:
.LLFB70:
	save	%sp, -104, %sp
.LLCFI70:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 26
	ble	%icc, .LL282
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL282:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL283
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	26, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL284
	 nop
.LL283:
	mov	0, %o4
	mov	0, %o5
.LL284:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	extract_constant_offset__Long_extract_constant_offset_38, .-extract_constant_offset__Long_extract_constant_offset_38
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_39, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_39:
.LLFB71:
	save	%sp, -104, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 25
	ble	%icc, .LL286
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL286:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL287
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	25, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL288
	 nop
.LL287:
	mov	0, %o4
	mov	0, %o5
.LL288:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	extract_constant_offset__Long_extract_constant_offset_39, .-extract_constant_offset__Long_extract_constant_offset_39
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_40, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_40:
.LLFB72:
	save	%sp, -104, %sp
.LLCFI72:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 24
	ble	%icc, .LL290
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL290:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL291
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	24, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL292
	 nop
.LL291:
	mov	0, %o4
	mov	0, %o5
.LL292:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	extract_constant_offset__Long_extract_constant_offset_40, .-extract_constant_offset__Long_extract_constant_offset_40
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_41, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_41:
.LLFB73:
	save	%sp, -104, %sp
.LLCFI73:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 23
	ble	%icc, .LL294
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL294:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL295
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	23, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL296
	 nop
.LL295:
	mov	0, %o4
	mov	0, %o5
.LL296:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	extract_constant_offset__Long_extract_constant_offset_41, .-extract_constant_offset__Long_extract_constant_offset_41
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_42, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_42:
.LLFB74:
	save	%sp, -104, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 22
	ble	%icc, .LL298
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL298:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL299
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	22, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL300
	 nop
.LL299:
	mov	0, %o4
	mov	0, %o5
.LL300:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	extract_constant_offset__Long_extract_constant_offset_42, .-extract_constant_offset__Long_extract_constant_offset_42
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_43, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_43:
.LLFB75:
	save	%sp, -104, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 21
	ble	%icc, .LL302
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL302:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL303
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	21, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL304
	 nop
.LL303:
	mov	0, %o4
	mov	0, %o5
.LL304:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	extract_constant_offset__Long_extract_constant_offset_43, .-extract_constant_offset__Long_extract_constant_offset_43
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_44, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_44:
.LLFB76:
	save	%sp, -104, %sp
.LLCFI76:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 20
	ble	%icc, .LL306
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL306:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL307
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	20, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL308
	 nop
.LL307:
	mov	0, %o4
	mov	0, %o5
.LL308:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	extract_constant_offset__Long_extract_constant_offset_44, .-extract_constant_offset__Long_extract_constant_offset_44
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_45, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_45:
.LLFB77:
	save	%sp, -104, %sp
.LLCFI77:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 19
	ble	%icc, .LL310
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL310:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL311
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	19, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL312
	 nop
.LL311:
	mov	0, %o4
	mov	0, %o5
.LL312:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	extract_constant_offset__Long_extract_constant_offset_45, .-extract_constant_offset__Long_extract_constant_offset_45
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_46, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_46:
.LLFB78:
	save	%sp, -104, %sp
.LLCFI78:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 18
	ble	%icc, .LL314
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL314:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL315
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	18, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL316
	 nop
.LL315:
	mov	0, %o4
	mov	0, %o5
.LL316:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	extract_constant_offset__Long_extract_constant_offset_46, .-extract_constant_offset__Long_extract_constant_offset_46
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_47, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_47:
.LLFB79:
	save	%sp, -104, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 17
	ble	%icc, .LL318
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL318:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL319
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	17, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL320
	 nop
.LL319:
	mov	0, %o4
	mov	0, %o5
.LL320:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	extract_constant_offset__Long_extract_constant_offset_47, .-extract_constant_offset__Long_extract_constant_offset_47
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_48, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_48:
.LLFB80:
	save	%sp, -104, %sp
.LLCFI80:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 16
	ble	%icc, .LL322
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL322:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL323
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	16, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL324
	 nop
.LL323:
	mov	0, %o4
	mov	0, %o5
.LL324:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	extract_constant_offset__Long_extract_constant_offset_48, .-extract_constant_offset__Long_extract_constant_offset_48
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_49, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_49:
.LLFB81:
	save	%sp, -104, %sp
.LLCFI81:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 15
	ble	%icc, .LL326
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL326:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL327
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	15, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL328
	 nop
.LL327:
	mov	0, %o4
	mov	0, %o5
.LL328:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	extract_constant_offset__Long_extract_constant_offset_49, .-extract_constant_offset__Long_extract_constant_offset_49
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_50, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_50:
.LLFB82:
	save	%sp, -104, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 14
	ble	%icc, .LL330
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL330:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL331
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	14, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL332
	 nop
.LL331:
	mov	0, %o4
	mov	0, %o5
.LL332:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	extract_constant_offset__Long_extract_constant_offset_50, .-extract_constant_offset__Long_extract_constant_offset_50
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_51, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_51:
.LLFB83:
	save	%sp, -104, %sp
.LLCFI83:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 13
	ble	%icc, .LL334
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL334:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL335
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	13, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL336
	 nop
.LL335:
	mov	0, %o4
	mov	0, %o5
.LL336:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	extract_constant_offset__Long_extract_constant_offset_51, .-extract_constant_offset__Long_extract_constant_offset_51
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_52, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_52:
.LLFB84:
	save	%sp, -104, %sp
.LLCFI84:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 12
	ble	%icc, .LL338
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL338:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL339
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	12, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL340
	 nop
.LL339:
	mov	0, %o4
	mov	0, %o5
.LL340:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	extract_constant_offset__Long_extract_constant_offset_52, .-extract_constant_offset__Long_extract_constant_offset_52
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_53, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_53:
.LLFB85:
	save	%sp, -104, %sp
.LLCFI85:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 11
	ble	%icc, .LL342
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL342:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL343
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	11, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL344
	 nop
.LL343:
	mov	0, %o4
	mov	0, %o5
.LL344:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	extract_constant_offset__Long_extract_constant_offset_53, .-extract_constant_offset__Long_extract_constant_offset_53
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_54, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_54:
.LLFB86:
	save	%sp, -104, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 10
	ble	%icc, .LL346
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL346:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL347
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	10, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL348
	 nop
.LL347:
	mov	0, %o4
	mov	0, %o5
.LL348:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	extract_constant_offset__Long_extract_constant_offset_54, .-extract_constant_offset__Long_extract_constant_offset_54
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_55, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_55:
.LLFB87:
	save	%sp, -104, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 9
	ble	%icc, .LL350
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL350:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL351
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	9, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL352
	 nop
.LL351:
	mov	0, %o4
	mov	0, %o5
.LL352:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	extract_constant_offset__Long_extract_constant_offset_55, .-extract_constant_offset__Long_extract_constant_offset_55
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_56, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_56:
.LLFB88:
	save	%sp, -104, %sp
.LLCFI88:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 8
	ble	%icc, .LL354
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL354:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL355
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	8, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL356
	 nop
.LL355:
	mov	0, %o4
	mov	0, %o5
.LL356:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	extract_constant_offset__Long_extract_constant_offset_56, .-extract_constant_offset__Long_extract_constant_offset_56
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_57, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_57:
.LLFB89:
	save	%sp, -104, %sp
.LLCFI89:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 7
	ble	%icc, .LL358
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL358:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL359
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	7, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL360
	 nop
.LL359:
	mov	0, %o4
	mov	0, %o5
.LL360:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	extract_constant_offset__Long_extract_constant_offset_57, .-extract_constant_offset__Long_extract_constant_offset_57
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_58, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_58:
.LLFB90:
	save	%sp, -104, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 6
	ble	%icc, .LL362
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL362:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL363
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	6, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL364
	 nop
.LL363:
	mov	0, %o4
	mov	0, %o5
.LL364:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	extract_constant_offset__Long_extract_constant_offset_58, .-extract_constant_offset__Long_extract_constant_offset_58
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_59, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_59:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 5
	ble	%icc, .LL366
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL366:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL367
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	5, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL368
	 nop
.LL367:
	mov	0, %o4
	mov	0, %o5
.LL368:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	extract_constant_offset__Long_extract_constant_offset_59, .-extract_constant_offset__Long_extract_constant_offset_59
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_60, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_60:
.LLFB92:
	save	%sp, -104, %sp
.LLCFI92:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 4
	ble	%icc, .LL370
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL370:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL371
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	4, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL372
	 nop
.LL371:
	mov	0, %o4
	mov	0, %o5
.LL372:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	extract_constant_offset__Long_extract_constant_offset_60, .-extract_constant_offset__Long_extract_constant_offset_60
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_61, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_61:
.LLFB93:
	save	%sp, -104, %sp
.LLCFI93:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 3
	ble	%icc, .LL374
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL374:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL375
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	3, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL376
	 nop
.LL375:
	mov	0, %o4
	mov	0, %o5
.LL376:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	extract_constant_offset__Long_extract_constant_offset_61, .-extract_constant_offset__Long_extract_constant_offset_61
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_62, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_62:
.LLFB94:
	save	%sp, -104, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 2
	ble	%icc, .LL378
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL378:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL379
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	2, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL380
	 nop
.LL379:
	mov	0, %o4
	mov	0, %o5
.LL380:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	extract_constant_offset__Long_extract_constant_offset_62, .-extract_constant_offset__Long_extract_constant_offset_62
	.align 4
	.type	extract_constant_offset__Long_extract_constant_offset_63, #function
	.proc	05
extract_constant_offset__Long_extract_constant_offset_63:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 1
	ble	%icc, .LL382
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL382:
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL383
	 nop
	ldd	[%fp-8], %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	1, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	64, %o3
	sub	%o3, %g1, %g1
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	srlx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	ba,pt	%xcc, .LL384
	 nop
.LL383:
	mov	0, %o4
	mov	0, %o5
.LL384:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	extract_constant_offset__Long_extract_constant_offset_63, .-extract_constant_offset__Long_extract_constant_offset_63
	.align 4
	.global extract_constant_offset_M3
	.type	extract_constant_offset_M3, #function
	.proc	0120
extract_constant_offset_M3:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	extract_constant_offset_M3, .-extract_constant_offset_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	st	%i0, [%fp+68]
	sethi	%hi(MM_extract_constant_offset), %g1
	or	%g1, %lo(MM_extract_constant_offset), %o0
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
	.size	L_1, 3904
L_1:
	.ascii	"extract_constant_offset_M3"
	.skip 1
	.ascii	"Long_extract_constant_offset_63"
	.skip 1
	.ascii	"Long_extract_constant_offset_62"
	.skip 1
	.ascii	"Long_extract_constant_offset_61"
	.skip 1
	.ascii	"Long_extract_constant_offset_60"
	.skip 1
	.ascii	"Long_extract_constant_offset_59"
	.skip 1
	.ascii	"Long_extract_constant_offset_58"
	.skip 1
	.ascii	"Long_extract_constant_offset_57"
	.skip 1
	.ascii	"Long_extract_constant_offset_56"
	.skip 1
	.ascii	"Long_extract_constant_offset_55"
	.skip 1
	.ascii	"Long_extract_constant_offset_54"
	.skip 1
	.ascii	"Long_extract_constant_offset_53"
	.skip 1
	.ascii	"Long_extract_constant_offset_52"
	.skip 1
	.ascii	"Long_extract_constant_offset_51"
	.skip 1
	.ascii	"Long_extract_constant_offset_50"
	.skip 1
	.ascii	"Long_extract_constant_offset_49"
	.skip 1
	.ascii	"Long_extract_constant_offset_48"
	.skip 1
	.ascii	"Long_extract_constant_offset_47"
	.skip 1
	.ascii	"Long_extract_constant_offset_46"
	.skip 1
	.ascii	"Long_extract_constant_offset_45"
	.skip 1
	.ascii	"Long_extract_constant_offset_44"
	.skip 1
	.ascii	"Long_extract_constant_offset_43"
	.skip 1
	.ascii	"Long_extract_constant_offset_42"
	.skip 1
	.ascii	"Long_extract_constant_offset_41"
	.skip 1
	.ascii	"Long_extract_constant_offset_40"
	.skip 1
	.ascii	"Long_extract_constant_offset_39"
	.skip 1
	.ascii	"Long_extract_constant_offset_38"
	.skip 1
	.ascii	"Long_extract_constant_offset_37"
	.skip 1
	.ascii	"Long_extract_constant_offset_36"
	.skip 1
	.ascii	"Long_extract_constant_offset_35"
	.skip 1
	.ascii	"Long_extract_constant_offset_34"
	.skip 1
	.ascii	"Long_extract_constant_offset_33"
	.skip 1
	.ascii	"Long_extract_constant_offset_32"
	.skip 1
	.ascii	"Long_extract_constant_offset_31"
	.skip 1
	.ascii	"Long_extract_constant_offset_30"
	.skip 1
	.ascii	"Long_extract_constant_offset_29"
	.skip 1
	.ascii	"Long_extract_constant_offset_28"
	.skip 1
	.ascii	"Long_extract_constant_offset_27"
	.skip 1
	.ascii	"Long_extract_constant_offset_26"
	.skip 1
	.ascii	"Long_extract_constant_offset_25"
	.skip 1
	.ascii	"Long_extract_constant_offset_24"
	.skip 1
	.ascii	"Long_extract_constant_offset_23"
	.skip 1
	.ascii	"Long_extract_constant_offset_22"
	.skip 1
	.ascii	"Long_extract_constant_offset_21"
	.skip 1
	.ascii	"Long_extract_constant_offset_20"
	.skip 1
	.ascii	"Long_extract_constant_offset_19"
	.skip 1
	.ascii	"Long_extract_constant_offset_18"
	.skip 1
	.ascii	"Long_extract_constant_offset_17"
	.skip 1
	.ascii	"Long_extract_constant_offset_16"
	.skip 1
	.ascii	"Long_extract_constant_offset_15"
	.skip 1
	.ascii	"Long_extract_constant_offset_14"
	.skip 1
	.ascii	"Long_extract_constant_offset_13"
	.skip 1
	.ascii	"Long_extract_constant_offset_12"
	.skip 1
	.ascii	"Long_extract_constant_offset_11"
	.skip 1
	.ascii	"Long_extract_constant_offset_10"
	.skip 1
	.ascii	"Long_extract_constant_offset_9"
	.skip 1
	.ascii	"Long_extract_constant_offset_8"
	.skip 1
	.ascii	"Long_extract_constant_offset_7"
	.skip 1
	.ascii	"Long_extract_constant_offset_6"
	.skip 1
	.ascii	"Long_extract_constant_offset_5"
	.skip 1
	.ascii	"Long_extract_constant_offset_4"
	.skip 1
	.ascii	"Long_extract_constant_offset_3"
	.skip 1
	.ascii	"Long_extract_constant_offset_2"
	.skip 1
	.ascii	"Long_extract_constant_offset_1"
	.skip 1
	.ascii	"Long_extract_constant_offset_0"
	.skip 1
	.ascii	"Word_extract_constant_offset_31"
	.skip 1
	.ascii	"Word_extract_constant_offset_30"
	.skip 1
	.ascii	"Word_extract_constant_offset_29"
	.skip 1
	.ascii	"Word_extract_constant_offset_28"
	.skip 1
	.ascii	"Word_extract_constant_offset_27"
	.skip 1
	.ascii	"Word_extract_constant_offset_26"
	.skip 1
	.ascii	"Word_extract_constant_offset_25"
	.skip 1
	.ascii	"Word_extract_constant_offset_24"
	.skip 1
	.ascii	"Word_extract_constant_offset_23"
	.skip 1
	.ascii	"Word_extract_constant_offset_22"
	.skip 1
	.ascii	"Word_extract_constant_offset_21"
	.skip 1
	.ascii	"Word_extract_constant_offset_20"
	.skip 1
	.ascii	"Word_extract_constant_offset_19"
	.skip 1
	.ascii	"Word_extract_constant_offset_18"
	.skip 1
	.ascii	"Word_extract_constant_offset_17"
	.skip 1
	.ascii	"Word_extract_constant_offset_16"
	.skip 1
	.ascii	"Word_extract_constant_offset_15"
	.skip 1
	.ascii	"Word_extract_constant_offset_14"
	.skip 1
	.ascii	"Word_extract_constant_offset_13"
	.skip 1
	.ascii	"Word_extract_constant_offset_12"
	.skip 1
	.ascii	"Word_extract_constant_offset_11"
	.skip 1
	.ascii	"Word_extract_constant_offset_10"
	.skip 1
	.ascii	"Word_extract_constant_offset_9"
	.skip 1
	.ascii	"Word_extract_constant_offset_8"
	.skip 1
	.ascii	"Word_extract_constant_offset_7"
	.skip 1
	.ascii	"Word_extract_constant_offset_6"
	.skip 1
	.ascii	"Word_extract_constant_offset_5"
	.skip 1
	.ascii	"Word_extract_constant_offset_4"
	.skip 1
	.ascii	"Word_extract_constant_offset_3"
	.skip 1
	.ascii	"Word_extract_constant_offset_2"
	.skip 1
	.ascii	"Word_extract_constant_offset_1"
	.skip 1
	.ascii	"Word_extract_constant_offset_0"
	.skip 2
	.long	extract_constant_offset_M3
	.long	L_1
	.long	extract_constant_offset__Long_extract_constant_offset_63
	.long	L_1+27
	.long	extract_constant_offset__Long_extract_constant_offset_62
	.long	L_1+59
	.long	extract_constant_offset__Long_extract_constant_offset_61
	.long	L_1+91
	.long	extract_constant_offset__Long_extract_constant_offset_60
	.long	L_1+123
	.long	extract_constant_offset__Long_extract_constant_offset_59
	.long	L_1+155
	.long	extract_constant_offset__Long_extract_constant_offset_58
	.long	L_1+187
	.long	extract_constant_offset__Long_extract_constant_offset_57
	.long	L_1+219
	.long	extract_constant_offset__Long_extract_constant_offset_56
	.long	L_1+251
	.long	extract_constant_offset__Long_extract_constant_offset_55
	.long	L_1+283
	.long	extract_constant_offset__Long_extract_constant_offset_54
	.long	L_1+315
	.long	extract_constant_offset__Long_extract_constant_offset_53
	.long	L_1+347
	.long	extract_constant_offset__Long_extract_constant_offset_52
	.long	L_1+379
	.long	extract_constant_offset__Long_extract_constant_offset_51
	.long	L_1+411
	.long	extract_constant_offset__Long_extract_constant_offset_50
	.long	L_1+443
	.long	extract_constant_offset__Long_extract_constant_offset_49
	.long	L_1+475
	.long	extract_constant_offset__Long_extract_constant_offset_48
	.long	L_1+507
	.long	extract_constant_offset__Long_extract_constant_offset_47
	.long	L_1+539
	.long	extract_constant_offset__Long_extract_constant_offset_46
	.long	L_1+571
	.long	extract_constant_offset__Long_extract_constant_offset_45
	.long	L_1+603
	.long	extract_constant_offset__Long_extract_constant_offset_44
	.long	L_1+635
	.long	extract_constant_offset__Long_extract_constant_offset_43
	.long	L_1+667
	.long	extract_constant_offset__Long_extract_constant_offset_42
	.long	L_1+699
	.long	extract_constant_offset__Long_extract_constant_offset_41
	.long	L_1+731
	.long	extract_constant_offset__Long_extract_constant_offset_40
	.long	L_1+763
	.long	extract_constant_offset__Long_extract_constant_offset_39
	.long	L_1+795
	.long	extract_constant_offset__Long_extract_constant_offset_38
	.long	L_1+827
	.long	extract_constant_offset__Long_extract_constant_offset_37
	.long	L_1+859
	.long	extract_constant_offset__Long_extract_constant_offset_36
	.long	L_1+891
	.long	extract_constant_offset__Long_extract_constant_offset_35
	.long	L_1+923
	.long	extract_constant_offset__Long_extract_constant_offset_34
	.long	L_1+955
	.long	extract_constant_offset__Long_extract_constant_offset_33
	.long	L_1+987
	.long	extract_constant_offset__Long_extract_constant_offset_32
	.long	L_1+1019
	.long	extract_constant_offset__Long_extract_constant_offset_31
	.long	L_1+1051
	.long	extract_constant_offset__Long_extract_constant_offset_30
	.long	L_1+1083
	.long	extract_constant_offset__Long_extract_constant_offset_29
	.long	L_1+1115
	.long	extract_constant_offset__Long_extract_constant_offset_28
	.long	L_1+1147
	.long	extract_constant_offset__Long_extract_constant_offset_27
	.long	L_1+1179
	.long	extract_constant_offset__Long_extract_constant_offset_26
	.long	L_1+1211
	.long	extract_constant_offset__Long_extract_constant_offset_25
	.long	L_1+1243
	.long	extract_constant_offset__Long_extract_constant_offset_24
	.long	L_1+1275
	.long	extract_constant_offset__Long_extract_constant_offset_23
	.long	L_1+1307
	.long	extract_constant_offset__Long_extract_constant_offset_22
	.long	L_1+1339
	.long	extract_constant_offset__Long_extract_constant_offset_21
	.long	L_1+1371
	.long	extract_constant_offset__Long_extract_constant_offset_20
	.long	L_1+1403
	.long	extract_constant_offset__Long_extract_constant_offset_19
	.long	L_1+1435
	.long	extract_constant_offset__Long_extract_constant_offset_18
	.long	L_1+1467
	.long	extract_constant_offset__Long_extract_constant_offset_17
	.long	L_1+1499
	.long	extract_constant_offset__Long_extract_constant_offset_16
	.long	L_1+1531
	.long	extract_constant_offset__Long_extract_constant_offset_15
	.long	L_1+1563
	.long	extract_constant_offset__Long_extract_constant_offset_14
	.long	L_1+1595
	.long	extract_constant_offset__Long_extract_constant_offset_13
	.long	L_1+1627
	.long	extract_constant_offset__Long_extract_constant_offset_12
	.long	L_1+1659
	.long	extract_constant_offset__Long_extract_constant_offset_11
	.long	L_1+1691
	.long	extract_constant_offset__Long_extract_constant_offset_10
	.long	L_1+1723
	.long	extract_constant_offset__Long_extract_constant_offset_9
	.long	L_1+1755
	.long	extract_constant_offset__Long_extract_constant_offset_8
	.long	L_1+1786
	.long	extract_constant_offset__Long_extract_constant_offset_7
	.long	L_1+1817
	.long	extract_constant_offset__Long_extract_constant_offset_6
	.long	L_1+1848
	.long	extract_constant_offset__Long_extract_constant_offset_5
	.long	L_1+1879
	.long	extract_constant_offset__Long_extract_constant_offset_4
	.long	L_1+1910
	.long	extract_constant_offset__Long_extract_constant_offset_3
	.long	L_1+1941
	.long	extract_constant_offset__Long_extract_constant_offset_2
	.long	L_1+1972
	.long	extract_constant_offset__Long_extract_constant_offset_1
	.long	L_1+2003
	.long	extract_constant_offset__Long_extract_constant_offset_0
	.long	L_1+2034
	.long	extract_constant_offset__Word_extract_constant_offset_31
	.long	L_1+2065
	.long	extract_constant_offset__Word_extract_constant_offset_30
	.long	L_1+2097
	.long	extract_constant_offset__Word_extract_constant_offset_29
	.long	L_1+2129
	.long	extract_constant_offset__Word_extract_constant_offset_28
	.long	L_1+2161
	.long	extract_constant_offset__Word_extract_constant_offset_27
	.long	L_1+2193
	.long	extract_constant_offset__Word_extract_constant_offset_26
	.long	L_1+2225
	.long	extract_constant_offset__Word_extract_constant_offset_25
	.long	L_1+2257
	.long	extract_constant_offset__Word_extract_constant_offset_24
	.long	L_1+2289
	.long	extract_constant_offset__Word_extract_constant_offset_23
	.long	L_1+2321
	.long	extract_constant_offset__Word_extract_constant_offset_22
	.long	L_1+2353
	.long	extract_constant_offset__Word_extract_constant_offset_21
	.long	L_1+2385
	.long	extract_constant_offset__Word_extract_constant_offset_20
	.long	L_1+2417
	.long	extract_constant_offset__Word_extract_constant_offset_19
	.long	L_1+2449
	.long	extract_constant_offset__Word_extract_constant_offset_18
	.long	L_1+2481
	.long	extract_constant_offset__Word_extract_constant_offset_17
	.long	L_1+2513
	.long	extract_constant_offset__Word_extract_constant_offset_16
	.long	L_1+2545
	.long	extract_constant_offset__Word_extract_constant_offset_15
	.long	L_1+2577
	.long	extract_constant_offset__Word_extract_constant_offset_14
	.long	L_1+2609
	.long	extract_constant_offset__Word_extract_constant_offset_13
	.long	L_1+2641
	.long	extract_constant_offset__Word_extract_constant_offset_12
	.long	L_1+2673
	.long	extract_constant_offset__Word_extract_constant_offset_11
	.long	L_1+2705
	.long	extract_constant_offset__Word_extract_constant_offset_10
	.long	L_1+2737
	.long	extract_constant_offset__Word_extract_constant_offset_9
	.long	L_1+2769
	.long	extract_constant_offset__Word_extract_constant_offset_8
	.long	L_1+2800
	.long	extract_constant_offset__Word_extract_constant_offset_7
	.long	L_1+2831
	.long	extract_constant_offset__Word_extract_constant_offset_6
	.long	L_1+2862
	.long	extract_constant_offset__Word_extract_constant_offset_5
	.long	L_1+2893
	.long	extract_constant_offset__Word_extract_constant_offset_4
	.long	L_1+2924
	.long	extract_constant_offset__Word_extract_constant_offset_3
	.long	L_1+2955
	.long	extract_constant_offset__Word_extract_constant_offset_2
	.long	L_1+2986
	.long	extract_constant_offset__Word_extract_constant_offset_1
	.long	L_1+3017
	.long	extract_constant_offset__Word_extract_constant_offset_0
	.long	L_1+3048
	.skip 4
	.ascii	"../SOLsun/extract_constant_offset.m3"
	.skip 8
	.align 8
	.type	MM_extract_constant_offset, #object
	.size	MM_extract_constant_offset, 208
MM_extract_constant_offset:
	.long	L_1+3860
	.skip 16
	.long	L_1+3080
	.skip 12
	.long	MM_extract_constant_offset+148
	.skip 4
	.long	extract_constant_offset_M3
	.long	3
	.half	1073
	.skip 2
	.long	0
	.long	1074
	.long	0
	.long	1075
	.long	1076
	.half	1077
	.skip 2
	.long	1078
	.skip 4
	.long	0
	.long	1079
	.long	1080
	.skip 4
	.long	1083237486
	.long	-872737355
	.byte	59
	.skip 7
	.long	0
	.long	1084
	.byte	61
	.skip 3
	.long	1086
	.long	1149756283
	.skip 12
	.long	extract_constant_offset_I3
	.long	MM_extract_constant_offset+160
	.skip 4
	.long	Long_I3
	.long	MM_extract_constant_offset+172
	.skip 4
	.long	Word_I3
	.long	MM_extract_constant_offset+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_extract_constant_offset+196
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
