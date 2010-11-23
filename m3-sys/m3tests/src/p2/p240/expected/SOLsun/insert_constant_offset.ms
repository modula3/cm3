	.file	"insert_constant_offset.mc"
	.section	".text"
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_0, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_0:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	ble	%icc, .LL2
	 nop
	mov	1761, %o0
	call	_m3_fault, 0
	 nop
.LL2:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL3
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL4
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL5
	 nop
.LL4:
	ld	[%fp+72], %g1
.LL5:
	ba,pt	%xcc, .LL6
	 nop
.LL3:
	ld	[%fp+68], %g1
.LL6:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	insert_constant_offset__Word_insert_constant_offset_0, .-insert_constant_offset__Word_insert_constant_offset_0
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_1, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_1:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 31
	ble	%icc, .LL8
	 nop
	mov	1825, %o0
	call	_m3_fault, 0
	 nop
.LL8:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL9
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL10
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	add	%g1, %g1, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	add	%g1, %g1, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL11
	 nop
.LL10:
	ld	[%fp+72], %g1
.LL11:
	ba,pt	%xcc, .LL12
	 nop
.LL9:
	ld	[%fp+68], %g1
.LL12:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	insert_constant_offset__Word_insert_constant_offset_1, .-insert_constant_offset__Word_insert_constant_offset_1
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_2, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_2:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 30
	ble	%icc, .LL14
	 nop
	mov	1889, %o0
	call	_m3_fault, 0
	 nop
.LL14:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL15
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL16
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 2, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 2, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL17
	 nop
.LL16:
	ld	[%fp+72], %g1
.LL17:
	ba,pt	%xcc, .LL18
	 nop
.LL15:
	ld	[%fp+68], %g1
.LL18:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	insert_constant_offset__Word_insert_constant_offset_2, .-insert_constant_offset__Word_insert_constant_offset_2
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_3, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_3:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 29
	ble	%icc, .LL20
	 nop
	mov	1953, %o0
	call	_m3_fault, 0
	 nop
.LL20:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL21
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL22
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 3, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 3, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL23
	 nop
.LL22:
	ld	[%fp+72], %g1
.LL23:
	ba,pt	%xcc, .LL24
	 nop
.LL21:
	ld	[%fp+68], %g1
.LL24:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	insert_constant_offset__Word_insert_constant_offset_3, .-insert_constant_offset__Word_insert_constant_offset_3
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_4, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_4:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 28
	ble	%icc, .LL26
	 nop
	mov	2017, %o0
	call	_m3_fault, 0
	 nop
.LL26:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL27
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL28
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 4, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 4, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL29
	 nop
.LL28:
	ld	[%fp+72], %g1
.LL29:
	ba,pt	%xcc, .LL30
	 nop
.LL27:
	ld	[%fp+68], %g1
.LL30:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	insert_constant_offset__Word_insert_constant_offset_4, .-insert_constant_offset__Word_insert_constant_offset_4
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_5, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_5:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 27
	ble	%icc, .LL32
	 nop
	mov	2081, %o0
	call	_m3_fault, 0
	 nop
.LL32:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL33
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL34
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 5, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 5, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL35
	 nop
.LL34:
	ld	[%fp+72], %g1
.LL35:
	ba,pt	%xcc, .LL36
	 nop
.LL33:
	ld	[%fp+68], %g1
.LL36:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	insert_constant_offset__Word_insert_constant_offset_5, .-insert_constant_offset__Word_insert_constant_offset_5
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_6, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_6:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 26
	ble	%icc, .LL38
	 nop
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL38:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL39
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL40
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 6, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 6, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL41
	 nop
.LL40:
	ld	[%fp+72], %g1
.LL41:
	ba,pt	%xcc, .LL42
	 nop
.LL39:
	ld	[%fp+68], %g1
.LL42:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	insert_constant_offset__Word_insert_constant_offset_6, .-insert_constant_offset__Word_insert_constant_offset_6
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_7, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_7:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 25
	ble	%icc, .LL44
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL44:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL45
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL46
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 7, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 7, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL47
	 nop
.LL46:
	ld	[%fp+72], %g1
.LL47:
	ba,pt	%xcc, .LL48
	 nop
.LL45:
	ld	[%fp+68], %g1
.LL48:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	insert_constant_offset__Word_insert_constant_offset_7, .-insert_constant_offset__Word_insert_constant_offset_7
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_8, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_8:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 24
	ble	%icc, .LL50
	 nop
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL50:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL51
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL52
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 8, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 8, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL53
	 nop
.LL52:
	ld	[%fp+72], %g1
.LL53:
	ba,pt	%xcc, .LL54
	 nop
.LL51:
	ld	[%fp+68], %g1
.LL54:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	insert_constant_offset__Word_insert_constant_offset_8, .-insert_constant_offset__Word_insert_constant_offset_8
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_9, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_9:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 23
	ble	%icc, .LL56
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL56:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL57
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL58
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 9, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 9, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL59
	 nop
.LL58:
	ld	[%fp+72], %g1
.LL59:
	ba,pt	%xcc, .LL60
	 nop
.LL57:
	ld	[%fp+68], %g1
.LL60:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	insert_constant_offset__Word_insert_constant_offset_9, .-insert_constant_offset__Word_insert_constant_offset_9
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_10, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_10:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 22
	ble	%icc, .LL62
	 nop
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL62:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL63
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL64
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 10, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 10, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL65
	 nop
.LL64:
	ld	[%fp+72], %g1
.LL65:
	ba,pt	%xcc, .LL66
	 nop
.LL63:
	ld	[%fp+68], %g1
.LL66:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	insert_constant_offset__Word_insert_constant_offset_10, .-insert_constant_offset__Word_insert_constant_offset_10
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_11, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_11:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 21
	ble	%icc, .LL68
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL68:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL69
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL70
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 11, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 11, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL71
	 nop
.LL70:
	ld	[%fp+72], %g1
.LL71:
	ba,pt	%xcc, .LL72
	 nop
.LL69:
	ld	[%fp+68], %g1
.LL72:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	insert_constant_offset__Word_insert_constant_offset_11, .-insert_constant_offset__Word_insert_constant_offset_11
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_12, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_12:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 20
	ble	%icc, .LL74
	 nop
	mov	2529, %o0
	call	_m3_fault, 0
	 nop
.LL74:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL75
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL76
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 12, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 12, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL77
	 nop
.LL76:
	ld	[%fp+72], %g1
.LL77:
	ba,pt	%xcc, .LL78
	 nop
.LL75:
	ld	[%fp+68], %g1
.LL78:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	insert_constant_offset__Word_insert_constant_offset_12, .-insert_constant_offset__Word_insert_constant_offset_12
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_13, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_13:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 19
	ble	%icc, .LL80
	 nop
	mov	2593, %o0
	call	_m3_fault, 0
	 nop
.LL80:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL81
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL82
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 13, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 13, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL83
	 nop
.LL82:
	ld	[%fp+72], %g1
.LL83:
	ba,pt	%xcc, .LL84
	 nop
.LL81:
	ld	[%fp+68], %g1
.LL84:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	insert_constant_offset__Word_insert_constant_offset_13, .-insert_constant_offset__Word_insert_constant_offset_13
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_14, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_14:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 18
	ble	%icc, .LL86
	 nop
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL86:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL87
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL88
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 14, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 14, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL89
	 nop
.LL88:
	ld	[%fp+72], %g1
.LL89:
	ba,pt	%xcc, .LL90
	 nop
.LL87:
	ld	[%fp+68], %g1
.LL90:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	insert_constant_offset__Word_insert_constant_offset_14, .-insert_constant_offset__Word_insert_constant_offset_14
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_15, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_15:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 17
	ble	%icc, .LL92
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL92:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL93
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL94
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 15, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 15, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL95
	 nop
.LL94:
	ld	[%fp+72], %g1
.LL95:
	ba,pt	%xcc, .LL96
	 nop
.LL93:
	ld	[%fp+68], %g1
.LL96:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	insert_constant_offset__Word_insert_constant_offset_15, .-insert_constant_offset__Word_insert_constant_offset_15
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_16, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 16
	ble	%icc, .LL98
	 nop
	mov	2785, %o0
	call	_m3_fault, 0
	 nop
.LL98:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL99
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL100
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 16, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 16, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL101
	 nop
.LL100:
	ld	[%fp+72], %g1
.LL101:
	ba,pt	%xcc, .LL102
	 nop
.LL99:
	ld	[%fp+68], %g1
.LL102:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	insert_constant_offset__Word_insert_constant_offset_16, .-insert_constant_offset__Word_insert_constant_offset_16
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_17, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_17:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 15
	ble	%icc, .LL104
	 nop
	mov	2849, %o0
	call	_m3_fault, 0
	 nop
.LL104:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL105
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL106
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 17, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 17, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL107
	 nop
.LL106:
	ld	[%fp+72], %g1
.LL107:
	ba,pt	%xcc, .LL108
	 nop
.LL105:
	ld	[%fp+68], %g1
.LL108:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	insert_constant_offset__Word_insert_constant_offset_17, .-insert_constant_offset__Word_insert_constant_offset_17
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_18, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_18:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 14
	ble	%icc, .LL110
	 nop
	mov	2913, %o0
	call	_m3_fault, 0
	 nop
.LL110:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL111
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL112
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 18, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 18, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL113
	 nop
.LL112:
	ld	[%fp+72], %g1
.LL113:
	ba,pt	%xcc, .LL114
	 nop
.LL111:
	ld	[%fp+68], %g1
.LL114:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	insert_constant_offset__Word_insert_constant_offset_18, .-insert_constant_offset__Word_insert_constant_offset_18
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_19, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_19:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 13
	ble	%icc, .LL116
	 nop
	mov	2977, %o0
	call	_m3_fault, 0
	 nop
.LL116:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL117
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL118
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 19, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 19, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL119
	 nop
.LL118:
	ld	[%fp+72], %g1
.LL119:
	ba,pt	%xcc, .LL120
	 nop
.LL117:
	ld	[%fp+68], %g1
.LL120:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	insert_constant_offset__Word_insert_constant_offset_19, .-insert_constant_offset__Word_insert_constant_offset_19
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_20, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_20:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 12
	ble	%icc, .LL122
	 nop
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL122:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL123
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL124
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 20, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 20, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL125
	 nop
.LL124:
	ld	[%fp+72], %g1
.LL125:
	ba,pt	%xcc, .LL126
	 nop
.LL123:
	ld	[%fp+68], %g1
.LL126:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	insert_constant_offset__Word_insert_constant_offset_20, .-insert_constant_offset__Word_insert_constant_offset_20
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_21, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_21:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 11
	ble	%icc, .LL128
	 nop
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL128:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL129
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL130
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 21, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 21, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL131
	 nop
.LL130:
	ld	[%fp+72], %g1
.LL131:
	ba,pt	%xcc, .LL132
	 nop
.LL129:
	ld	[%fp+68], %g1
.LL132:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	insert_constant_offset__Word_insert_constant_offset_21, .-insert_constant_offset__Word_insert_constant_offset_21
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_22, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_22:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 10
	ble	%icc, .LL134
	 nop
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL134:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL135
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL136
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 22, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 22, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL137
	 nop
.LL136:
	ld	[%fp+72], %g1
.LL137:
	ba,pt	%xcc, .LL138
	 nop
.LL135:
	ld	[%fp+68], %g1
.LL138:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	insert_constant_offset__Word_insert_constant_offset_22, .-insert_constant_offset__Word_insert_constant_offset_22
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_23, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_23:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 9
	ble	%icc, .LL140
	 nop
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL140:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL141
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL142
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 23, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 23, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL143
	 nop
.LL142:
	ld	[%fp+72], %g1
.LL143:
	ba,pt	%xcc, .LL144
	 nop
.LL141:
	ld	[%fp+68], %g1
.LL144:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	insert_constant_offset__Word_insert_constant_offset_23, .-insert_constant_offset__Word_insert_constant_offset_23
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_24, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_24:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 8
	ble	%icc, .LL146
	 nop
	mov	3297, %o0
	call	_m3_fault, 0
	 nop
.LL146:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL147
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL148
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 24, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 24, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL149
	 nop
.LL148:
	ld	[%fp+72], %g1
.LL149:
	ba,pt	%xcc, .LL150
	 nop
.LL147:
	ld	[%fp+68], %g1
.LL150:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	insert_constant_offset__Word_insert_constant_offset_24, .-insert_constant_offset__Word_insert_constant_offset_24
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_25, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_25:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 7
	ble	%icc, .LL152
	 nop
	mov	3361, %o0
	call	_m3_fault, 0
	 nop
.LL152:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL153
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL154
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 25, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 25, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL155
	 nop
.LL154:
	ld	[%fp+72], %g1
.LL155:
	ba,pt	%xcc, .LL156
	 nop
.LL153:
	ld	[%fp+68], %g1
.LL156:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE25:
	.size	insert_constant_offset__Word_insert_constant_offset_25, .-insert_constant_offset__Word_insert_constant_offset_25
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_26, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_26:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 6
	ble	%icc, .LL158
	 nop
	mov	3425, %o0
	call	_m3_fault, 0
	 nop
.LL158:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL159
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL160
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 26, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 26, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL161
	 nop
.LL160:
	ld	[%fp+72], %g1
.LL161:
	ba,pt	%xcc, .LL162
	 nop
.LL159:
	ld	[%fp+68], %g1
.LL162:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	insert_constant_offset__Word_insert_constant_offset_26, .-insert_constant_offset__Word_insert_constant_offset_26
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_27, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_27:
.LLFB27:
	save	%sp, -96, %sp
.LLCFI27:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 5
	ble	%icc, .LL164
	 nop
	mov	3489, %o0
	call	_m3_fault, 0
	 nop
.LL164:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL165
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL166
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 27, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 27, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL167
	 nop
.LL166:
	ld	[%fp+72], %g1
.LL167:
	ba,pt	%xcc, .LL168
	 nop
.LL165:
	ld	[%fp+68], %g1
.LL168:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE27:
	.size	insert_constant_offset__Word_insert_constant_offset_27, .-insert_constant_offset__Word_insert_constant_offset_27
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_28, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_28:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 4
	ble	%icc, .LL170
	 nop
	mov	3553, %o0
	call	_m3_fault, 0
	 nop
.LL170:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL171
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL172
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 28, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 28, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL173
	 nop
.LL172:
	ld	[%fp+72], %g1
.LL173:
	ba,pt	%xcc, .LL174
	 nop
.LL171:
	ld	[%fp+68], %g1
.LL174:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	insert_constant_offset__Word_insert_constant_offset_28, .-insert_constant_offset__Word_insert_constant_offset_28
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_29, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_29:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 3
	ble	%icc, .LL176
	 nop
	mov	3617, %o0
	call	_m3_fault, 0
	 nop
.LL176:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL177
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL178
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 29, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 29, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL179
	 nop
.LL178:
	ld	[%fp+72], %g1
.LL179:
	ba,pt	%xcc, .LL180
	 nop
.LL177:
	ld	[%fp+68], %g1
.LL180:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	insert_constant_offset__Word_insert_constant_offset_29, .-insert_constant_offset__Word_insert_constant_offset_29
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_30, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_30:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 2
	ble	%icc, .LL182
	 nop
	mov	3681, %o0
	call	_m3_fault, 0
	 nop
.LL182:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL183
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL184
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 30, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 30, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL185
	 nop
.LL184:
	ld	[%fp+72], %g1
.LL185:
	ba,pt	%xcc, .LL186
	 nop
.LL183:
	ld	[%fp+68], %g1
.LL186:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	insert_constant_offset__Word_insert_constant_offset_30, .-insert_constant_offset__Word_insert_constant_offset_30
	.align 4
	.type	insert_constant_offset__Word_insert_constant_offset_31, #function
	.proc	04
insert_constant_offset__Word_insert_constant_offset_31:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 1
	ble	%icc, .LL188
	 nop
	mov	3745, %o0
	call	_m3_fault, 0
	 nop
.LL188:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL189
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	be	%icc, .LL190
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o5
	sll	%o5, %g1, %g1
	xnor	%g0, %g1, %g1
	sll	%g1, 31, %g1
	xnor	%g0, %g1, %o5
	ld	[%fp+68], %g1
	and	%o5, %g1, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	mov	-1, %o4
	sll	%o4, %g1, %g1
	xnor	%g0, %g1, %o4
	ld	[%fp+72], %g1
	and	%o4, %g1, %g1
	sll	%g1, 31, %g1
	or	%o5, %g1, %g1
	ba,pt	%xcc, .LL191
	 nop
.LL190:
	ld	[%fp+72], %g1
.LL191:
	ba,pt	%xcc, .LL192
	 nop
.LL189:
	ld	[%fp+68], %g1
.LL192:
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	insert_constant_offset__Word_insert_constant_offset_31, .-insert_constant_offset__Word_insert_constant_offset_31
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_0, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_0:
.LLFB32:
	save	%sp, -112, %sp
.LLCFI32:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	ble	%icc, .LL194
	 nop
	mov	3809, %o0
	call	_m3_fault, 0
	 nop
.LL194:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL195
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL196
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o2
	srl	%o5, 0, %o5
	or	%o5, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	ba,pt	%xcc, .LL197
	 nop
.LL196:
	ldd	[%fp-16], %o4
.LL197:
	ba,pt	%xcc, .LL198
	 nop
.LL195:
	ldd	[%fp-8], %o4
.LL198:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE32:
	.size	insert_constant_offset__Long_insert_constant_offset_0, .-insert_constant_offset__Long_insert_constant_offset_0
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_1, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_1:
.LLFB33:
	save	%sp, -112, %sp
.LLCFI33:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 63
	ble	%icc, .LL200
	 nop
	mov	3873, %o0
	call	_m3_fault, 0
	 nop
.LL200:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL201
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL202
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	addcc	%o5, %o5, %o5
	addx	%o4, %o4, %o4
	xnor	%o4, 0, %o2
	xnor	%o5, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	addcc	%o5, %o5, %o5
	addx	%o4, %o4, %o4
	or	%o2, %o4, %o4
	or	%o3, %o5, %o5
	ba,pt	%xcc, .LL203
	 nop
.LL202:
	ldd	[%fp-16], %o4
.LL203:
	ba,pt	%xcc, .LL204
	 nop
.LL201:
	ldd	[%fp-8], %o4
.LL204:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE33:
	.size	insert_constant_offset__Long_insert_constant_offset_1, .-insert_constant_offset__Long_insert_constant_offset_1
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_2, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_2:
.LLFB34:
	save	%sp, -112, %sp
.LLCFI34:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 62
	ble	%icc, .LL206
	 nop
	mov	3937, %o0
	call	_m3_fault, 0
	 nop
.LL206:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL207
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL208
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 30, %g1
	sll	%o4, 2, %l2
	or	%g1, %l2, %l2
	sll	%o5, 2, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 30, %g1
	sll	%o4, 2, %l0
	or	%g1, %l0, %l0
	sll	%o5, 2, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL209
	 nop
.LL208:
	ldd	[%fp-16], %o4
.LL209:
	ba,pt	%xcc, .LL210
	 nop
.LL207:
	ldd	[%fp-8], %o4
.LL210:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE34:
	.size	insert_constant_offset__Long_insert_constant_offset_2, .-insert_constant_offset__Long_insert_constant_offset_2
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_3, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_3:
.LLFB35:
	save	%sp, -112, %sp
.LLCFI35:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 61
	ble	%icc, .LL212
	 nop
	mov	4001, %o0
	call	_m3_fault, 0
	 nop
.LL212:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL213
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL214
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 29, %g1
	sll	%o4, 3, %l2
	or	%g1, %l2, %l2
	sll	%o5, 3, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 29, %g1
	sll	%o4, 3, %l0
	or	%g1, %l0, %l0
	sll	%o5, 3, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL215
	 nop
.LL214:
	ldd	[%fp-16], %o4
.LL215:
	ba,pt	%xcc, .LL216
	 nop
.LL213:
	ldd	[%fp-8], %o4
.LL216:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE35:
	.size	insert_constant_offset__Long_insert_constant_offset_3, .-insert_constant_offset__Long_insert_constant_offset_3
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_4, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_4:
.LLFB36:
	save	%sp, -112, %sp
.LLCFI36:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 60
	ble	%icc, .LL218
	 nop
	mov	4065, %o0
	call	_m3_fault, 0
	 nop
.LL218:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL219
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL220
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 28, %g1
	sll	%o4, 4, %l2
	or	%g1, %l2, %l2
	sll	%o5, 4, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 28, %g1
	sll	%o4, 4, %l0
	or	%g1, %l0, %l0
	sll	%o5, 4, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL221
	 nop
.LL220:
	ldd	[%fp-16], %o4
.LL221:
	ba,pt	%xcc, .LL222
	 nop
.LL219:
	ldd	[%fp-8], %o4
.LL222:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	insert_constant_offset__Long_insert_constant_offset_4, .-insert_constant_offset__Long_insert_constant_offset_4
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_5, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_5:
.LLFB37:
	save	%sp, -112, %sp
.LLCFI37:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 59
	ble	%icc, .LL224
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL224:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL225
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL226
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 27, %g1
	sll	%o4, 5, %l2
	or	%g1, %l2, %l2
	sll	%o5, 5, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 27, %g1
	sll	%o4, 5, %l0
	or	%g1, %l0, %l0
	sll	%o5, 5, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL227
	 nop
.LL226:
	ldd	[%fp-16], %o4
.LL227:
	ba,pt	%xcc, .LL228
	 nop
.LL225:
	ldd	[%fp-8], %o4
.LL228:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	insert_constant_offset__Long_insert_constant_offset_5, .-insert_constant_offset__Long_insert_constant_offset_5
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_6, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_6:
.LLFB38:
	save	%sp, -112, %sp
.LLCFI38:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 58
	ble	%icc, .LL230
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL230:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL231
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL232
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 26, %g1
	sll	%o4, 6, %l2
	or	%g1, %l2, %l2
	sll	%o5, 6, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 26, %g1
	sll	%o4, 6, %l0
	or	%g1, %l0, %l0
	sll	%o5, 6, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL233
	 nop
.LL232:
	ldd	[%fp-16], %o4
.LL233:
	ba,pt	%xcc, .LL234
	 nop
.LL231:
	ldd	[%fp-8], %o4
.LL234:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	insert_constant_offset__Long_insert_constant_offset_6, .-insert_constant_offset__Long_insert_constant_offset_6
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_7, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_7:
.LLFB39:
	save	%sp, -112, %sp
.LLCFI39:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 57
	ble	%icc, .LL236
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL236:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL237
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL238
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 25, %g1
	sll	%o4, 7, %l2
	or	%g1, %l2, %l2
	sll	%o5, 7, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 25, %g1
	sll	%o4, 7, %l0
	or	%g1, %l0, %l0
	sll	%o5, 7, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL239
	 nop
.LL238:
	ldd	[%fp-16], %o4
.LL239:
	ba,pt	%xcc, .LL240
	 nop
.LL237:
	ldd	[%fp-8], %o4
.LL240:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	insert_constant_offset__Long_insert_constant_offset_7, .-insert_constant_offset__Long_insert_constant_offset_7
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_8, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_8:
.LLFB40:
	save	%sp, -112, %sp
.LLCFI40:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 56
	ble	%icc, .LL242
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL242:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL243
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL244
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 24, %g1
	sll	%o4, 8, %l2
	or	%g1, %l2, %l2
	sll	%o5, 8, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 24, %g1
	sll	%o4, 8, %l0
	or	%g1, %l0, %l0
	sll	%o5, 8, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL245
	 nop
.LL244:
	ldd	[%fp-16], %o4
.LL245:
	ba,pt	%xcc, .LL246
	 nop
.LL243:
	ldd	[%fp-8], %o4
.LL246:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE40:
	.size	insert_constant_offset__Long_insert_constant_offset_8, .-insert_constant_offset__Long_insert_constant_offset_8
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_9, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_9:
.LLFB41:
	save	%sp, -112, %sp
.LLCFI41:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 55
	ble	%icc, .LL248
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL248:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL249
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL250
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 23, %g1
	sll	%o4, 9, %l2
	or	%g1, %l2, %l2
	sll	%o5, 9, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 23, %g1
	sll	%o4, 9, %l0
	or	%g1, %l0, %l0
	sll	%o5, 9, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL251
	 nop
.LL250:
	ldd	[%fp-16], %o4
.LL251:
	ba,pt	%xcc, .LL252
	 nop
.LL249:
	ldd	[%fp-8], %o4
.LL252:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE41:
	.size	insert_constant_offset__Long_insert_constant_offset_9, .-insert_constant_offset__Long_insert_constant_offset_9
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_10, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_10:
.LLFB42:
	save	%sp, -112, %sp
.LLCFI42:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 54
	ble	%icc, .LL254
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL254:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL255
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL256
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 22, %g1
	sll	%o4, 10, %l2
	or	%g1, %l2, %l2
	sll	%o5, 10, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 22, %g1
	sll	%o4, 10, %l0
	or	%g1, %l0, %l0
	sll	%o5, 10, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL257
	 nop
.LL256:
	ldd	[%fp-16], %o4
.LL257:
	ba,pt	%xcc, .LL258
	 nop
.LL255:
	ldd	[%fp-8], %o4
.LL258:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE42:
	.size	insert_constant_offset__Long_insert_constant_offset_10, .-insert_constant_offset__Long_insert_constant_offset_10
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_11, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_11:
.LLFB43:
	save	%sp, -112, %sp
.LLCFI43:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 53
	ble	%icc, .LL260
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL260:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL261
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL262
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 21, %g1
	sll	%o4, 11, %l2
	or	%g1, %l2, %l2
	sll	%o5, 11, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 21, %g1
	sll	%o4, 11, %l0
	or	%g1, %l0, %l0
	sll	%o5, 11, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL263
	 nop
.LL262:
	ldd	[%fp-16], %o4
.LL263:
	ba,pt	%xcc, .LL264
	 nop
.LL261:
	ldd	[%fp-8], %o4
.LL264:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE43:
	.size	insert_constant_offset__Long_insert_constant_offset_11, .-insert_constant_offset__Long_insert_constant_offset_11
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_12, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_12:
.LLFB44:
	save	%sp, -112, %sp
.LLCFI44:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 52
	ble	%icc, .LL266
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL266:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL267
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL268
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 20, %g1
	sll	%o4, 12, %l2
	or	%g1, %l2, %l2
	sll	%o5, 12, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 20, %g1
	sll	%o4, 12, %l0
	or	%g1, %l0, %l0
	sll	%o5, 12, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL269
	 nop
.LL268:
	ldd	[%fp-16], %o4
.LL269:
	ba,pt	%xcc, .LL270
	 nop
.LL267:
	ldd	[%fp-8], %o4
.LL270:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE44:
	.size	insert_constant_offset__Long_insert_constant_offset_12, .-insert_constant_offset__Long_insert_constant_offset_12
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_13, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_13:
.LLFB45:
	save	%sp, -112, %sp
.LLCFI45:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 51
	ble	%icc, .LL272
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL272:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL273
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL274
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 19, %g1
	sll	%o4, 13, %l2
	or	%g1, %l2, %l2
	sll	%o5, 13, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 19, %g1
	sll	%o4, 13, %l0
	or	%g1, %l0, %l0
	sll	%o5, 13, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL275
	 nop
.LL274:
	ldd	[%fp-16], %o4
.LL275:
	ba,pt	%xcc, .LL276
	 nop
.LL273:
	ldd	[%fp-8], %o4
.LL276:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE45:
	.size	insert_constant_offset__Long_insert_constant_offset_13, .-insert_constant_offset__Long_insert_constant_offset_13
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_14, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_14:
.LLFB46:
	save	%sp, -112, %sp
.LLCFI46:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 50
	ble	%icc, .LL278
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL278:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL279
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL280
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 18, %g1
	sll	%o4, 14, %l2
	or	%g1, %l2, %l2
	sll	%o5, 14, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 18, %g1
	sll	%o4, 14, %l0
	or	%g1, %l0, %l0
	sll	%o5, 14, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL281
	 nop
.LL280:
	ldd	[%fp-16], %o4
.LL281:
	ba,pt	%xcc, .LL282
	 nop
.LL279:
	ldd	[%fp-8], %o4
.LL282:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE46:
	.size	insert_constant_offset__Long_insert_constant_offset_14, .-insert_constant_offset__Long_insert_constant_offset_14
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_15, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_15:
.LLFB47:
	save	%sp, -112, %sp
.LLCFI47:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 49
	ble	%icc, .LL284
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL284:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL285
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL286
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 17, %g1
	sll	%o4, 15, %l2
	or	%g1, %l2, %l2
	sll	%o5, 15, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 17, %g1
	sll	%o4, 15, %l0
	or	%g1, %l0, %l0
	sll	%o5, 15, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL287
	 nop
.LL286:
	ldd	[%fp-16], %o4
.LL287:
	ba,pt	%xcc, .LL288
	 nop
.LL285:
	ldd	[%fp-8], %o4
.LL288:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE47:
	.size	insert_constant_offset__Long_insert_constant_offset_15, .-insert_constant_offset__Long_insert_constant_offset_15
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_16, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_16:
.LLFB48:
	save	%sp, -112, %sp
.LLCFI48:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 48
	ble	%icc, .LL290
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL290:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL291
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL292
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 16, %g1
	sll	%o4, 16, %l2
	or	%g1, %l2, %l2
	sll	%o5, 16, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 16, %g1
	sll	%o4, 16, %l0
	or	%g1, %l0, %l0
	sll	%o5, 16, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL293
	 nop
.LL292:
	ldd	[%fp-16], %o4
.LL293:
	ba,pt	%xcc, .LL294
	 nop
.LL291:
	ldd	[%fp-8], %o4
.LL294:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	insert_constant_offset__Long_insert_constant_offset_16, .-insert_constant_offset__Long_insert_constant_offset_16
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_17, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_17:
.LLFB49:
	save	%sp, -112, %sp
.LLCFI49:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 47
	ble	%icc, .LL296
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL296:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL297
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL298
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 15, %g1
	sll	%o4, 17, %l2
	or	%g1, %l2, %l2
	sll	%o5, 17, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 15, %g1
	sll	%o4, 17, %l0
	or	%g1, %l0, %l0
	sll	%o5, 17, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL299
	 nop
.LL298:
	ldd	[%fp-16], %o4
.LL299:
	ba,pt	%xcc, .LL300
	 nop
.LL297:
	ldd	[%fp-8], %o4
.LL300:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	insert_constant_offset__Long_insert_constant_offset_17, .-insert_constant_offset__Long_insert_constant_offset_17
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_18, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_18:
.LLFB50:
	save	%sp, -112, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 46
	ble	%icc, .LL302
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL302:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL303
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL304
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 14, %g1
	sll	%o4, 18, %l2
	or	%g1, %l2, %l2
	sll	%o5, 18, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 14, %g1
	sll	%o4, 18, %l0
	or	%g1, %l0, %l0
	sll	%o5, 18, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL305
	 nop
.LL304:
	ldd	[%fp-16], %o4
.LL305:
	ba,pt	%xcc, .LL306
	 nop
.LL303:
	ldd	[%fp-8], %o4
.LL306:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	insert_constant_offset__Long_insert_constant_offset_18, .-insert_constant_offset__Long_insert_constant_offset_18
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_19, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_19:
.LLFB51:
	save	%sp, -112, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 45
	ble	%icc, .LL308
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL308:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL309
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL310
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 13, %g1
	sll	%o4, 19, %l2
	or	%g1, %l2, %l2
	sll	%o5, 19, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 13, %g1
	sll	%o4, 19, %l0
	or	%g1, %l0, %l0
	sll	%o5, 19, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL311
	 nop
.LL310:
	ldd	[%fp-16], %o4
.LL311:
	ba,pt	%xcc, .LL312
	 nop
.LL309:
	ldd	[%fp-8], %o4
.LL312:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	insert_constant_offset__Long_insert_constant_offset_19, .-insert_constant_offset__Long_insert_constant_offset_19
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_20, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_20:
.LLFB52:
	save	%sp, -112, %sp
.LLCFI52:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 44
	ble	%icc, .LL314
	 nop
	sethi	%hi(4096), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL314:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL315
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL316
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 12, %g1
	sll	%o4, 20, %l2
	or	%g1, %l2, %l2
	sll	%o5, 20, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 12, %g1
	sll	%o4, 20, %l0
	or	%g1, %l0, %l0
	sll	%o5, 20, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL317
	 nop
.LL316:
	ldd	[%fp-16], %o4
.LL317:
	ba,pt	%xcc, .LL318
	 nop
.LL315:
	ldd	[%fp-8], %o4
.LL318:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	insert_constant_offset__Long_insert_constant_offset_20, .-insert_constant_offset__Long_insert_constant_offset_20
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_21, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_21:
.LLFB53:
	save	%sp, -112, %sp
.LLCFI53:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 43
	ble	%icc, .LL320
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL320:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL321
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL322
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 11, %g1
	sll	%o4, 21, %l2
	or	%g1, %l2, %l2
	sll	%o5, 21, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 11, %g1
	sll	%o4, 21, %l0
	or	%g1, %l0, %l0
	sll	%o5, 21, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL323
	 nop
.LL322:
	ldd	[%fp-16], %o4
.LL323:
	ba,pt	%xcc, .LL324
	 nop
.LL321:
	ldd	[%fp-8], %o4
.LL324:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	insert_constant_offset__Long_insert_constant_offset_21, .-insert_constant_offset__Long_insert_constant_offset_21
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_22, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_22:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 42
	ble	%icc, .LL326
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL326:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL327
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL328
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 10, %g1
	sll	%o4, 22, %l2
	or	%g1, %l2, %l2
	sll	%o5, 22, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 10, %g1
	sll	%o4, 22, %l0
	or	%g1, %l0, %l0
	sll	%o5, 22, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL329
	 nop
.LL328:
	ldd	[%fp-16], %o4
.LL329:
	ba,pt	%xcc, .LL330
	 nop
.LL327:
	ldd	[%fp-8], %o4
.LL330:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	insert_constant_offset__Long_insert_constant_offset_22, .-insert_constant_offset__Long_insert_constant_offset_22
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_23, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_23:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 41
	ble	%icc, .LL332
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL332:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL333
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL334
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 9, %g1
	sll	%o4, 23, %l2
	or	%g1, %l2, %l2
	sll	%o5, 23, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 9, %g1
	sll	%o4, 23, %l0
	or	%g1, %l0, %l0
	sll	%o5, 23, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL335
	 nop
.LL334:
	ldd	[%fp-16], %o4
.LL335:
	ba,pt	%xcc, .LL336
	 nop
.LL333:
	ldd	[%fp-8], %o4
.LL336:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	insert_constant_offset__Long_insert_constant_offset_23, .-insert_constant_offset__Long_insert_constant_offset_23
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_24, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_24:
.LLFB56:
	save	%sp, -112, %sp
.LLCFI56:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 40
	ble	%icc, .LL338
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL338:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL339
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL340
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 8, %g1
	sll	%o4, 24, %l2
	or	%g1, %l2, %l2
	sll	%o5, 24, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 8, %g1
	sll	%o4, 24, %l0
	or	%g1, %l0, %l0
	sll	%o5, 24, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL341
	 nop
.LL340:
	ldd	[%fp-16], %o4
.LL341:
	ba,pt	%xcc, .LL342
	 nop
.LL339:
	ldd	[%fp-8], %o4
.LL342:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	insert_constant_offset__Long_insert_constant_offset_24, .-insert_constant_offset__Long_insert_constant_offset_24
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_25, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_25:
.LLFB57:
	save	%sp, -112, %sp
.LLCFI57:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 39
	ble	%icc, .LL344
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL344:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL345
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL346
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 7, %g1
	sll	%o4, 25, %l2
	or	%g1, %l2, %l2
	sll	%o5, 25, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 7, %g1
	sll	%o4, 25, %l0
	or	%g1, %l0, %l0
	sll	%o5, 25, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL347
	 nop
.LL346:
	ldd	[%fp-16], %o4
.LL347:
	ba,pt	%xcc, .LL348
	 nop
.LL345:
	ldd	[%fp-8], %o4
.LL348:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	insert_constant_offset__Long_insert_constant_offset_25, .-insert_constant_offset__Long_insert_constant_offset_25
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_26, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_26:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 38
	ble	%icc, .LL350
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL350:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL351
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL352
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 6, %g1
	sll	%o4, 26, %l2
	or	%g1, %l2, %l2
	sll	%o5, 26, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 6, %g1
	sll	%o4, 26, %l0
	or	%g1, %l0, %l0
	sll	%o5, 26, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL353
	 nop
.LL352:
	ldd	[%fp-16], %o4
.LL353:
	ba,pt	%xcc, .LL354
	 nop
.LL351:
	ldd	[%fp-8], %o4
.LL354:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	insert_constant_offset__Long_insert_constant_offset_26, .-insert_constant_offset__Long_insert_constant_offset_26
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_27, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_27:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 37
	ble	%icc, .LL356
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL356:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL357
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL358
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 5, %g1
	sll	%o4, 27, %l2
	or	%g1, %l2, %l2
	sll	%o5, 27, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 5, %g1
	sll	%o4, 27, %l0
	or	%g1, %l0, %l0
	sll	%o5, 27, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL359
	 nop
.LL358:
	ldd	[%fp-16], %o4
.LL359:
	ba,pt	%xcc, .LL360
	 nop
.LL357:
	ldd	[%fp-8], %o4
.LL360:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	insert_constant_offset__Long_insert_constant_offset_27, .-insert_constant_offset__Long_insert_constant_offset_27
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_28, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_28:
.LLFB60:
	save	%sp, -112, %sp
.LLCFI60:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 36
	ble	%icc, .LL362
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL362:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL363
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL364
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 4, %g1
	sll	%o4, 28, %l2
	or	%g1, %l2, %l2
	sll	%o5, 28, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 4, %g1
	sll	%o4, 28, %l0
	or	%g1, %l0, %l0
	sll	%o5, 28, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL365
	 nop
.LL364:
	ldd	[%fp-16], %o4
.LL365:
	ba,pt	%xcc, .LL366
	 nop
.LL363:
	ldd	[%fp-8], %o4
.LL366:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	insert_constant_offset__Long_insert_constant_offset_28, .-insert_constant_offset__Long_insert_constant_offset_28
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_29, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_29:
.LLFB61:
	save	%sp, -112, %sp
.LLCFI61:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 35
	ble	%icc, .LL368
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL368:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL369
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL370
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 3, %g1
	sll	%o4, 29, %l2
	or	%g1, %l2, %l2
	sll	%o5, 29, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 3, %g1
	sll	%o4, 29, %l0
	or	%g1, %l0, %l0
	sll	%o5, 29, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL371
	 nop
.LL370:
	ldd	[%fp-16], %o4
.LL371:
	ba,pt	%xcc, .LL372
	 nop
.LL369:
	ldd	[%fp-8], %o4
.LL372:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	insert_constant_offset__Long_insert_constant_offset_29, .-insert_constant_offset__Long_insert_constant_offset_29
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_30, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_30:
.LLFB62:
	save	%sp, -112, %sp
.LLCFI62:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 34
	ble	%icc, .LL374
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL374:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL375
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL376
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 2, %g1
	sll	%o4, 30, %l2
	or	%g1, %l2, %l2
	sll	%o5, 30, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 2, %g1
	sll	%o4, 30, %l0
	or	%g1, %l0, %l0
	sll	%o5, 30, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL377
	 nop
.LL376:
	ldd	[%fp-16], %o4
.LL377:
	ba,pt	%xcc, .LL378
	 nop
.LL375:
	ldd	[%fp-8], %o4
.LL378:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	insert_constant_offset__Long_insert_constant_offset_30, .-insert_constant_offset__Long_insert_constant_offset_30
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_31, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_31:
.LLFB63:
	save	%sp, -112, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 33
	ble	%icc, .LL380
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL380:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL381
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL382
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	srl	%o5, 1, %g1
	sll	%o4, 31, %l2
	or	%g1, %l2, %l2
	sll	%o5, 31, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o2
	and	%o3, %o5, %o3
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o0
	xnor	%o5, 0, %o1
	ldd	[%fp-16], %o4
	and	%o0, %o4, %o4
	and	%o1, %o5, %o5
	srl	%o5, 1, %g1
	sll	%o4, 31, %l0
	or	%g1, %l0, %l0
	sll	%o5, 31, %l1
	or	%o2, %l0, %o4
	or	%o3, %l1, %o5
	ba,pt	%xcc, .LL383
	 nop
.LL382:
	ldd	[%fp-16], %o4
.LL383:
	ba,pt	%xcc, .LL384
	 nop
.LL381:
	ldd	[%fp-8], %o4
.LL384:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	insert_constant_offset__Long_insert_constant_offset_31, .-insert_constant_offset__Long_insert_constant_offset_31
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_32, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_32:
.LLFB64:
	save	%sp, -112, %sp
.LLCFI64:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 32
	ble	%icc, .LL386
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL386:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL387
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL388
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 0, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 0, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL389
	 nop
.LL388:
	ldd	[%fp-16], %o4
.LL389:
	ba,pt	%xcc, .LL390
	 nop
.LL387:
	ldd	[%fp-8], %o4
.LL390:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	insert_constant_offset__Long_insert_constant_offset_32, .-insert_constant_offset__Long_insert_constant_offset_32
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_33, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_33:
.LLFB65:
	save	%sp, -112, %sp
.LLCFI65:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 31
	ble	%icc, .LL392
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL392:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL393
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL394
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 1, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 1, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL395
	 nop
.LL394:
	ldd	[%fp-16], %o4
.LL395:
	ba,pt	%xcc, .LL396
	 nop
.LL393:
	ldd	[%fp-8], %o4
.LL396:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	insert_constant_offset__Long_insert_constant_offset_33, .-insert_constant_offset__Long_insert_constant_offset_33
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_34, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_34:
.LLFB66:
	save	%sp, -112, %sp
.LLCFI66:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 30
	ble	%icc, .LL398
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL398:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL399
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL400
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 2, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 2, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL401
	 nop
.LL400:
	ldd	[%fp-16], %o4
.LL401:
	ba,pt	%xcc, .LL402
	 nop
.LL399:
	ldd	[%fp-8], %o4
.LL402:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	insert_constant_offset__Long_insert_constant_offset_34, .-insert_constant_offset__Long_insert_constant_offset_34
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_35, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_35:
.LLFB67:
	save	%sp, -112, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 29
	ble	%icc, .LL404
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL404:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL405
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL406
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 3, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 3, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL407
	 nop
.LL406:
	ldd	[%fp-16], %o4
.LL407:
	ba,pt	%xcc, .LL408
	 nop
.LL405:
	ldd	[%fp-8], %o4
.LL408:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	insert_constant_offset__Long_insert_constant_offset_35, .-insert_constant_offset__Long_insert_constant_offset_35
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_36, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_36:
.LLFB68:
	save	%sp, -112, %sp
.LLCFI68:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 28
	ble	%icc, .LL410
	 nop
	sethi	%hi(5120), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL410:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL411
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL412
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 4, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 4, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL413
	 nop
.LL412:
	ldd	[%fp-16], %o4
.LL413:
	ba,pt	%xcc, .LL414
	 nop
.LL411:
	ldd	[%fp-8], %o4
.LL414:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	insert_constant_offset__Long_insert_constant_offset_36, .-insert_constant_offset__Long_insert_constant_offset_36
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_37, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_37:
.LLFB69:
	save	%sp, -112, %sp
.LLCFI69:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 27
	ble	%icc, .LL416
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL416:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL417
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL418
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 5, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 5, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL419
	 nop
.LL418:
	ldd	[%fp-16], %o4
.LL419:
	ba,pt	%xcc, .LL420
	 nop
.LL417:
	ldd	[%fp-8], %o4
.LL420:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	insert_constant_offset__Long_insert_constant_offset_37, .-insert_constant_offset__Long_insert_constant_offset_37
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_38, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_38:
.LLFB70:
	save	%sp, -112, %sp
.LLCFI70:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 26
	ble	%icc, .LL422
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL422:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL423
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL424
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 6, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 6, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL425
	 nop
.LL424:
	ldd	[%fp-16], %o4
.LL425:
	ba,pt	%xcc, .LL426
	 nop
.LL423:
	ldd	[%fp-8], %o4
.LL426:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	insert_constant_offset__Long_insert_constant_offset_38, .-insert_constant_offset__Long_insert_constant_offset_38
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_39, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_39:
.LLFB71:
	save	%sp, -112, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 25
	ble	%icc, .LL428
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL428:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL429
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL430
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 7, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 7, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL431
	 nop
.LL430:
	ldd	[%fp-16], %o4
.LL431:
	ba,pt	%xcc, .LL432
	 nop
.LL429:
	ldd	[%fp-8], %o4
.LL432:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	insert_constant_offset__Long_insert_constant_offset_39, .-insert_constant_offset__Long_insert_constant_offset_39
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_40, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_40:
.LLFB72:
	save	%sp, -112, %sp
.LLCFI72:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 24
	ble	%icc, .LL434
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL434:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL435
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL436
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 8, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 8, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL437
	 nop
.LL436:
	ldd	[%fp-16], %o4
.LL437:
	ba,pt	%xcc, .LL438
	 nop
.LL435:
	ldd	[%fp-8], %o4
.LL438:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	insert_constant_offset__Long_insert_constant_offset_40, .-insert_constant_offset__Long_insert_constant_offset_40
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_41, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_41:
.LLFB73:
	save	%sp, -112, %sp
.LLCFI73:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 23
	ble	%icc, .LL440
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL440:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL441
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL442
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 9, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 9, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL443
	 nop
.LL442:
	ldd	[%fp-16], %o4
.LL443:
	ba,pt	%xcc, .LL444
	 nop
.LL441:
	ldd	[%fp-8], %o4
.LL444:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	insert_constant_offset__Long_insert_constant_offset_41, .-insert_constant_offset__Long_insert_constant_offset_41
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_42, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_42:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 22
	ble	%icc, .LL446
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL446:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL447
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL448
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 10, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 10, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL449
	 nop
.LL448:
	ldd	[%fp-16], %o4
.LL449:
	ba,pt	%xcc, .LL450
	 nop
.LL447:
	ldd	[%fp-8], %o4
.LL450:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	insert_constant_offset__Long_insert_constant_offset_42, .-insert_constant_offset__Long_insert_constant_offset_42
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_43, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_43:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 21
	ble	%icc, .LL452
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL452:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL453
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL454
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 11, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 11, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL455
	 nop
.LL454:
	ldd	[%fp-16], %o4
.LL455:
	ba,pt	%xcc, .LL456
	 nop
.LL453:
	ldd	[%fp-8], %o4
.LL456:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	insert_constant_offset__Long_insert_constant_offset_43, .-insert_constant_offset__Long_insert_constant_offset_43
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_44, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_44:
.LLFB76:
	save	%sp, -112, %sp
.LLCFI76:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 20
	ble	%icc, .LL458
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL458:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL459
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL460
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 12, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 12, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL461
	 nop
.LL460:
	ldd	[%fp-16], %o4
.LL461:
	ba,pt	%xcc, .LL462
	 nop
.LL459:
	ldd	[%fp-8], %o4
.LL462:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	insert_constant_offset__Long_insert_constant_offset_44, .-insert_constant_offset__Long_insert_constant_offset_44
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_45, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_45:
.LLFB77:
	save	%sp, -112, %sp
.LLCFI77:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 19
	ble	%icc, .LL464
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL464:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL465
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL466
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 13, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 13, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL467
	 nop
.LL466:
	ldd	[%fp-16], %o4
.LL467:
	ba,pt	%xcc, .LL468
	 nop
.LL465:
	ldd	[%fp-8], %o4
.LL468:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	insert_constant_offset__Long_insert_constant_offset_45, .-insert_constant_offset__Long_insert_constant_offset_45
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_46, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_46:
.LLFB78:
	save	%sp, -112, %sp
.LLCFI78:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 18
	ble	%icc, .LL470
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL470:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL471
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL472
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 14, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 14, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL473
	 nop
.LL472:
	ldd	[%fp-16], %o4
.LL473:
	ba,pt	%xcc, .LL474
	 nop
.LL471:
	ldd	[%fp-8], %o4
.LL474:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	insert_constant_offset__Long_insert_constant_offset_46, .-insert_constant_offset__Long_insert_constant_offset_46
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_47, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_47:
.LLFB79:
	save	%sp, -112, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 17
	ble	%icc, .LL476
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL476:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL477
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL478
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 15, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 15, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL479
	 nop
.LL478:
	ldd	[%fp-16], %o4
.LL479:
	ba,pt	%xcc, .LL480
	 nop
.LL477:
	ldd	[%fp-8], %o4
.LL480:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	insert_constant_offset__Long_insert_constant_offset_47, .-insert_constant_offset__Long_insert_constant_offset_47
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_48, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_48:
.LLFB80:
	save	%sp, -112, %sp
.LLCFI80:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 16
	ble	%icc, .LL482
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL482:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL483
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL484
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 16, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 16, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL485
	 nop
.LL484:
	ldd	[%fp-16], %o4
.LL485:
	ba,pt	%xcc, .LL486
	 nop
.LL483:
	ldd	[%fp-8], %o4
.LL486:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	insert_constant_offset__Long_insert_constant_offset_48, .-insert_constant_offset__Long_insert_constant_offset_48
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_49, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_49:
.LLFB81:
	save	%sp, -112, %sp
.LLCFI81:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 15
	ble	%icc, .LL488
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL488:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL489
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL490
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 17, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 17, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL491
	 nop
.LL490:
	ldd	[%fp-16], %o4
.LL491:
	ba,pt	%xcc, .LL492
	 nop
.LL489:
	ldd	[%fp-8], %o4
.LL492:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	insert_constant_offset__Long_insert_constant_offset_49, .-insert_constant_offset__Long_insert_constant_offset_49
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_50, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_50:
.LLFB82:
	save	%sp, -112, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 14
	ble	%icc, .LL494
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL494:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL495
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL496
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 18, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 18, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL497
	 nop
.LL496:
	ldd	[%fp-16], %o4
.LL497:
	ba,pt	%xcc, .LL498
	 nop
.LL495:
	ldd	[%fp-8], %o4
.LL498:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	insert_constant_offset__Long_insert_constant_offset_50, .-insert_constant_offset__Long_insert_constant_offset_50
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_51, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_51:
.LLFB83:
	save	%sp, -112, %sp
.LLCFI83:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 13
	ble	%icc, .LL500
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL500:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL501
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL502
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 19, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 19, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL503
	 nop
.LL502:
	ldd	[%fp-16], %o4
.LL503:
	ba,pt	%xcc, .LL504
	 nop
.LL501:
	ldd	[%fp-8], %o4
.LL504:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	insert_constant_offset__Long_insert_constant_offset_51, .-insert_constant_offset__Long_insert_constant_offset_51
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_52, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_52:
.LLFB84:
	save	%sp, -112, %sp
.LLCFI84:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 12
	ble	%icc, .LL506
	 nop
	sethi	%hi(6144), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL506:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL507
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL508
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 20, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 20, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL509
	 nop
.LL508:
	ldd	[%fp-16], %o4
.LL509:
	ba,pt	%xcc, .LL510
	 nop
.LL507:
	ldd	[%fp-8], %o4
.LL510:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	insert_constant_offset__Long_insert_constant_offset_52, .-insert_constant_offset__Long_insert_constant_offset_52
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_53, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_53:
.LLFB85:
	save	%sp, -112, %sp
.LLCFI85:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 11
	ble	%icc, .LL512
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL512:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL513
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL514
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 21, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 21, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL515
	 nop
.LL514:
	ldd	[%fp-16], %o4
.LL515:
	ba,pt	%xcc, .LL516
	 nop
.LL513:
	ldd	[%fp-8], %o4
.LL516:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	insert_constant_offset__Long_insert_constant_offset_53, .-insert_constant_offset__Long_insert_constant_offset_53
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_54, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_54:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 10
	ble	%icc, .LL518
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL518:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL519
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL520
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 22, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 22, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL521
	 nop
.LL520:
	ldd	[%fp-16], %o4
.LL521:
	ba,pt	%xcc, .LL522
	 nop
.LL519:
	ldd	[%fp-8], %o4
.LL522:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	insert_constant_offset__Long_insert_constant_offset_54, .-insert_constant_offset__Long_insert_constant_offset_54
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_55, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_55:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 9
	ble	%icc, .LL524
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL524:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL525
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL526
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 23, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 23, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL527
	 nop
.LL526:
	ldd	[%fp-16], %o4
.LL527:
	ba,pt	%xcc, .LL528
	 nop
.LL525:
	ldd	[%fp-8], %o4
.LL528:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	insert_constant_offset__Long_insert_constant_offset_55, .-insert_constant_offset__Long_insert_constant_offset_55
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_56, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_56:
.LLFB88:
	save	%sp, -112, %sp
.LLCFI88:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 8
	ble	%icc, .LL530
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL530:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL531
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL532
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 24, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 24, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL533
	 nop
.LL532:
	ldd	[%fp-16], %o4
.LL533:
	ba,pt	%xcc, .LL534
	 nop
.LL531:
	ldd	[%fp-8], %o4
.LL534:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	insert_constant_offset__Long_insert_constant_offset_56, .-insert_constant_offset__Long_insert_constant_offset_56
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_57, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_57:
.LLFB89:
	save	%sp, -112, %sp
.LLCFI89:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 7
	ble	%icc, .LL536
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL536:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL537
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL538
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 25, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 25, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL539
	 nop
.LL538:
	ldd	[%fp-16], %o4
.LL539:
	ba,pt	%xcc, .LL540
	 nop
.LL537:
	ldd	[%fp-8], %o4
.LL540:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	insert_constant_offset__Long_insert_constant_offset_57, .-insert_constant_offset__Long_insert_constant_offset_57
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_58, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_58:
.LLFB90:
	save	%sp, -112, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 6
	ble	%icc, .LL542
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL542:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL543
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL544
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 26, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 26, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL545
	 nop
.LL544:
	ldd	[%fp-16], %o4
.LL545:
	ba,pt	%xcc, .LL546
	 nop
.LL543:
	ldd	[%fp-8], %o4
.LL546:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	insert_constant_offset__Long_insert_constant_offset_58, .-insert_constant_offset__Long_insert_constant_offset_58
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_59, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_59:
.LLFB91:
	save	%sp, -112, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 5
	ble	%icc, .LL548
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL548:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL549
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL550
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 27, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 27, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL551
	 nop
.LL550:
	ldd	[%fp-16], %o4
.LL551:
	ba,pt	%xcc, .LL552
	 nop
.LL549:
	ldd	[%fp-8], %o4
.LL552:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	insert_constant_offset__Long_insert_constant_offset_59, .-insert_constant_offset__Long_insert_constant_offset_59
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_60, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_60:
.LLFB92:
	save	%sp, -112, %sp
.LLCFI92:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 4
	ble	%icc, .LL554
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL554:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL555
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL556
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 28, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 28, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL557
	 nop
.LL556:
	ldd	[%fp-16], %o4
.LL557:
	ba,pt	%xcc, .LL558
	 nop
.LL555:
	ldd	[%fp-8], %o4
.LL558:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	insert_constant_offset__Long_insert_constant_offset_60, .-insert_constant_offset__Long_insert_constant_offset_60
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_61, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_61:
.LLFB93:
	save	%sp, -112, %sp
.LLCFI93:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 3
	ble	%icc, .LL560
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL560:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL561
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL562
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 29, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 29, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL563
	 nop
.LL562:
	ldd	[%fp-16], %o4
.LL563:
	ba,pt	%xcc, .LL564
	 nop
.LL561:
	ldd	[%fp-8], %o4
.LL564:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	insert_constant_offset__Long_insert_constant_offset_61, .-insert_constant_offset__Long_insert_constant_offset_61
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_62, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_62:
.LLFB94:
	save	%sp, -112, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 2
	ble	%icc, .LL566
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL566:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL567
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL568
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 30, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 30, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL569
	 nop
.LL568:
	ldd	[%fp-16], %o4
.LL569:
	ba,pt	%xcc, .LL570
	 nop
.LL567:
	ldd	[%fp-8], %o4
.LL570:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	insert_constant_offset__Long_insert_constant_offset_62, .-insert_constant_offset__Long_insert_constant_offset_62
	.align 4
	.type	insert_constant_offset__Long_insert_constant_offset_63, #function
	.proc	05
insert_constant_offset__Long_insert_constant_offset_63:
.LLFB95:
	save	%sp, -112, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 1
	ble	%icc, .LL572
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL572:
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 0
	be	%icc, .LL573
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	cmp	%g1, 64
	be	%icc, .LL574
	 nop
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o4
	or	%o4, 1023, %o4
	mov	%o4, %o5
	sllx	%o4, 32, %o4
	srl	%o5, 0, %o5
	or	%o5, %o4, %o4
	sllx	%o4, %g1, %o5
	srlx	%o5, 32, %o4
	xnor	%o4, 0, %o4
	xnor	%o5, 0, %o5
	sll	%o5, 31, %l2
	mov	0, %l3
	xnor	%l2, 0, %o2
	xnor	%l3, 0, %o3
	ldd	[%fp-8], %o4
	and	%o2, %o4, %o4
	and	%o3, %o5, %o5
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	ld	[%g1+144], %g1
	sethi	%hi(4294966272), %o2
	or	%o2, 1023, %o2
	mov	%o2, %o3
	sllx	%o2, 32, %o2
	srl	%o3, 0, %o3
	or	%o3, %o2, %o2
	sllx	%o2, %g1, %o3
	srlx	%o3, 32, %o2
	xnor	%o2, 0, %o0
	xnor	%o3, 0, %o1
	ldd	[%fp-16], %o2
	and	%o0, %o2, %o2
	and	%o1, %o3, %o3
	sll	%o3, 31, %l0
	mov	0, %l1
	or	%o4, %l0, %o4
	or	%o5, %l1, %o5
	ba,pt	%xcc, .LL575
	 nop
.LL574:
	ldd	[%fp-16], %o4
.LL575:
	ba,pt	%xcc, .LL576
	 nop
.LL573:
	ldd	[%fp-8], %o4
.LL576:
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	insert_constant_offset__Long_insert_constant_offset_63, .-insert_constant_offset__Long_insert_constant_offset_63
	.align 4
	.global insert_constant_offset_M3
	.type	insert_constant_offset_M3, #function
	.proc	0120
insert_constant_offset_M3:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	st	%i0, [%fp+68]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	insert_constant_offset_M3, .-insert_constant_offset_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	st	%i0, [%fp+68]
	sethi	%hi(MM_insert_constant_offset), %g1
	or	%g1, %lo(MM_insert_constant_offset), %o0
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
	.size	L_1, 3800
L_1:
	.ascii	"insert_constant_offset_M3"
	.skip 1
	.ascii	"Long_insert_constant_offset_63"
	.skip 1
	.ascii	"Long_insert_constant_offset_62"
	.skip 1
	.ascii	"Long_insert_constant_offset_61"
	.skip 1
	.ascii	"Long_insert_constant_offset_60"
	.skip 1
	.ascii	"Long_insert_constant_offset_59"
	.skip 1
	.ascii	"Long_insert_constant_offset_58"
	.skip 1
	.ascii	"Long_insert_constant_offset_57"
	.skip 1
	.ascii	"Long_insert_constant_offset_56"
	.skip 1
	.ascii	"Long_insert_constant_offset_55"
	.skip 1
	.ascii	"Long_insert_constant_offset_54"
	.skip 1
	.ascii	"Long_insert_constant_offset_53"
	.skip 1
	.ascii	"Long_insert_constant_offset_52"
	.skip 1
	.ascii	"Long_insert_constant_offset_51"
	.skip 1
	.ascii	"Long_insert_constant_offset_50"
	.skip 1
	.ascii	"Long_insert_constant_offset_49"
	.skip 1
	.ascii	"Long_insert_constant_offset_48"
	.skip 1
	.ascii	"Long_insert_constant_offset_47"
	.skip 1
	.ascii	"Long_insert_constant_offset_46"
	.skip 1
	.ascii	"Long_insert_constant_offset_45"
	.skip 1
	.ascii	"Long_insert_constant_offset_44"
	.skip 1
	.ascii	"Long_insert_constant_offset_43"
	.skip 1
	.ascii	"Long_insert_constant_offset_42"
	.skip 1
	.ascii	"Long_insert_constant_offset_41"
	.skip 1
	.ascii	"Long_insert_constant_offset_40"
	.skip 1
	.ascii	"Long_insert_constant_offset_39"
	.skip 1
	.ascii	"Long_insert_constant_offset_38"
	.skip 1
	.ascii	"Long_insert_constant_offset_37"
	.skip 1
	.ascii	"Long_insert_constant_offset_36"
	.skip 1
	.ascii	"Long_insert_constant_offset_35"
	.skip 1
	.ascii	"Long_insert_constant_offset_34"
	.skip 1
	.ascii	"Long_insert_constant_offset_33"
	.skip 1
	.ascii	"Long_insert_constant_offset_32"
	.skip 1
	.ascii	"Long_insert_constant_offset_31"
	.skip 1
	.ascii	"Long_insert_constant_offset_30"
	.skip 1
	.ascii	"Long_insert_constant_offset_29"
	.skip 1
	.ascii	"Long_insert_constant_offset_28"
	.skip 1
	.ascii	"Long_insert_constant_offset_27"
	.skip 1
	.ascii	"Long_insert_constant_offset_26"
	.skip 1
	.ascii	"Long_insert_constant_offset_25"
	.skip 1
	.ascii	"Long_insert_constant_offset_24"
	.skip 1
	.ascii	"Long_insert_constant_offset_23"
	.skip 1
	.ascii	"Long_insert_constant_offset_22"
	.skip 1
	.ascii	"Long_insert_constant_offset_21"
	.skip 1
	.ascii	"Long_insert_constant_offset_20"
	.skip 1
	.ascii	"Long_insert_constant_offset_19"
	.skip 1
	.ascii	"Long_insert_constant_offset_18"
	.skip 1
	.ascii	"Long_insert_constant_offset_17"
	.skip 1
	.ascii	"Long_insert_constant_offset_16"
	.skip 1
	.ascii	"Long_insert_constant_offset_15"
	.skip 1
	.ascii	"Long_insert_constant_offset_14"
	.skip 1
	.ascii	"Long_insert_constant_offset_13"
	.skip 1
	.ascii	"Long_insert_constant_offset_12"
	.skip 1
	.ascii	"Long_insert_constant_offset_11"
	.skip 1
	.ascii	"Long_insert_constant_offset_10"
	.skip 1
	.ascii	"Long_insert_constant_offset_9"
	.skip 1
	.ascii	"Long_insert_constant_offset_8"
	.skip 1
	.ascii	"Long_insert_constant_offset_7"
	.skip 1
	.ascii	"Long_insert_constant_offset_6"
	.skip 1
	.ascii	"Long_insert_constant_offset_5"
	.skip 1
	.ascii	"Long_insert_constant_offset_4"
	.skip 1
	.ascii	"Long_insert_constant_offset_3"
	.skip 1
	.ascii	"Long_insert_constant_offset_2"
	.skip 1
	.ascii	"Long_insert_constant_offset_1"
	.skip 1
	.ascii	"Long_insert_constant_offset_0"
	.skip 1
	.ascii	"Word_insert_constant_offset_31"
	.skip 1
	.ascii	"Word_insert_constant_offset_30"
	.skip 1
	.ascii	"Word_insert_constant_offset_29"
	.skip 1
	.ascii	"Word_insert_constant_offset_28"
	.skip 1
	.ascii	"Word_insert_constant_offset_27"
	.skip 1
	.ascii	"Word_insert_constant_offset_26"
	.skip 1
	.ascii	"Word_insert_constant_offset_25"
	.skip 1
	.ascii	"Word_insert_constant_offset_24"
	.skip 1
	.ascii	"Word_insert_constant_offset_23"
	.skip 1
	.ascii	"Word_insert_constant_offset_22"
	.skip 1
	.ascii	"Word_insert_constant_offset_21"
	.skip 1
	.ascii	"Word_insert_constant_offset_20"
	.skip 1
	.ascii	"Word_insert_constant_offset_19"
	.skip 1
	.ascii	"Word_insert_constant_offset_18"
	.skip 1
	.ascii	"Word_insert_constant_offset_17"
	.skip 1
	.ascii	"Word_insert_constant_offset_16"
	.skip 1
	.ascii	"Word_insert_constant_offset_15"
	.skip 1
	.ascii	"Word_insert_constant_offset_14"
	.skip 1
	.ascii	"Word_insert_constant_offset_13"
	.skip 1
	.ascii	"Word_insert_constant_offset_12"
	.skip 1
	.ascii	"Word_insert_constant_offset_11"
	.skip 1
	.ascii	"Word_insert_constant_offset_10"
	.skip 1
	.ascii	"Word_insert_constant_offset_9"
	.skip 1
	.ascii	"Word_insert_constant_offset_8"
	.skip 1
	.ascii	"Word_insert_constant_offset_7"
	.skip 1
	.ascii	"Word_insert_constant_offset_6"
	.skip 1
	.ascii	"Word_insert_constant_offset_5"
	.skip 1
	.ascii	"Word_insert_constant_offset_4"
	.skip 1
	.ascii	"Word_insert_constant_offset_3"
	.skip 1
	.ascii	"Word_insert_constant_offset_2"
	.skip 1
	.ascii	"Word_insert_constant_offset_1"
	.skip 1
	.ascii	"Word_insert_constant_offset_0"
	.skip 3
	.long	insert_constant_offset_M3
	.long	L_1
	.long	insert_constant_offset__Long_insert_constant_offset_63
	.long	L_1+26
	.long	insert_constant_offset__Long_insert_constant_offset_62
	.long	L_1+57
	.long	insert_constant_offset__Long_insert_constant_offset_61
	.long	L_1+88
	.long	insert_constant_offset__Long_insert_constant_offset_60
	.long	L_1+119
	.long	insert_constant_offset__Long_insert_constant_offset_59
	.long	L_1+150
	.long	insert_constant_offset__Long_insert_constant_offset_58
	.long	L_1+181
	.long	insert_constant_offset__Long_insert_constant_offset_57
	.long	L_1+212
	.long	insert_constant_offset__Long_insert_constant_offset_56
	.long	L_1+243
	.long	insert_constant_offset__Long_insert_constant_offset_55
	.long	L_1+274
	.long	insert_constant_offset__Long_insert_constant_offset_54
	.long	L_1+305
	.long	insert_constant_offset__Long_insert_constant_offset_53
	.long	L_1+336
	.long	insert_constant_offset__Long_insert_constant_offset_52
	.long	L_1+367
	.long	insert_constant_offset__Long_insert_constant_offset_51
	.long	L_1+398
	.long	insert_constant_offset__Long_insert_constant_offset_50
	.long	L_1+429
	.long	insert_constant_offset__Long_insert_constant_offset_49
	.long	L_1+460
	.long	insert_constant_offset__Long_insert_constant_offset_48
	.long	L_1+491
	.long	insert_constant_offset__Long_insert_constant_offset_47
	.long	L_1+522
	.long	insert_constant_offset__Long_insert_constant_offset_46
	.long	L_1+553
	.long	insert_constant_offset__Long_insert_constant_offset_45
	.long	L_1+584
	.long	insert_constant_offset__Long_insert_constant_offset_44
	.long	L_1+615
	.long	insert_constant_offset__Long_insert_constant_offset_43
	.long	L_1+646
	.long	insert_constant_offset__Long_insert_constant_offset_42
	.long	L_1+677
	.long	insert_constant_offset__Long_insert_constant_offset_41
	.long	L_1+708
	.long	insert_constant_offset__Long_insert_constant_offset_40
	.long	L_1+739
	.long	insert_constant_offset__Long_insert_constant_offset_39
	.long	L_1+770
	.long	insert_constant_offset__Long_insert_constant_offset_38
	.long	L_1+801
	.long	insert_constant_offset__Long_insert_constant_offset_37
	.long	L_1+832
	.long	insert_constant_offset__Long_insert_constant_offset_36
	.long	L_1+863
	.long	insert_constant_offset__Long_insert_constant_offset_35
	.long	L_1+894
	.long	insert_constant_offset__Long_insert_constant_offset_34
	.long	L_1+925
	.long	insert_constant_offset__Long_insert_constant_offset_33
	.long	L_1+956
	.long	insert_constant_offset__Long_insert_constant_offset_32
	.long	L_1+987
	.long	insert_constant_offset__Long_insert_constant_offset_31
	.long	L_1+1018
	.long	insert_constant_offset__Long_insert_constant_offset_30
	.long	L_1+1049
	.long	insert_constant_offset__Long_insert_constant_offset_29
	.long	L_1+1080
	.long	insert_constant_offset__Long_insert_constant_offset_28
	.long	L_1+1111
	.long	insert_constant_offset__Long_insert_constant_offset_27
	.long	L_1+1142
	.long	insert_constant_offset__Long_insert_constant_offset_26
	.long	L_1+1173
	.long	insert_constant_offset__Long_insert_constant_offset_25
	.long	L_1+1204
	.long	insert_constant_offset__Long_insert_constant_offset_24
	.long	L_1+1235
	.long	insert_constant_offset__Long_insert_constant_offset_23
	.long	L_1+1266
	.long	insert_constant_offset__Long_insert_constant_offset_22
	.long	L_1+1297
	.long	insert_constant_offset__Long_insert_constant_offset_21
	.long	L_1+1328
	.long	insert_constant_offset__Long_insert_constant_offset_20
	.long	L_1+1359
	.long	insert_constant_offset__Long_insert_constant_offset_19
	.long	L_1+1390
	.long	insert_constant_offset__Long_insert_constant_offset_18
	.long	L_1+1421
	.long	insert_constant_offset__Long_insert_constant_offset_17
	.long	L_1+1452
	.long	insert_constant_offset__Long_insert_constant_offset_16
	.long	L_1+1483
	.long	insert_constant_offset__Long_insert_constant_offset_15
	.long	L_1+1514
	.long	insert_constant_offset__Long_insert_constant_offset_14
	.long	L_1+1545
	.long	insert_constant_offset__Long_insert_constant_offset_13
	.long	L_1+1576
	.long	insert_constant_offset__Long_insert_constant_offset_12
	.long	L_1+1607
	.long	insert_constant_offset__Long_insert_constant_offset_11
	.long	L_1+1638
	.long	insert_constant_offset__Long_insert_constant_offset_10
	.long	L_1+1669
	.long	insert_constant_offset__Long_insert_constant_offset_9
	.long	L_1+1700
	.long	insert_constant_offset__Long_insert_constant_offset_8
	.long	L_1+1730
	.long	insert_constant_offset__Long_insert_constant_offset_7
	.long	L_1+1760
	.long	insert_constant_offset__Long_insert_constant_offset_6
	.long	L_1+1790
	.long	insert_constant_offset__Long_insert_constant_offset_5
	.long	L_1+1820
	.long	insert_constant_offset__Long_insert_constant_offset_4
	.long	L_1+1850
	.long	insert_constant_offset__Long_insert_constant_offset_3
	.long	L_1+1880
	.long	insert_constant_offset__Long_insert_constant_offset_2
	.long	L_1+1910
	.long	insert_constant_offset__Long_insert_constant_offset_1
	.long	L_1+1940
	.long	insert_constant_offset__Long_insert_constant_offset_0
	.long	L_1+1970
	.long	insert_constant_offset__Word_insert_constant_offset_31
	.long	L_1+2000
	.long	insert_constant_offset__Word_insert_constant_offset_30
	.long	L_1+2031
	.long	insert_constant_offset__Word_insert_constant_offset_29
	.long	L_1+2062
	.long	insert_constant_offset__Word_insert_constant_offset_28
	.long	L_1+2093
	.long	insert_constant_offset__Word_insert_constant_offset_27
	.long	L_1+2124
	.long	insert_constant_offset__Word_insert_constant_offset_26
	.long	L_1+2155
	.long	insert_constant_offset__Word_insert_constant_offset_25
	.long	L_1+2186
	.long	insert_constant_offset__Word_insert_constant_offset_24
	.long	L_1+2217
	.long	insert_constant_offset__Word_insert_constant_offset_23
	.long	L_1+2248
	.long	insert_constant_offset__Word_insert_constant_offset_22
	.long	L_1+2279
	.long	insert_constant_offset__Word_insert_constant_offset_21
	.long	L_1+2310
	.long	insert_constant_offset__Word_insert_constant_offset_20
	.long	L_1+2341
	.long	insert_constant_offset__Word_insert_constant_offset_19
	.long	L_1+2372
	.long	insert_constant_offset__Word_insert_constant_offset_18
	.long	L_1+2403
	.long	insert_constant_offset__Word_insert_constant_offset_17
	.long	L_1+2434
	.long	insert_constant_offset__Word_insert_constant_offset_16
	.long	L_1+2465
	.long	insert_constant_offset__Word_insert_constant_offset_15
	.long	L_1+2496
	.long	insert_constant_offset__Word_insert_constant_offset_14
	.long	L_1+2527
	.long	insert_constant_offset__Word_insert_constant_offset_13
	.long	L_1+2558
	.long	insert_constant_offset__Word_insert_constant_offset_12
	.long	L_1+2589
	.long	insert_constant_offset__Word_insert_constant_offset_11
	.long	L_1+2620
	.long	insert_constant_offset__Word_insert_constant_offset_10
	.long	L_1+2651
	.long	insert_constant_offset__Word_insert_constant_offset_9
	.long	L_1+2682
	.long	insert_constant_offset__Word_insert_constant_offset_8
	.long	L_1+2712
	.long	insert_constant_offset__Word_insert_constant_offset_7
	.long	L_1+2742
	.long	insert_constant_offset__Word_insert_constant_offset_6
	.long	L_1+2772
	.long	insert_constant_offset__Word_insert_constant_offset_5
	.long	L_1+2802
	.long	insert_constant_offset__Word_insert_constant_offset_4
	.long	L_1+2832
	.long	insert_constant_offset__Word_insert_constant_offset_3
	.long	L_1+2862
	.long	insert_constant_offset__Word_insert_constant_offset_2
	.long	L_1+2892
	.long	insert_constant_offset__Word_insert_constant_offset_1
	.long	L_1+2922
	.long	insert_constant_offset__Word_insert_constant_offset_0
	.long	L_1+2952
	.skip 4
	.ascii	"../SOLsun/insert_constant_offset.m3"
	.skip 1
	.align 8
	.type	MM_insert_constant_offset, #object
	.size	MM_insert_constant_offset, 208
MM_insert_constant_offset:
	.long	L_1+3764
	.skip 16
	.long	L_1+2984
	.skip 12
	.long	MM_insert_constant_offset+148
	.skip 4
	.long	insert_constant_offset_M3
	.long	3
	.half	977
	.skip 2
	.long	0
	.long	978
	.long	0
	.long	979
	.long	980
	.half	981
	.skip 2
	.long	982
	.skip 4
	.long	0
	.long	983
	.long	984
	.skip 4
	.long	1083101155
	.long	1408749273
	.byte	-37
	.skip 7
	.long	0
	.long	988
	.byte	-35
	.skip 3
	.long	990
	.long	1148714877
	.skip 12
	.long	insert_constant_offset_I3
	.long	MM_insert_constant_offset+160
	.skip 4
	.long	Long_I3
	.long	MM_insert_constant_offset+172
	.skip 4
	.long	Word_I3
	.long	MM_insert_constant_offset+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_insert_constant_offset+196
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
