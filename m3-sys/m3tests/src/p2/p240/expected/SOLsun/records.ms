	.file	"records.mc"
	.section	".text"
	.align 4
	.type	records__ARCHAR1, #function
	.proc	014
records__ARCHAR1:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	%i0, %g1
	ldub	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	records__ARCHAR1, .-records__ARCHAR1
	.align 4
	.type	records__BRCHAR1, #function
	.proc	020
records__BRCHAR1:
.LLFB1:
	save	%sp, -104, %sp
.LLCFI1:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	1, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1:
	.size	records__BRCHAR1, .-records__BRCHAR1
	.align 4
	.type	records__CRCHAR1, #function
	.proc	014
records__CRCHAR1:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	records__CRCHAR1, .-records__CRCHAR1
	.align 4
	.type	records__DRCHAR1, #function
	.proc	020
records__DRCHAR1:
.LLFB3:
	save	%sp, -104, %sp
.LLCFI3:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	1, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE3:
	.size	records__DRCHAR1, .-records__DRCHAR1
	.align 4
	.type	records__ERCHAR1, #function
	.proc	014
records__ERCHAR1:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	records__ERCHAR1, .-records__ERCHAR1
	.align 4
	.type	records__FRCHAR1, #function
	.proc	020
records__FRCHAR1:
.LLFB5:
	save	%sp, -104, %sp
.LLCFI5:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	1, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE5:
	.size	records__FRCHAR1, .-records__FRCHAR1
	.align 4
	.type	records__ARCHAR2, #function
	.proc	014
records__ARCHAR2:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	records__ARCHAR2, .-records__ARCHAR2
	.align 4
	.type	records__BRCHAR2, #function
	.proc	020
records__BRCHAR2:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE7:
	.size	records__BRCHAR2, .-records__BRCHAR2
	.align 4
	.type	records__CRCHAR2, #function
	.proc	014
records__CRCHAR2:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	records__CRCHAR2, .-records__CRCHAR2
	.align 4
	.type	records__DRCHAR2, #function
	.proc	020
records__DRCHAR2:
.LLFB9:
	save	%sp, -104, %sp
.LLCFI9:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE9:
	.size	records__DRCHAR2, .-records__DRCHAR2
	.align 4
	.type	records__ERCHAR2, #function
	.proc	014
records__ERCHAR2:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	records__ERCHAR2, .-records__ERCHAR2
	.align 4
	.type	records__FRCHAR2, #function
	.proc	020
records__FRCHAR2:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE11:
	.size	records__FRCHAR2, .-records__FRCHAR2
	.align 4
	.type	records__ARCHAR3, #function
	.proc	014
records__ARCHAR3:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	mov	%i0, %g1
	ldub	[%g1+1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	records__ARCHAR3, .-records__ARCHAR3
	.align 4
	.type	records__BRCHAR3, #function
	.proc	020
records__BRCHAR3:
.LLFB13:
	save	%sp, -104, %sp
.LLCFI13:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	3, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE13:
	.size	records__BRCHAR3, .-records__BRCHAR3
	.align 4
	.type	records__CRCHAR3, #function
	.proc	014
records__CRCHAR3:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	records__CRCHAR3, .-records__CRCHAR3
	.align 4
	.type	records__DRCHAR3, #function
	.proc	020
records__DRCHAR3:
.LLFB15:
	save	%sp, -104, %sp
.LLCFI15:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	3, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE15:
	.size	records__DRCHAR3, .-records__DRCHAR3
	.align 4
	.type	records__ERCHAR3, #function
	.proc	014
records__ERCHAR3:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	records__ERCHAR3, .-records__ERCHAR3
	.align 4
	.type	records__FRCHAR3, #function
	.proc	020
records__FRCHAR3:
.LLFB17:
	save	%sp, -104, %sp
.LLCFI17:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	3, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE17:
	.size	records__FRCHAR3, .-records__FRCHAR3
	.align 4
	.type	records__ARCHAR4, #function
	.proc	014
records__ARCHAR4:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	records__ARCHAR4, .-records__ARCHAR4
	.align 4
	.type	records__BRCHAR4, #function
	.proc	020
records__BRCHAR4:
.LLFB19:
	save	%sp, -104, %sp
.LLCFI19:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE19:
	.size	records__BRCHAR4, .-records__BRCHAR4
	.align 4
	.type	records__CRCHAR4, #function
	.proc	014
records__CRCHAR4:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	records__CRCHAR4, .-records__CRCHAR4
	.align 4
	.type	records__DRCHAR4, #function
	.proc	020
records__DRCHAR4:
.LLFB21:
	save	%sp, -104, %sp
.LLCFI21:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE21:
	.size	records__DRCHAR4, .-records__DRCHAR4
	.align 4
	.type	records__ERCHAR4, #function
	.proc	014
records__ERCHAR4:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	records__ERCHAR4, .-records__ERCHAR4
	.align 4
	.type	records__FRCHAR4, #function
	.proc	020
records__FRCHAR4:
.LLFB23:
	save	%sp, -104, %sp
.LLCFI23:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE23:
	.size	records__FRCHAR4, .-records__FRCHAR4
	.align 4
	.type	records__ARCHAR5, #function
	.proc	014
records__ARCHAR5:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	mov	%i0, %g1
	ldub	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE24:
	.size	records__ARCHAR5, .-records__ARCHAR5
	.align 4
	.type	records__BRCHAR5, #function
	.proc	020
records__BRCHAR5:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	5, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE25:
	.size	records__BRCHAR5, .-records__BRCHAR5
	.align 4
	.type	records__CRCHAR5, #function
	.proc	014
records__CRCHAR5:
.LLFB26:
	save	%sp, -96, %sp
.LLCFI26:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE26:
	.size	records__CRCHAR5, .-records__CRCHAR5
	.align 4
	.type	records__DRCHAR5, #function
	.proc	020
records__DRCHAR5:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	5, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE27:
	.size	records__DRCHAR5, .-records__DRCHAR5
	.align 4
	.type	records__ERCHAR5, #function
	.proc	014
records__ERCHAR5:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	records__ERCHAR5, .-records__ERCHAR5
	.align 4
	.type	records__FRCHAR5, #function
	.proc	020
records__FRCHAR5:
.LLFB29:
	save	%sp, -104, %sp
.LLCFI29:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	5, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE29:
	.size	records__FRCHAR5, .-records__FRCHAR5
	.align 4
	.type	records__ARCHAR6, #function
	.proc	014
records__ARCHAR6:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	mov	%i0, %g1
	lduh	[%g1+2], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	records__ARCHAR6, .-records__ARCHAR6
	.align 4
	.type	records__BRCHAR6, #function
	.proc	020
records__BRCHAR6:
.LLFB31:
	save	%sp, -104, %sp
.LLCFI31:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE31:
	.size	records__BRCHAR6, .-records__BRCHAR6
	.align 4
	.type	records__CRCHAR6, #function
	.proc	014
records__CRCHAR6:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+2], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	records__CRCHAR6, .-records__CRCHAR6
	.align 4
	.type	records__DRCHAR6, #function
	.proc	020
records__DRCHAR6:
.LLFB33:
	save	%sp, -104, %sp
.LLCFI33:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE33:
	.size	records__DRCHAR6, .-records__DRCHAR6
	.align 4
	.type	records__ERCHAR6, #function
	.proc	014
records__ERCHAR6:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+2], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	records__ERCHAR6, .-records__ERCHAR6
	.align 4
	.type	records__FRCHAR6, #function
	.proc	020
records__FRCHAR6:
.LLFB35:
	save	%sp, -104, %sp
.LLCFI35:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE35:
	.size	records__FRCHAR6, .-records__FRCHAR6
	.align 4
	.type	records__ARCHAR7, #function
	.proc	014
records__ARCHAR7:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	mov	%i0, %g1
	ldub	[%g1+3], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	records__ARCHAR7, .-records__ARCHAR7
	.align 4
	.type	records__BRCHAR7, #function
	.proc	020
records__BRCHAR7:
.LLFB37:
	save	%sp, -104, %sp
.LLCFI37:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	7, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE37:
	.size	records__BRCHAR7, .-records__BRCHAR7
	.align 4
	.type	records__CRCHAR7, #function
	.proc	014
records__CRCHAR7:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+3], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE38:
	.size	records__CRCHAR7, .-records__CRCHAR7
	.align 4
	.type	records__DRCHAR7, #function
	.proc	020
records__DRCHAR7:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	7, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE39:
	.size	records__DRCHAR7, .-records__DRCHAR7
	.align 4
	.type	records__ERCHAR7, #function
	.proc	014
records__ERCHAR7:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+3], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	records__ERCHAR7, .-records__ERCHAR7
	.align 4
	.type	records__FRCHAR7, #function
	.proc	020
records__FRCHAR7:
.LLFB41:
	save	%sp, -104, %sp
.LLCFI41:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	7, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE41:
	.size	records__FRCHAR7, .-records__FRCHAR7
	.align 4
	.type	records__ARCHAR8, #function
	.proc	014
records__ARCHAR8:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	records__ARCHAR8, .-records__ARCHAR8
	.align 4
	.type	records__BRCHAR8, #function
	.proc	020
records__BRCHAR8:
.LLFB43:
	save	%sp, -104, %sp
.LLCFI43:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE43:
	.size	records__BRCHAR8, .-records__BRCHAR8
	.align 4
	.type	records__CRCHAR8, #function
	.proc	014
records__CRCHAR8:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	records__CRCHAR8, .-records__CRCHAR8
	.align 4
	.type	records__DRCHAR8, #function
	.proc	020
records__DRCHAR8:
.LLFB45:
	save	%sp, -104, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE45:
	.size	records__DRCHAR8, .-records__DRCHAR8
	.align 4
	.type	records__ERCHAR8, #function
	.proc	014
records__ERCHAR8:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	records__ERCHAR8, .-records__ERCHAR8
	.align 4
	.type	records__FRCHAR8, #function
	.proc	020
records__FRCHAR8:
.LLFB47:
	save	%sp, -104, %sp
.LLCFI47:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE47:
	.size	records__FRCHAR8, .-records__FRCHAR8
	.align 4
	.type	records__ARCHAR9, #function
	.proc	014
records__ARCHAR9:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	mov	%i0, %g1
	ldub	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	records__ARCHAR9, .-records__ARCHAR9
	.align 4
	.type	records__BRCHAR9, #function
	.proc	020
records__BRCHAR9:
.LLFB49:
	save	%sp, -112, %sp
.LLCFI49:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	9, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE49:
	.size	records__BRCHAR9, .-records__BRCHAR9
	.align 4
	.type	records__CRCHAR9, #function
	.proc	014
records__CRCHAR9:
.LLFB50:
	save	%sp, -96, %sp
.LLCFI50:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE50:
	.size	records__CRCHAR9, .-records__CRCHAR9
	.align 4
	.type	records__DRCHAR9, #function
	.proc	020
records__DRCHAR9:
.LLFB51:
	save	%sp, -112, %sp
.LLCFI51:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	9, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE51:
	.size	records__DRCHAR9, .-records__DRCHAR9
	.align 4
	.type	records__ERCHAR9, #function
	.proc	014
records__ERCHAR9:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	records__ERCHAR9, .-records__ERCHAR9
	.align 4
	.type	records__FRCHAR9, #function
	.proc	020
records__FRCHAR9:
.LLFB53:
	save	%sp, -112, %sp
.LLCFI53:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	9, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE53:
	.size	records__FRCHAR9, .-records__FRCHAR9
	.align 4
	.type	records__ARCHAR10, #function
	.proc	014
records__ARCHAR10:
.LLFB54:
	save	%sp, -96, %sp
.LLCFI54:
	mov	%i0, %g1
	lduh	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE54:
	.size	records__ARCHAR10, .-records__ARCHAR10
	.align 4
	.type	records__BRCHAR10, #function
	.proc	020
records__BRCHAR10:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE55:
	.size	records__BRCHAR10, .-records__BRCHAR10
	.align 4
	.type	records__CRCHAR10, #function
	.proc	014
records__CRCHAR10:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE56:
	.size	records__CRCHAR10, .-records__CRCHAR10
	.align 4
	.type	records__DRCHAR10, #function
	.proc	020
records__DRCHAR10:
.LLFB57:
	save	%sp, -112, %sp
.LLCFI57:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE57:
	.size	records__DRCHAR10, .-records__DRCHAR10
	.align 4
	.type	records__ERCHAR10, #function
	.proc	014
records__ERCHAR10:
.LLFB58:
	save	%sp, -96, %sp
.LLCFI58:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE58:
	.size	records__ERCHAR10, .-records__ERCHAR10
	.align 4
	.type	records__FRCHAR10, #function
	.proc	020
records__FRCHAR10:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE59:
	.size	records__FRCHAR10, .-records__FRCHAR10
	.align 4
	.type	records__ARCHAR11, #function
	.proc	014
records__ARCHAR11:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	mov	%i0, %g1
	ldub	[%g1+5], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	records__ARCHAR11, .-records__ARCHAR11
	.align 4
	.type	records__BRCHAR11, #function
	.proc	020
records__BRCHAR11:
.LLFB61:
	save	%sp, -112, %sp
.LLCFI61:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	11, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE61:
	.size	records__BRCHAR11, .-records__BRCHAR11
	.align 4
	.type	records__CRCHAR11, #function
	.proc	014
records__CRCHAR11:
.LLFB62:
	save	%sp, -96, %sp
.LLCFI62:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+5], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE62:
	.size	records__CRCHAR11, .-records__CRCHAR11
	.align 4
	.type	records__DRCHAR11, #function
	.proc	020
records__DRCHAR11:
.LLFB63:
	save	%sp, -112, %sp
.LLCFI63:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	11, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE63:
	.size	records__DRCHAR11, .-records__DRCHAR11
	.align 4
	.type	records__ERCHAR11, #function
	.proc	014
records__ERCHAR11:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+5], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	records__ERCHAR11, .-records__ERCHAR11
	.align 4
	.type	records__FRCHAR11, #function
	.proc	020
records__FRCHAR11:
.LLFB65:
	save	%sp, -112, %sp
.LLCFI65:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	11, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE65:
	.size	records__FRCHAR11, .-records__FRCHAR11
	.align 4
	.type	records__ARCHAR12, #function
	.proc	014
records__ARCHAR12:
.LLFB66:
	save	%sp, -96, %sp
.LLCFI66:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	records__ARCHAR12, .-records__ARCHAR12
	.align 4
	.type	records__BRCHAR12, #function
	.proc	020
records__BRCHAR12:
.LLFB67:
	save	%sp, -112, %sp
.LLCFI67:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE67:
	.size	records__BRCHAR12, .-records__BRCHAR12
	.align 4
	.type	records__CRCHAR12, #function
	.proc	014
records__CRCHAR12:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	records__CRCHAR12, .-records__CRCHAR12
	.align 4
	.type	records__DRCHAR12, #function
	.proc	020
records__DRCHAR12:
.LLFB69:
	save	%sp, -112, %sp
.LLCFI69:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE69:
	.size	records__DRCHAR12, .-records__DRCHAR12
	.align 4
	.type	records__ERCHAR12, #function
	.proc	014
records__ERCHAR12:
.LLFB70:
	save	%sp, -96, %sp
.LLCFI70:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	records__ERCHAR12, .-records__ERCHAR12
	.align 4
	.type	records__FRCHAR12, #function
	.proc	020
records__FRCHAR12:
.LLFB71:
	save	%sp, -112, %sp
.LLCFI71:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE71:
	.size	records__FRCHAR12, .-records__FRCHAR12
	.align 4
	.type	records__ARCHAR13, #function
	.proc	014
records__ARCHAR13:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	mov	%i0, %g1
	ldub	[%g1+6], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	records__ARCHAR13, .-records__ARCHAR13
	.align 4
	.type	records__BRCHAR13, #function
	.proc	020
records__BRCHAR13:
.LLFB73:
	save	%sp, -112, %sp
.LLCFI73:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	13, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE73:
	.size	records__BRCHAR13, .-records__BRCHAR13
	.align 4
	.type	records__CRCHAR13, #function
	.proc	014
records__CRCHAR13:
.LLFB74:
	save	%sp, -96, %sp
.LLCFI74:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+6], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE74:
	.size	records__CRCHAR13, .-records__CRCHAR13
	.align 4
	.type	records__DRCHAR13, #function
	.proc	020
records__DRCHAR13:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	13, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE75:
	.size	records__DRCHAR13, .-records__DRCHAR13
	.align 4
	.type	records__ERCHAR13, #function
	.proc	014
records__ERCHAR13:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+6], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	records__ERCHAR13, .-records__ERCHAR13
	.align 4
	.type	records__FRCHAR13, #function
	.proc	020
records__FRCHAR13:
.LLFB77:
	save	%sp, -112, %sp
.LLCFI77:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	13, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE77:
	.size	records__FRCHAR13, .-records__FRCHAR13
	.align 4
	.type	records__ARCHAR14, #function
	.proc	014
records__ARCHAR14:
.LLFB78:
	save	%sp, -96, %sp
.LLCFI78:
	mov	%i0, %g1
	lduh	[%g1+6], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	records__ARCHAR14, .-records__ARCHAR14
	.align 4
	.type	records__BRCHAR14, #function
	.proc	020
records__BRCHAR14:
.LLFB79:
	save	%sp, -112, %sp
.LLCFI79:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	14, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE79:
	.size	records__BRCHAR14, .-records__BRCHAR14
	.align 4
	.type	records__CRCHAR14, #function
	.proc	014
records__CRCHAR14:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+6], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	records__CRCHAR14, .-records__CRCHAR14
	.align 4
	.type	records__DRCHAR14, #function
	.proc	020
records__DRCHAR14:
.LLFB81:
	save	%sp, -112, %sp
.LLCFI81:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	14, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE81:
	.size	records__DRCHAR14, .-records__DRCHAR14
	.align 4
	.type	records__ERCHAR14, #function
	.proc	014
records__ERCHAR14:
.LLFB82:
	save	%sp, -96, %sp
.LLCFI82:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+6], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	records__ERCHAR14, .-records__ERCHAR14
	.align 4
	.type	records__FRCHAR14, #function
	.proc	020
records__FRCHAR14:
.LLFB83:
	save	%sp, -112, %sp
.LLCFI83:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	14, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE83:
	.size	records__FRCHAR14, .-records__FRCHAR14
	.align 4
	.type	records__ARCHAR15, #function
	.proc	014
records__ARCHAR15:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	mov	%i0, %g1
	ldub	[%g1+7], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE84:
	.size	records__ARCHAR15, .-records__ARCHAR15
	.align 4
	.type	records__BRCHAR15, #function
	.proc	020
records__BRCHAR15:
.LLFB85:
	save	%sp, -112, %sp
.LLCFI85:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	15, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE85:
	.size	records__BRCHAR15, .-records__BRCHAR15
	.align 4
	.type	records__CRCHAR15, #function
	.proc	014
records__CRCHAR15:
.LLFB86:
	save	%sp, -96, %sp
.LLCFI86:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+7], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE86:
	.size	records__CRCHAR15, .-records__CRCHAR15
	.align 4
	.type	records__DRCHAR15, #function
	.proc	020
records__DRCHAR15:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	15, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE87:
	.size	records__DRCHAR15, .-records__DRCHAR15
	.align 4
	.type	records__ERCHAR15, #function
	.proc	014
records__ERCHAR15:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+7], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE88:
	.size	records__ERCHAR15, .-records__ERCHAR15
	.align 4
	.type	records__FRCHAR15, #function
	.proc	020
records__FRCHAR15:
.LLFB89:
	save	%sp, -112, %sp
.LLCFI89:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	15, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE89:
	.size	records__FRCHAR15, .-records__FRCHAR15
	.align 4
	.type	records__ARCHAR16, #function
	.proc	014
records__ARCHAR16:
.LLFB90:
	save	%sp, -96, %sp
.LLCFI90:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE90:
	.size	records__ARCHAR16, .-records__ARCHAR16
	.align 4
	.type	records__BRCHAR16, #function
	.proc	020
records__BRCHAR16:
.LLFB91:
	save	%sp, -112, %sp
.LLCFI91:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE91:
	.size	records__BRCHAR16, .-records__BRCHAR16
	.align 4
	.type	records__CRCHAR16, #function
	.proc	014
records__CRCHAR16:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE92:
	.size	records__CRCHAR16, .-records__CRCHAR16
	.align 4
	.type	records__DRCHAR16, #function
	.proc	020
records__DRCHAR16:
.LLFB93:
	save	%sp, -112, %sp
.LLCFI93:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE93:
	.size	records__DRCHAR16, .-records__DRCHAR16
	.align 4
	.type	records__ERCHAR16, #function
	.proc	014
records__ERCHAR16:
.LLFB94:
	save	%sp, -96, %sp
.LLCFI94:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE94:
	.size	records__ERCHAR16, .-records__ERCHAR16
	.align 4
	.type	records__FRCHAR16, #function
	.proc	020
records__FRCHAR16:
.LLFB95:
	save	%sp, -112, %sp
.LLCFI95:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE95:
	.size	records__FRCHAR16, .-records__FRCHAR16
	.align 4
	.type	records__ARCHAR17, #function
	.proc	014
records__ARCHAR17:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	mov	%i0, %g1
	ldub	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	records__ARCHAR17, .-records__ARCHAR17
	.align 4
	.type	records__BRCHAR17, #function
	.proc	020
records__BRCHAR17:
.LLFB97:
	save	%sp, -120, %sp
.LLCFI97:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	17, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE97:
	.size	records__BRCHAR17, .-records__BRCHAR17
	.align 4
	.type	records__CRCHAR17, #function
	.proc	014
records__CRCHAR17:
.LLFB98:
	save	%sp, -96, %sp
.LLCFI98:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE98:
	.size	records__CRCHAR17, .-records__CRCHAR17
	.align 4
	.type	records__DRCHAR17, #function
	.proc	020
records__DRCHAR17:
.LLFB99:
	save	%sp, -120, %sp
.LLCFI99:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	17, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE99:
	.size	records__DRCHAR17, .-records__DRCHAR17
	.align 4
	.type	records__ERCHAR17, #function
	.proc	014
records__ERCHAR17:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE100:
	.size	records__ERCHAR17, .-records__ERCHAR17
	.align 4
	.type	records__FRCHAR17, #function
	.proc	020
records__FRCHAR17:
.LLFB101:
	save	%sp, -120, %sp
.LLCFI101:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	17, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE101:
	.size	records__FRCHAR17, .-records__FRCHAR17
	.align 4
	.type	records__ARCHAR18, #function
	.proc	014
records__ARCHAR18:
.LLFB102:
	save	%sp, -96, %sp
.LLCFI102:
	mov	%i0, %g1
	lduh	[%g1+8], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	records__ARCHAR18, .-records__ARCHAR18
	.align 4
	.type	records__BRCHAR18, #function
	.proc	020
records__BRCHAR18:
.LLFB103:
	save	%sp, -120, %sp
.LLCFI103:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	18, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE103:
	.size	records__BRCHAR18, .-records__BRCHAR18
	.align 4
	.type	records__CRCHAR18, #function
	.proc	014
records__CRCHAR18:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+8], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	records__CRCHAR18, .-records__CRCHAR18
	.align 4
	.type	records__DRCHAR18, #function
	.proc	020
records__DRCHAR18:
.LLFB105:
	save	%sp, -120, %sp
.LLCFI105:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	18, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE105:
	.size	records__DRCHAR18, .-records__DRCHAR18
	.align 4
	.type	records__ERCHAR18, #function
	.proc	014
records__ERCHAR18:
.LLFB106:
	save	%sp, -96, %sp
.LLCFI106:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+8], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	records__ERCHAR18, .-records__ERCHAR18
	.align 4
	.type	records__FRCHAR18, #function
	.proc	020
records__FRCHAR18:
.LLFB107:
	save	%sp, -120, %sp
.LLCFI107:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	18, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE107:
	.size	records__FRCHAR18, .-records__FRCHAR18
	.align 4
	.type	records__ARCHAR19, #function
	.proc	014
records__ARCHAR19:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	mov	%i0, %g1
	ldub	[%g1+9], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE108:
	.size	records__ARCHAR19, .-records__ARCHAR19
	.align 4
	.type	records__BRCHAR19, #function
	.proc	020
records__BRCHAR19:
.LLFB109:
	save	%sp, -120, %sp
.LLCFI109:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	19, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE109:
	.size	records__BRCHAR19, .-records__BRCHAR19
	.align 4
	.type	records__CRCHAR19, #function
	.proc	014
records__CRCHAR19:
.LLFB110:
	save	%sp, -96, %sp
.LLCFI110:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+9], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE110:
	.size	records__CRCHAR19, .-records__CRCHAR19
	.align 4
	.type	records__DRCHAR19, #function
	.proc	020
records__DRCHAR19:
.LLFB111:
	save	%sp, -120, %sp
.LLCFI111:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	19, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE111:
	.size	records__DRCHAR19, .-records__DRCHAR19
	.align 4
	.type	records__ERCHAR19, #function
	.proc	014
records__ERCHAR19:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+9], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE112:
	.size	records__ERCHAR19, .-records__ERCHAR19
	.align 4
	.type	records__FRCHAR19, #function
	.proc	020
records__FRCHAR19:
.LLFB113:
	save	%sp, -120, %sp
.LLCFI113:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	19, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE113:
	.size	records__FRCHAR19, .-records__FRCHAR19
	.align 4
	.type	records__ARCHAR20, #function
	.proc	014
records__ARCHAR20:
.LLFB114:
	save	%sp, -96, %sp
.LLCFI114:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE114:
	.size	records__ARCHAR20, .-records__ARCHAR20
	.align 4
	.type	records__BRCHAR20, #function
	.proc	020
records__BRCHAR20:
.LLFB115:
	save	%sp, -120, %sp
.LLCFI115:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE115:
	.size	records__BRCHAR20, .-records__BRCHAR20
	.align 4
	.type	records__CRCHAR20, #function
	.proc	014
records__CRCHAR20:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE116:
	.size	records__CRCHAR20, .-records__CRCHAR20
	.align 4
	.type	records__DRCHAR20, #function
	.proc	020
records__DRCHAR20:
.LLFB117:
	save	%sp, -120, %sp
.LLCFI117:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE117:
	.size	records__DRCHAR20, .-records__DRCHAR20
	.align 4
	.type	records__ERCHAR20, #function
	.proc	014
records__ERCHAR20:
.LLFB118:
	save	%sp, -96, %sp
.LLCFI118:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE118:
	.size	records__ERCHAR20, .-records__ERCHAR20
	.align 4
	.type	records__FRCHAR20, #function
	.proc	020
records__FRCHAR20:
.LLFB119:
	save	%sp, -120, %sp
.LLCFI119:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE119:
	.size	records__FRCHAR20, .-records__FRCHAR20
	.align 4
	.type	records__ARCHAR21, #function
	.proc	014
records__ARCHAR21:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	mov	%i0, %g1
	ldub	[%g1+10], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	records__ARCHAR21, .-records__ARCHAR21
	.align 4
	.type	records__BRCHAR21, #function
	.proc	020
records__BRCHAR21:
.LLFB121:
	save	%sp, -120, %sp
.LLCFI121:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	21, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE121:
	.size	records__BRCHAR21, .-records__BRCHAR21
	.align 4
	.type	records__CRCHAR21, #function
	.proc	014
records__CRCHAR21:
.LLFB122:
	save	%sp, -96, %sp
.LLCFI122:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+10], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE122:
	.size	records__CRCHAR21, .-records__CRCHAR21
	.align 4
	.type	records__DRCHAR21, #function
	.proc	020
records__DRCHAR21:
.LLFB123:
	save	%sp, -120, %sp
.LLCFI123:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	21, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE123:
	.size	records__DRCHAR21, .-records__DRCHAR21
	.align 4
	.type	records__ERCHAR21, #function
	.proc	014
records__ERCHAR21:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+10], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	records__ERCHAR21, .-records__ERCHAR21
	.align 4
	.type	records__FRCHAR21, #function
	.proc	020
records__FRCHAR21:
.LLFB125:
	save	%sp, -120, %sp
.LLCFI125:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	21, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE125:
	.size	records__FRCHAR21, .-records__FRCHAR21
	.align 4
	.type	records__ARCHAR22, #function
	.proc	014
records__ARCHAR22:
.LLFB126:
	save	%sp, -96, %sp
.LLCFI126:
	mov	%i0, %g1
	lduh	[%g1+10], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	records__ARCHAR22, .-records__ARCHAR22
	.align 4
	.type	records__BRCHAR22, #function
	.proc	020
records__BRCHAR22:
.LLFB127:
	save	%sp, -120, %sp
.LLCFI127:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	22, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE127:
	.size	records__BRCHAR22, .-records__BRCHAR22
	.align 4
	.type	records__CRCHAR22, #function
	.proc	014
records__CRCHAR22:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+10], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	records__CRCHAR22, .-records__CRCHAR22
	.align 4
	.type	records__DRCHAR22, #function
	.proc	020
records__DRCHAR22:
.LLFB129:
	save	%sp, -120, %sp
.LLCFI129:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	22, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE129:
	.size	records__DRCHAR22, .-records__DRCHAR22
	.align 4
	.type	records__ERCHAR22, #function
	.proc	014
records__ERCHAR22:
.LLFB130:
	save	%sp, -96, %sp
.LLCFI130:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+10], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	records__ERCHAR22, .-records__ERCHAR22
	.align 4
	.type	records__FRCHAR22, #function
	.proc	020
records__FRCHAR22:
.LLFB131:
	save	%sp, -120, %sp
.LLCFI131:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	22, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE131:
	.size	records__FRCHAR22, .-records__FRCHAR22
	.align 4
	.type	records__ARCHAR23, #function
	.proc	014
records__ARCHAR23:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	mov	%i0, %g1
	ldub	[%g1+11], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE132:
	.size	records__ARCHAR23, .-records__ARCHAR23
	.align 4
	.type	records__BRCHAR23, #function
	.proc	020
records__BRCHAR23:
.LLFB133:
	save	%sp, -120, %sp
.LLCFI133:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	23, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE133:
	.size	records__BRCHAR23, .-records__BRCHAR23
	.align 4
	.type	records__CRCHAR23, #function
	.proc	014
records__CRCHAR23:
.LLFB134:
	save	%sp, -96, %sp
.LLCFI134:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+11], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE134:
	.size	records__CRCHAR23, .-records__CRCHAR23
	.align 4
	.type	records__DRCHAR23, #function
	.proc	020
records__DRCHAR23:
.LLFB135:
	save	%sp, -120, %sp
.LLCFI135:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	23, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE135:
	.size	records__DRCHAR23, .-records__DRCHAR23
	.align 4
	.type	records__ERCHAR23, #function
	.proc	014
records__ERCHAR23:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+11], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	records__ERCHAR23, .-records__ERCHAR23
	.align 4
	.type	records__FRCHAR23, #function
	.proc	020
records__FRCHAR23:
.LLFB137:
	save	%sp, -120, %sp
.LLCFI137:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	23, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE137:
	.size	records__FRCHAR23, .-records__FRCHAR23
	.align 4
	.type	records__ARCHAR24, #function
	.proc	014
records__ARCHAR24:
.LLFB138:
	save	%sp, -96, %sp
.LLCFI138:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE138:
	.size	records__ARCHAR24, .-records__ARCHAR24
	.align 4
	.type	records__BRCHAR24, #function
	.proc	020
records__BRCHAR24:
.LLFB139:
	save	%sp, -120, %sp
.LLCFI139:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE139:
	.size	records__BRCHAR24, .-records__BRCHAR24
	.align 4
	.type	records__CRCHAR24, #function
	.proc	014
records__CRCHAR24:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	records__CRCHAR24, .-records__CRCHAR24
	.align 4
	.type	records__DRCHAR24, #function
	.proc	020
records__DRCHAR24:
.LLFB141:
	save	%sp, -120, %sp
.LLCFI141:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE141:
	.size	records__DRCHAR24, .-records__DRCHAR24
	.align 4
	.type	records__ERCHAR24, #function
	.proc	014
records__ERCHAR24:
.LLFB142:
	save	%sp, -96, %sp
.LLCFI142:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	records__ERCHAR24, .-records__ERCHAR24
	.align 4
	.type	records__FRCHAR24, #function
	.proc	020
records__FRCHAR24:
.LLFB143:
	save	%sp, -120, %sp
.LLCFI143:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE143:
	.size	records__FRCHAR24, .-records__FRCHAR24
	.align 4
	.type	records__ARCHAR25, #function
	.proc	014
records__ARCHAR25:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	mov	%i0, %g1
	ldub	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	records__ARCHAR25, .-records__ARCHAR25
	.align 4
	.type	records__BRCHAR25, #function
	.proc	020
records__BRCHAR25:
.LLFB145:
	save	%sp, -128, %sp
.LLCFI145:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	25, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE145:
	.size	records__BRCHAR25, .-records__BRCHAR25
	.align 4
	.type	records__CRCHAR25, #function
	.proc	014
records__CRCHAR25:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	records__CRCHAR25, .-records__CRCHAR25
	.align 4
	.type	records__DRCHAR25, #function
	.proc	020
records__DRCHAR25:
.LLFB147:
	save	%sp, -128, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	25, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE147:
	.size	records__DRCHAR25, .-records__DRCHAR25
	.align 4
	.type	records__ERCHAR25, #function
	.proc	014
records__ERCHAR25:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE148:
	.size	records__ERCHAR25, .-records__ERCHAR25
	.align 4
	.type	records__FRCHAR25, #function
	.proc	020
records__FRCHAR25:
.LLFB149:
	save	%sp, -128, %sp
.LLCFI149:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	25, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE149:
	.size	records__FRCHAR25, .-records__FRCHAR25
	.align 4
	.type	records__ARCHAR26, #function
	.proc	014
records__ARCHAR26:
.LLFB150:
	save	%sp, -96, %sp
.LLCFI150:
	mov	%i0, %g1
	lduh	[%g1+12], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE150:
	.size	records__ARCHAR26, .-records__ARCHAR26
	.align 4
	.type	records__BRCHAR26, #function
	.proc	020
records__BRCHAR26:
.LLFB151:
	save	%sp, -128, %sp
.LLCFI151:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	26, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE151:
	.size	records__BRCHAR26, .-records__BRCHAR26
	.align 4
	.type	records__CRCHAR26, #function
	.proc	014
records__CRCHAR26:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+12], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE152:
	.size	records__CRCHAR26, .-records__CRCHAR26
	.align 4
	.type	records__DRCHAR26, #function
	.proc	020
records__DRCHAR26:
.LLFB153:
	save	%sp, -128, %sp
.LLCFI153:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	26, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE153:
	.size	records__DRCHAR26, .-records__DRCHAR26
	.align 4
	.type	records__ERCHAR26, #function
	.proc	014
records__ERCHAR26:
.LLFB154:
	save	%sp, -96, %sp
.LLCFI154:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+12], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE154:
	.size	records__ERCHAR26, .-records__ERCHAR26
	.align 4
	.type	records__FRCHAR26, #function
	.proc	020
records__FRCHAR26:
.LLFB155:
	save	%sp, -128, %sp
.LLCFI155:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	26, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE155:
	.size	records__FRCHAR26, .-records__FRCHAR26
	.align 4
	.type	records__ARCHAR27, #function
	.proc	014
records__ARCHAR27:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	mov	%i0, %g1
	ldub	[%g1+13], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	records__ARCHAR27, .-records__ARCHAR27
	.align 4
	.type	records__BRCHAR27, #function
	.proc	020
records__BRCHAR27:
.LLFB157:
	save	%sp, -128, %sp
.LLCFI157:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	27, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE157:
	.size	records__BRCHAR27, .-records__BRCHAR27
	.align 4
	.type	records__CRCHAR27, #function
	.proc	014
records__CRCHAR27:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+13], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	records__CRCHAR27, .-records__CRCHAR27
	.align 4
	.type	records__DRCHAR27, #function
	.proc	020
records__DRCHAR27:
.LLFB159:
	save	%sp, -128, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	27, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE159:
	.size	records__DRCHAR27, .-records__DRCHAR27
	.align 4
	.type	records__ERCHAR27, #function
	.proc	014
records__ERCHAR27:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+13], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	records__ERCHAR27, .-records__ERCHAR27
	.align 4
	.type	records__FRCHAR27, #function
	.proc	020
records__FRCHAR27:
.LLFB161:
	save	%sp, -128, %sp
.LLCFI161:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	27, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE161:
	.size	records__FRCHAR27, .-records__FRCHAR27
	.align 4
	.type	records__ARCHAR28, #function
	.proc	014
records__ARCHAR28:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	records__ARCHAR28, .-records__ARCHAR28
	.align 4
	.type	records__BRCHAR28, #function
	.proc	020
records__BRCHAR28:
.LLFB163:
	save	%sp, -128, %sp
.LLCFI163:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE163:
	.size	records__BRCHAR28, .-records__BRCHAR28
	.align 4
	.type	records__CRCHAR28, #function
	.proc	014
records__CRCHAR28:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	records__CRCHAR28, .-records__CRCHAR28
	.align 4
	.type	records__DRCHAR28, #function
	.proc	020
records__DRCHAR28:
.LLFB165:
	save	%sp, -128, %sp
.LLCFI165:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE165:
	.size	records__DRCHAR28, .-records__DRCHAR28
	.align 4
	.type	records__ERCHAR28, #function
	.proc	014
records__ERCHAR28:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	records__ERCHAR28, .-records__ERCHAR28
	.align 4
	.type	records__FRCHAR28, #function
	.proc	020
records__FRCHAR28:
.LLFB167:
	save	%sp, -128, %sp
.LLCFI167:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE167:
	.size	records__FRCHAR28, .-records__FRCHAR28
	.align 4
	.type	records__ARCHAR29, #function
	.proc	014
records__ARCHAR29:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	mov	%i0, %g1
	ldub	[%g1+14], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE168:
	.size	records__ARCHAR29, .-records__ARCHAR29
	.align 4
	.type	records__BRCHAR29, #function
	.proc	020
records__BRCHAR29:
.LLFB169:
	save	%sp, -128, %sp
.LLCFI169:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	29, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE169:
	.size	records__BRCHAR29, .-records__BRCHAR29
	.align 4
	.type	records__CRCHAR29, #function
	.proc	014
records__CRCHAR29:
.LLFB170:
	save	%sp, -96, %sp
.LLCFI170:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+14], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE170:
	.size	records__CRCHAR29, .-records__CRCHAR29
	.align 4
	.type	records__DRCHAR29, #function
	.proc	020
records__DRCHAR29:
.LLFB171:
	save	%sp, -128, %sp
.LLCFI171:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	29, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE171:
	.size	records__DRCHAR29, .-records__DRCHAR29
	.align 4
	.type	records__ERCHAR29, #function
	.proc	014
records__ERCHAR29:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+14], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	records__ERCHAR29, .-records__ERCHAR29
	.align 4
	.type	records__FRCHAR29, #function
	.proc	020
records__FRCHAR29:
.LLFB173:
	save	%sp, -128, %sp
.LLCFI173:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	29, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE173:
	.size	records__FRCHAR29, .-records__FRCHAR29
	.align 4
	.type	records__ARCHAR30, #function
	.proc	014
records__ARCHAR30:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	mov	%i0, %g1
	lduh	[%g1+14], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	records__ARCHAR30, .-records__ARCHAR30
	.align 4
	.type	records__BRCHAR30, #function
	.proc	020
records__BRCHAR30:
.LLFB175:
	save	%sp, -128, %sp
.LLCFI175:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	30, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE175:
	.size	records__BRCHAR30, .-records__BRCHAR30
	.align 4
	.type	records__CRCHAR30, #function
	.proc	014
records__CRCHAR30:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+14], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	records__CRCHAR30, .-records__CRCHAR30
	.align 4
	.type	records__DRCHAR30, #function
	.proc	020
records__DRCHAR30:
.LLFB177:
	save	%sp, -128, %sp
.LLCFI177:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	30, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE177:
	.size	records__DRCHAR30, .-records__DRCHAR30
	.align 4
	.type	records__ERCHAR30, #function
	.proc	014
records__ERCHAR30:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+14], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	records__ERCHAR30, .-records__ERCHAR30
	.align 4
	.type	records__FRCHAR30, #function
	.proc	020
records__FRCHAR30:
.LLFB179:
	save	%sp, -128, %sp
.LLCFI179:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	30, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE179:
	.size	records__FRCHAR30, .-records__FRCHAR30
	.align 4
	.type	records__ARCHAR31, #function
	.proc	014
records__ARCHAR31:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	mov	%i0, %g1
	ldub	[%g1+15], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE180:
	.size	records__ARCHAR31, .-records__ARCHAR31
	.align 4
	.type	records__BRCHAR31, #function
	.proc	020
records__BRCHAR31:
.LLFB181:
	save	%sp, -128, %sp
.LLCFI181:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	31, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE181:
	.size	records__BRCHAR31, .-records__BRCHAR31
	.align 4
	.type	records__CRCHAR31, #function
	.proc	014
records__CRCHAR31:
.LLFB182:
	save	%sp, -96, %sp
.LLCFI182:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+15], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE182:
	.size	records__CRCHAR31, .-records__CRCHAR31
	.align 4
	.type	records__DRCHAR31, #function
	.proc	020
records__DRCHAR31:
.LLFB183:
	save	%sp, -128, %sp
.LLCFI183:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	31, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE183:
	.size	records__DRCHAR31, .-records__DRCHAR31
	.align 4
	.type	records__ERCHAR31, #function
	.proc	014
records__ERCHAR31:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+15], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	records__ERCHAR31, .-records__ERCHAR31
	.align 4
	.type	records__FRCHAR31, #function
	.proc	020
records__FRCHAR31:
.LLFB185:
	save	%sp, -128, %sp
.LLCFI185:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	31, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE185:
	.size	records__FRCHAR31, .-records__FRCHAR31
	.align 4
	.type	records__ARCHAR32, #function
	.proc	014
records__ARCHAR32:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	records__ARCHAR32, .-records__ARCHAR32
	.align 4
	.type	records__BRCHAR32, #function
	.proc	020
records__BRCHAR32:
.LLFB187:
	save	%sp, -128, %sp
.LLCFI187:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE187:
	.size	records__BRCHAR32, .-records__BRCHAR32
	.align 4
	.type	records__CRCHAR32, #function
	.proc	014
records__CRCHAR32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	records__CRCHAR32, .-records__CRCHAR32
	.align 4
	.type	records__DRCHAR32, #function
	.proc	020
records__DRCHAR32:
.LLFB189:
	save	%sp, -128, %sp
.LLCFI189:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE189:
	.size	records__DRCHAR32, .-records__DRCHAR32
	.align 4
	.type	records__ERCHAR32, #function
	.proc	014
records__ERCHAR32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	records__ERCHAR32, .-records__ERCHAR32
	.align 4
	.type	records__FRCHAR32, #function
	.proc	020
records__FRCHAR32:
.LLFB191:
	save	%sp, -128, %sp
.LLCFI191:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE191:
	.size	records__FRCHAR32, .-records__FRCHAR32
	.align 4
	.type	records__ARCHAR33, #function
	.proc	014
records__ARCHAR33:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	mov	%i0, %g1
	ldub	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	records__ARCHAR33, .-records__ARCHAR33
	.align 4
	.type	records__BRCHAR33, #function
	.proc	020
records__BRCHAR33:
.LLFB193:
	save	%sp, -136, %sp
.LLCFI193:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	33, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE193:
	.size	records__BRCHAR33, .-records__BRCHAR33
	.align 4
	.type	records__CRCHAR33, #function
	.proc	014
records__CRCHAR33:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	records__CRCHAR33, .-records__CRCHAR33
	.align 4
	.type	records__DRCHAR33, #function
	.proc	020
records__DRCHAR33:
.LLFB195:
	save	%sp, -136, %sp
.LLCFI195:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	33, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE195:
	.size	records__DRCHAR33, .-records__DRCHAR33
	.align 4
	.type	records__ERCHAR33, #function
	.proc	014
records__ERCHAR33:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE196:
	.size	records__ERCHAR33, .-records__ERCHAR33
	.align 4
	.type	records__FRCHAR33, #function
	.proc	020
records__FRCHAR33:
.LLFB197:
	save	%sp, -136, %sp
.LLCFI197:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	33, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE197:
	.size	records__FRCHAR33, .-records__FRCHAR33
	.align 4
	.type	records__ARCHAR34, #function
	.proc	014
records__ARCHAR34:
.LLFB198:
	save	%sp, -96, %sp
.LLCFI198:
	mov	%i0, %g1
	lduh	[%g1+16], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE198:
	.size	records__ARCHAR34, .-records__ARCHAR34
	.align 4
	.type	records__BRCHAR34, #function
	.proc	020
records__BRCHAR34:
.LLFB199:
	save	%sp, -136, %sp
.LLCFI199:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	34, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE199:
	.size	records__BRCHAR34, .-records__BRCHAR34
	.align 4
	.type	records__CRCHAR34, #function
	.proc	014
records__CRCHAR34:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+16], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE200:
	.size	records__CRCHAR34, .-records__CRCHAR34
	.align 4
	.type	records__DRCHAR34, #function
	.proc	020
records__DRCHAR34:
.LLFB201:
	save	%sp, -136, %sp
.LLCFI201:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	34, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE201:
	.size	records__DRCHAR34, .-records__DRCHAR34
	.align 4
	.type	records__ERCHAR34, #function
	.proc	014
records__ERCHAR34:
.LLFB202:
	save	%sp, -96, %sp
.LLCFI202:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+16], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE202:
	.size	records__ERCHAR34, .-records__ERCHAR34
	.align 4
	.type	records__FRCHAR34, #function
	.proc	020
records__FRCHAR34:
.LLFB203:
	save	%sp, -136, %sp
.LLCFI203:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	34, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE203:
	.size	records__FRCHAR34, .-records__FRCHAR34
	.align 4
	.type	records__ARCHAR35, #function
	.proc	014
records__ARCHAR35:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	mov	%i0, %g1
	ldub	[%g1+17], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	records__ARCHAR35, .-records__ARCHAR35
	.align 4
	.type	records__BRCHAR35, #function
	.proc	020
records__BRCHAR35:
.LLFB205:
	save	%sp, -136, %sp
.LLCFI205:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	35, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE205:
	.size	records__BRCHAR35, .-records__BRCHAR35
	.align 4
	.type	records__CRCHAR35, #function
	.proc	014
records__CRCHAR35:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+17], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	records__CRCHAR35, .-records__CRCHAR35
	.align 4
	.type	records__DRCHAR35, #function
	.proc	020
records__DRCHAR35:
.LLFB207:
	save	%sp, -136, %sp
.LLCFI207:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	35, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE207:
	.size	records__DRCHAR35, .-records__DRCHAR35
	.align 4
	.type	records__ERCHAR35, #function
	.proc	014
records__ERCHAR35:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+17], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	records__ERCHAR35, .-records__ERCHAR35
	.align 4
	.type	records__FRCHAR35, #function
	.proc	020
records__FRCHAR35:
.LLFB209:
	save	%sp, -136, %sp
.LLCFI209:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	35, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE209:
	.size	records__FRCHAR35, .-records__FRCHAR35
	.align 4
	.type	records__ARCHAR36, #function
	.proc	014
records__ARCHAR36:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	records__ARCHAR36, .-records__ARCHAR36
	.align 4
	.type	records__BRCHAR36, #function
	.proc	020
records__BRCHAR36:
.LLFB211:
	save	%sp, -136, %sp
.LLCFI211:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE211:
	.size	records__BRCHAR36, .-records__BRCHAR36
	.align 4
	.type	records__CRCHAR36, #function
	.proc	014
records__CRCHAR36:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	records__CRCHAR36, .-records__CRCHAR36
	.align 4
	.type	records__DRCHAR36, #function
	.proc	020
records__DRCHAR36:
.LLFB213:
	save	%sp, -136, %sp
.LLCFI213:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE213:
	.size	records__DRCHAR36, .-records__DRCHAR36
	.align 4
	.type	records__ERCHAR36, #function
	.proc	014
records__ERCHAR36:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	records__ERCHAR36, .-records__ERCHAR36
	.align 4
	.type	records__FRCHAR36, #function
	.proc	020
records__FRCHAR36:
.LLFB215:
	save	%sp, -136, %sp
.LLCFI215:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE215:
	.size	records__FRCHAR36, .-records__FRCHAR36
	.align 4
	.type	records__ARCHAR37, #function
	.proc	014
records__ARCHAR37:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	mov	%i0, %g1
	ldub	[%g1+18], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE216:
	.size	records__ARCHAR37, .-records__ARCHAR37
	.align 4
	.type	records__BRCHAR37, #function
	.proc	020
records__BRCHAR37:
.LLFB217:
	save	%sp, -136, %sp
.LLCFI217:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	37, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE217:
	.size	records__BRCHAR37, .-records__BRCHAR37
	.align 4
	.type	records__CRCHAR37, #function
	.proc	014
records__CRCHAR37:
.LLFB218:
	save	%sp, -96, %sp
.LLCFI218:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+18], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE218:
	.size	records__CRCHAR37, .-records__CRCHAR37
	.align 4
	.type	records__DRCHAR37, #function
	.proc	020
records__DRCHAR37:
.LLFB219:
	save	%sp, -136, %sp
.LLCFI219:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	37, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE219:
	.size	records__DRCHAR37, .-records__DRCHAR37
	.align 4
	.type	records__ERCHAR37, #function
	.proc	014
records__ERCHAR37:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+18], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	records__ERCHAR37, .-records__ERCHAR37
	.align 4
	.type	records__FRCHAR37, #function
	.proc	020
records__FRCHAR37:
.LLFB221:
	save	%sp, -136, %sp
.LLCFI221:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	37, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE221:
	.size	records__FRCHAR37, .-records__FRCHAR37
	.align 4
	.type	records__ARCHAR38, #function
	.proc	014
records__ARCHAR38:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	mov	%i0, %g1
	lduh	[%g1+18], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	records__ARCHAR38, .-records__ARCHAR38
	.align 4
	.type	records__BRCHAR38, #function
	.proc	020
records__BRCHAR38:
.LLFB223:
	save	%sp, -136, %sp
.LLCFI223:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	38, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE223:
	.size	records__BRCHAR38, .-records__BRCHAR38
	.align 4
	.type	records__CRCHAR38, #function
	.proc	014
records__CRCHAR38:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+18], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	records__CRCHAR38, .-records__CRCHAR38
	.align 4
	.type	records__DRCHAR38, #function
	.proc	020
records__DRCHAR38:
.LLFB225:
	save	%sp, -136, %sp
.LLCFI225:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	38, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE225:
	.size	records__DRCHAR38, .-records__DRCHAR38
	.align 4
	.type	records__ERCHAR38, #function
	.proc	014
records__ERCHAR38:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+18], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	records__ERCHAR38, .-records__ERCHAR38
	.align 4
	.type	records__FRCHAR38, #function
	.proc	020
records__FRCHAR38:
.LLFB227:
	save	%sp, -136, %sp
.LLCFI227:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	38, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE227:
	.size	records__FRCHAR38, .-records__FRCHAR38
	.align 4
	.type	records__ARCHAR39, #function
	.proc	014
records__ARCHAR39:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	mov	%i0, %g1
	ldub	[%g1+19], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE228:
	.size	records__ARCHAR39, .-records__ARCHAR39
	.align 4
	.type	records__BRCHAR39, #function
	.proc	020
records__BRCHAR39:
.LLFB229:
	save	%sp, -136, %sp
.LLCFI229:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	39, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE229:
	.size	records__BRCHAR39, .-records__BRCHAR39
	.align 4
	.type	records__CRCHAR39, #function
	.proc	014
records__CRCHAR39:
.LLFB230:
	save	%sp, -96, %sp
.LLCFI230:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+19], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE230:
	.size	records__CRCHAR39, .-records__CRCHAR39
	.align 4
	.type	records__DRCHAR39, #function
	.proc	020
records__DRCHAR39:
.LLFB231:
	save	%sp, -136, %sp
.LLCFI231:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	39, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE231:
	.size	records__DRCHAR39, .-records__DRCHAR39
	.align 4
	.type	records__ERCHAR39, #function
	.proc	014
records__ERCHAR39:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+19], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	records__ERCHAR39, .-records__ERCHAR39
	.align 4
	.type	records__FRCHAR39, #function
	.proc	020
records__FRCHAR39:
.LLFB233:
	save	%sp, -136, %sp
.LLCFI233:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	39, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE233:
	.size	records__FRCHAR39, .-records__FRCHAR39
	.align 4
	.type	records__ARCHAR40, #function
	.proc	014
records__ARCHAR40:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	mov	%i0, %g1
	ld	[%g1+20], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	records__ARCHAR40, .-records__ARCHAR40
	.align 4
	.type	records__BRCHAR40, #function
	.proc	020
records__BRCHAR40:
.LLFB235:
	save	%sp, -136, %sp
.LLCFI235:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE235:
	.size	records__BRCHAR40, .-records__BRCHAR40
	.align 4
	.type	records__CRCHAR40, #function
	.proc	014
records__CRCHAR40:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+20], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	records__CRCHAR40, .-records__CRCHAR40
	.align 4
	.type	records__DRCHAR40, #function
	.proc	020
records__DRCHAR40:
.LLFB237:
	save	%sp, -136, %sp
.LLCFI237:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE237:
	.size	records__DRCHAR40, .-records__DRCHAR40
	.align 4
	.type	records__ERCHAR40, #function
	.proc	014
records__ERCHAR40:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+20], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	records__ERCHAR40, .-records__ERCHAR40
	.align 4
	.type	records__FRCHAR40, #function
	.proc	020
records__FRCHAR40:
.LLFB239:
	save	%sp, -136, %sp
.LLCFI239:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE239:
	.size	records__FRCHAR40, .-records__FRCHAR40
	.align 4
	.type	records__ARCHAR41, #function
	.proc	014
records__ARCHAR41:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	mov	%i0, %g1
	ldub	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	records__ARCHAR41, .-records__ARCHAR41
	.align 4
	.type	records__BRCHAR41, #function
	.proc	020
records__BRCHAR41:
.LLFB241:
	save	%sp, -144, %sp
.LLCFI241:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	41, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE241:
	.size	records__BRCHAR41, .-records__BRCHAR41
	.align 4
	.type	records__CRCHAR41, #function
	.proc	014
records__CRCHAR41:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	records__CRCHAR41, .-records__CRCHAR41
	.align 4
	.type	records__DRCHAR41, #function
	.proc	020
records__DRCHAR41:
.LLFB243:
	save	%sp, -144, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	41, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE243:
	.size	records__DRCHAR41, .-records__DRCHAR41
	.align 4
	.type	records__ERCHAR41, #function
	.proc	014
records__ERCHAR41:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE244:
	.size	records__ERCHAR41, .-records__ERCHAR41
	.align 4
	.type	records__FRCHAR41, #function
	.proc	020
records__FRCHAR41:
.LLFB245:
	save	%sp, -144, %sp
.LLCFI245:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	41, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE245:
	.size	records__FRCHAR41, .-records__FRCHAR41
	.align 4
	.type	records__ARCHAR42, #function
	.proc	014
records__ARCHAR42:
.LLFB246:
	save	%sp, -96, %sp
.LLCFI246:
	mov	%i0, %g1
	lduh	[%g1+20], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE246:
	.size	records__ARCHAR42, .-records__ARCHAR42
	.align 4
	.type	records__BRCHAR42, #function
	.proc	020
records__BRCHAR42:
.LLFB247:
	save	%sp, -144, %sp
.LLCFI247:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	42, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE247:
	.size	records__BRCHAR42, .-records__BRCHAR42
	.align 4
	.type	records__CRCHAR42, #function
	.proc	014
records__CRCHAR42:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+20], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE248:
	.size	records__CRCHAR42, .-records__CRCHAR42
	.align 4
	.type	records__DRCHAR42, #function
	.proc	020
records__DRCHAR42:
.LLFB249:
	save	%sp, -144, %sp
.LLCFI249:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	42, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE249:
	.size	records__DRCHAR42, .-records__DRCHAR42
	.align 4
	.type	records__ERCHAR42, #function
	.proc	014
records__ERCHAR42:
.LLFB250:
	save	%sp, -96, %sp
.LLCFI250:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+20], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE250:
	.size	records__ERCHAR42, .-records__ERCHAR42
	.align 4
	.type	records__FRCHAR42, #function
	.proc	020
records__FRCHAR42:
.LLFB251:
	save	%sp, -144, %sp
.LLCFI251:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	42, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE251:
	.size	records__FRCHAR42, .-records__FRCHAR42
	.align 4
	.type	records__ARCHAR43, #function
	.proc	014
records__ARCHAR43:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	mov	%i0, %g1
	ldub	[%g1+21], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	records__ARCHAR43, .-records__ARCHAR43
	.align 4
	.type	records__BRCHAR43, #function
	.proc	020
records__BRCHAR43:
.LLFB253:
	save	%sp, -144, %sp
.LLCFI253:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	43, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE253:
	.size	records__BRCHAR43, .-records__BRCHAR43
	.align 4
	.type	records__CRCHAR43, #function
	.proc	014
records__CRCHAR43:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+21], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	records__CRCHAR43, .-records__CRCHAR43
	.align 4
	.type	records__DRCHAR43, #function
	.proc	020
records__DRCHAR43:
.LLFB255:
	save	%sp, -144, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	43, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE255:
	.size	records__DRCHAR43, .-records__DRCHAR43
	.align 4
	.type	records__ERCHAR43, #function
	.proc	014
records__ERCHAR43:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+21], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	records__ERCHAR43, .-records__ERCHAR43
	.align 4
	.type	records__FRCHAR43, #function
	.proc	020
records__FRCHAR43:
.LLFB257:
	save	%sp, -144, %sp
.LLCFI257:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	43, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE257:
	.size	records__FRCHAR43, .-records__FRCHAR43
	.align 4
	.type	records__ARCHAR44, #function
	.proc	014
records__ARCHAR44:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	mov	%i0, %g1
	ld	[%g1+20], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	records__ARCHAR44, .-records__ARCHAR44
	.align 4
	.type	records__BRCHAR44, #function
	.proc	020
records__BRCHAR44:
.LLFB259:
	save	%sp, -144, %sp
.LLCFI259:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	44, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE259:
	.size	records__BRCHAR44, .-records__BRCHAR44
	.align 4
	.type	records__CRCHAR44, #function
	.proc	014
records__CRCHAR44:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+20], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	records__CRCHAR44, .-records__CRCHAR44
	.align 4
	.type	records__DRCHAR44, #function
	.proc	020
records__DRCHAR44:
.LLFB261:
	save	%sp, -144, %sp
.LLCFI261:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	44, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE261:
	.size	records__DRCHAR44, .-records__DRCHAR44
	.align 4
	.type	records__ERCHAR44, #function
	.proc	014
records__ERCHAR44:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+20], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	records__ERCHAR44, .-records__ERCHAR44
	.align 4
	.type	records__FRCHAR44, #function
	.proc	020
records__FRCHAR44:
.LLFB263:
	save	%sp, -144, %sp
.LLCFI263:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	44, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE263:
	.size	records__FRCHAR44, .-records__FRCHAR44
	.align 4
	.type	records__ARCHAR45, #function
	.proc	014
records__ARCHAR45:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	mov	%i0, %g1
	ldub	[%g1+22], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE264:
	.size	records__ARCHAR45, .-records__ARCHAR45
	.align 4
	.type	records__BRCHAR45, #function
	.proc	020
records__BRCHAR45:
.LLFB265:
	save	%sp, -144, %sp
.LLCFI265:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	45, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE265:
	.size	records__BRCHAR45, .-records__BRCHAR45
	.align 4
	.type	records__CRCHAR45, #function
	.proc	014
records__CRCHAR45:
.LLFB266:
	save	%sp, -96, %sp
.LLCFI266:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+22], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE266:
	.size	records__CRCHAR45, .-records__CRCHAR45
	.align 4
	.type	records__DRCHAR45, #function
	.proc	020
records__DRCHAR45:
.LLFB267:
	save	%sp, -144, %sp
.LLCFI267:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	45, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE267:
	.size	records__DRCHAR45, .-records__DRCHAR45
	.align 4
	.type	records__ERCHAR45, #function
	.proc	014
records__ERCHAR45:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+22], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	records__ERCHAR45, .-records__ERCHAR45
	.align 4
	.type	records__FRCHAR45, #function
	.proc	020
records__FRCHAR45:
.LLFB269:
	save	%sp, -144, %sp
.LLCFI269:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	45, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE269:
	.size	records__FRCHAR45, .-records__FRCHAR45
	.align 4
	.type	records__ARCHAR46, #function
	.proc	014
records__ARCHAR46:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	mov	%i0, %g1
	lduh	[%g1+22], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	records__ARCHAR46, .-records__ARCHAR46
	.align 4
	.type	records__BRCHAR46, #function
	.proc	020
records__BRCHAR46:
.LLFB271:
	save	%sp, -144, %sp
.LLCFI271:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	46, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE271:
	.size	records__BRCHAR46, .-records__BRCHAR46
	.align 4
	.type	records__CRCHAR46, #function
	.proc	014
records__CRCHAR46:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+22], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	records__CRCHAR46, .-records__CRCHAR46
	.align 4
	.type	records__DRCHAR46, #function
	.proc	020
records__DRCHAR46:
.LLFB273:
	save	%sp, -144, %sp
.LLCFI273:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	46, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE273:
	.size	records__DRCHAR46, .-records__DRCHAR46
	.align 4
	.type	records__ERCHAR46, #function
	.proc	014
records__ERCHAR46:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+22], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	records__ERCHAR46, .-records__ERCHAR46
	.align 4
	.type	records__FRCHAR46, #function
	.proc	020
records__FRCHAR46:
.LLFB275:
	save	%sp, -144, %sp
.LLCFI275:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	46, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE275:
	.size	records__FRCHAR46, .-records__FRCHAR46
	.align 4
	.type	records__ARCHAR47, #function
	.proc	014
records__ARCHAR47:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	mov	%i0, %g1
	ldub	[%g1+23], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE276:
	.size	records__ARCHAR47, .-records__ARCHAR47
	.align 4
	.type	records__BRCHAR47, #function
	.proc	020
records__BRCHAR47:
.LLFB277:
	save	%sp, -144, %sp
.LLCFI277:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	47, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE277:
	.size	records__BRCHAR47, .-records__BRCHAR47
	.align 4
	.type	records__CRCHAR47, #function
	.proc	014
records__CRCHAR47:
.LLFB278:
	save	%sp, -96, %sp
.LLCFI278:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+23], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE278:
	.size	records__CRCHAR47, .-records__CRCHAR47
	.align 4
	.type	records__DRCHAR47, #function
	.proc	020
records__DRCHAR47:
.LLFB279:
	save	%sp, -144, %sp
.LLCFI279:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	47, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE279:
	.size	records__DRCHAR47, .-records__DRCHAR47
	.align 4
	.type	records__ERCHAR47, #function
	.proc	014
records__ERCHAR47:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+23], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	records__ERCHAR47, .-records__ERCHAR47
	.align 4
	.type	records__FRCHAR47, #function
	.proc	020
records__FRCHAR47:
.LLFB281:
	save	%sp, -144, %sp
.LLCFI281:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	47, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE281:
	.size	records__FRCHAR47, .-records__FRCHAR47
	.align 4
	.type	records__ARCHAR48, #function
	.proc	014
records__ARCHAR48:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	mov	%i0, %g1
	ld	[%g1+24], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	records__ARCHAR48, .-records__ARCHAR48
	.align 4
	.type	records__BRCHAR48, #function
	.proc	020
records__BRCHAR48:
.LLFB283:
	save	%sp, -144, %sp
.LLCFI283:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE283:
	.size	records__BRCHAR48, .-records__BRCHAR48
	.align 4
	.type	records__CRCHAR48, #function
	.proc	014
records__CRCHAR48:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+24], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	records__CRCHAR48, .-records__CRCHAR48
	.align 4
	.type	records__DRCHAR48, #function
	.proc	020
records__DRCHAR48:
.LLFB285:
	save	%sp, -144, %sp
.LLCFI285:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE285:
	.size	records__DRCHAR48, .-records__DRCHAR48
	.align 4
	.type	records__ERCHAR48, #function
	.proc	014
records__ERCHAR48:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+24], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	records__ERCHAR48, .-records__ERCHAR48
	.align 4
	.type	records__FRCHAR48, #function
	.proc	020
records__FRCHAR48:
.LLFB287:
	save	%sp, -144, %sp
.LLCFI287:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE287:
	.size	records__FRCHAR48, .-records__FRCHAR48
	.align 4
	.type	records__ARCHAR49, #function
	.proc	014
records__ARCHAR49:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	mov	%i0, %g1
	ldub	[%g1+24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE288:
	.size	records__ARCHAR49, .-records__ARCHAR49
	.align 4
	.type	records__BRCHAR49, #function
	.proc	020
records__BRCHAR49:
.LLFB289:
	save	%sp, -152, %sp
.LLCFI289:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	49, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE289:
	.size	records__BRCHAR49, .-records__BRCHAR49
	.align 4
	.type	records__CRCHAR49, #function
	.proc	014
records__CRCHAR49:
.LLFB290:
	save	%sp, -96, %sp
.LLCFI290:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE290:
	.size	records__CRCHAR49, .-records__CRCHAR49
	.align 4
	.type	records__DRCHAR49, #function
	.proc	020
records__DRCHAR49:
.LLFB291:
	save	%sp, -152, %sp
.LLCFI291:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	49, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE291:
	.size	records__DRCHAR49, .-records__DRCHAR49
	.align 4
	.type	records__ERCHAR49, #function
	.proc	014
records__ERCHAR49:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+24], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE292:
	.size	records__ERCHAR49, .-records__ERCHAR49
	.align 4
	.type	records__FRCHAR49, #function
	.proc	020
records__FRCHAR49:
.LLFB293:
	save	%sp, -152, %sp
.LLCFI293:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	49, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE293:
	.size	records__FRCHAR49, .-records__FRCHAR49
	.align 4
	.type	records__ARCHAR50, #function
	.proc	014
records__ARCHAR50:
.LLFB294:
	save	%sp, -96, %sp
.LLCFI294:
	mov	%i0, %g1
	lduh	[%g1+24], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE294:
	.size	records__ARCHAR50, .-records__ARCHAR50
	.align 4
	.type	records__BRCHAR50, #function
	.proc	020
records__BRCHAR50:
.LLFB295:
	save	%sp, -152, %sp
.LLCFI295:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	50, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE295:
	.size	records__BRCHAR50, .-records__BRCHAR50
	.align 4
	.type	records__CRCHAR50, #function
	.proc	014
records__CRCHAR50:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+24], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE296:
	.size	records__CRCHAR50, .-records__CRCHAR50
	.align 4
	.type	records__DRCHAR50, #function
	.proc	020
records__DRCHAR50:
.LLFB297:
	save	%sp, -152, %sp
.LLCFI297:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	50, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE297:
	.size	records__DRCHAR50, .-records__DRCHAR50
	.align 4
	.type	records__ERCHAR50, #function
	.proc	014
records__ERCHAR50:
.LLFB298:
	save	%sp, -96, %sp
.LLCFI298:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+24], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE298:
	.size	records__ERCHAR50, .-records__ERCHAR50
	.align 4
	.type	records__FRCHAR50, #function
	.proc	020
records__FRCHAR50:
.LLFB299:
	save	%sp, -152, %sp
.LLCFI299:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	50, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE299:
	.size	records__FRCHAR50, .-records__FRCHAR50
	.align 4
	.type	records__ARCHAR51, #function
	.proc	014
records__ARCHAR51:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	mov	%i0, %g1
	ldub	[%g1+25], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE300:
	.size	records__ARCHAR51, .-records__ARCHAR51
	.align 4
	.type	records__BRCHAR51, #function
	.proc	020
records__BRCHAR51:
.LLFB301:
	save	%sp, -152, %sp
.LLCFI301:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	51, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE301:
	.size	records__BRCHAR51, .-records__BRCHAR51
	.align 4
	.type	records__CRCHAR51, #function
	.proc	014
records__CRCHAR51:
.LLFB302:
	save	%sp, -96, %sp
.LLCFI302:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+25], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE302:
	.size	records__CRCHAR51, .-records__CRCHAR51
	.align 4
	.type	records__DRCHAR51, #function
	.proc	020
records__DRCHAR51:
.LLFB303:
	save	%sp, -152, %sp
.LLCFI303:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	51, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE303:
	.size	records__DRCHAR51, .-records__DRCHAR51
	.align 4
	.type	records__ERCHAR51, #function
	.proc	014
records__ERCHAR51:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+25], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE304:
	.size	records__ERCHAR51, .-records__ERCHAR51
	.align 4
	.type	records__FRCHAR51, #function
	.proc	020
records__FRCHAR51:
.LLFB305:
	save	%sp, -152, %sp
.LLCFI305:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	51, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE305:
	.size	records__FRCHAR51, .-records__FRCHAR51
	.align 4
	.type	records__ARCHAR52, #function
	.proc	014
records__ARCHAR52:
.LLFB306:
	save	%sp, -96, %sp
.LLCFI306:
	mov	%i0, %g1
	ld	[%g1+24], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE306:
	.size	records__ARCHAR52, .-records__ARCHAR52
	.align 4
	.type	records__BRCHAR52, #function
	.proc	020
records__BRCHAR52:
.LLFB307:
	save	%sp, -152, %sp
.LLCFI307:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	52, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE307:
	.size	records__BRCHAR52, .-records__BRCHAR52
	.align 4
	.type	records__CRCHAR52, #function
	.proc	014
records__CRCHAR52:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+24], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE308:
	.size	records__CRCHAR52, .-records__CRCHAR52
	.align 4
	.type	records__DRCHAR52, #function
	.proc	020
records__DRCHAR52:
.LLFB309:
	save	%sp, -152, %sp
.LLCFI309:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	52, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE309:
	.size	records__DRCHAR52, .-records__DRCHAR52
	.align 4
	.type	records__ERCHAR52, #function
	.proc	014
records__ERCHAR52:
.LLFB310:
	save	%sp, -96, %sp
.LLCFI310:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+24], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE310:
	.size	records__ERCHAR52, .-records__ERCHAR52
	.align 4
	.type	records__FRCHAR52, #function
	.proc	020
records__FRCHAR52:
.LLFB311:
	save	%sp, -152, %sp
.LLCFI311:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	52, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE311:
	.size	records__FRCHAR52, .-records__FRCHAR52
	.align 4
	.type	records__ARCHAR53, #function
	.proc	014
records__ARCHAR53:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	mov	%i0, %g1
	ldub	[%g1+26], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE312:
	.size	records__ARCHAR53, .-records__ARCHAR53
	.align 4
	.type	records__BRCHAR53, #function
	.proc	020
records__BRCHAR53:
.LLFB313:
	save	%sp, -152, %sp
.LLCFI313:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	53, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE313:
	.size	records__BRCHAR53, .-records__BRCHAR53
	.align 4
	.type	records__CRCHAR53, #function
	.proc	014
records__CRCHAR53:
.LLFB314:
	save	%sp, -96, %sp
.LLCFI314:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+26], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE314:
	.size	records__CRCHAR53, .-records__CRCHAR53
	.align 4
	.type	records__DRCHAR53, #function
	.proc	020
records__DRCHAR53:
.LLFB315:
	save	%sp, -152, %sp
.LLCFI315:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	53, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE315:
	.size	records__DRCHAR53, .-records__DRCHAR53
	.align 4
	.type	records__ERCHAR53, #function
	.proc	014
records__ERCHAR53:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+26], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE316:
	.size	records__ERCHAR53, .-records__ERCHAR53
	.align 4
	.type	records__FRCHAR53, #function
	.proc	020
records__FRCHAR53:
.LLFB317:
	save	%sp, -152, %sp
.LLCFI317:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	53, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE317:
	.size	records__FRCHAR53, .-records__FRCHAR53
	.align 4
	.type	records__ARCHAR54, #function
	.proc	014
records__ARCHAR54:
.LLFB318:
	save	%sp, -96, %sp
.LLCFI318:
	mov	%i0, %g1
	lduh	[%g1+26], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE318:
	.size	records__ARCHAR54, .-records__ARCHAR54
	.align 4
	.type	records__BRCHAR54, #function
	.proc	020
records__BRCHAR54:
.LLFB319:
	save	%sp, -152, %sp
.LLCFI319:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	54, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE319:
	.size	records__BRCHAR54, .-records__BRCHAR54
	.align 4
	.type	records__CRCHAR54, #function
	.proc	014
records__CRCHAR54:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+26], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE320:
	.size	records__CRCHAR54, .-records__CRCHAR54
	.align 4
	.type	records__DRCHAR54, #function
	.proc	020
records__DRCHAR54:
.LLFB321:
	save	%sp, -152, %sp
.LLCFI321:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	54, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE321:
	.size	records__DRCHAR54, .-records__DRCHAR54
	.align 4
	.type	records__ERCHAR54, #function
	.proc	014
records__ERCHAR54:
.LLFB322:
	save	%sp, -96, %sp
.LLCFI322:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+26], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE322:
	.size	records__ERCHAR54, .-records__ERCHAR54
	.align 4
	.type	records__FRCHAR54, #function
	.proc	020
records__FRCHAR54:
.LLFB323:
	save	%sp, -152, %sp
.LLCFI323:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	54, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE323:
	.size	records__FRCHAR54, .-records__FRCHAR54
	.align 4
	.type	records__ARCHAR55, #function
	.proc	014
records__ARCHAR55:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	mov	%i0, %g1
	ldub	[%g1+27], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE324:
	.size	records__ARCHAR55, .-records__ARCHAR55
	.align 4
	.type	records__BRCHAR55, #function
	.proc	020
records__BRCHAR55:
.LLFB325:
	save	%sp, -152, %sp
.LLCFI325:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	55, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE325:
	.size	records__BRCHAR55, .-records__BRCHAR55
	.align 4
	.type	records__CRCHAR55, #function
	.proc	014
records__CRCHAR55:
.LLFB326:
	save	%sp, -96, %sp
.LLCFI326:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+27], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE326:
	.size	records__CRCHAR55, .-records__CRCHAR55
	.align 4
	.type	records__DRCHAR55, #function
	.proc	020
records__DRCHAR55:
.LLFB327:
	save	%sp, -152, %sp
.LLCFI327:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	55, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE327:
	.size	records__DRCHAR55, .-records__DRCHAR55
	.align 4
	.type	records__ERCHAR55, #function
	.proc	014
records__ERCHAR55:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+27], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE328:
	.size	records__ERCHAR55, .-records__ERCHAR55
	.align 4
	.type	records__FRCHAR55, #function
	.proc	020
records__FRCHAR55:
.LLFB329:
	save	%sp, -152, %sp
.LLCFI329:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	55, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE329:
	.size	records__FRCHAR55, .-records__FRCHAR55
	.align 4
	.type	records__ARCHAR56, #function
	.proc	014
records__ARCHAR56:
.LLFB330:
	save	%sp, -96, %sp
.LLCFI330:
	mov	%i0, %g1
	ld	[%g1+28], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE330:
	.size	records__ARCHAR56, .-records__ARCHAR56
	.align 4
	.type	records__BRCHAR56, #function
	.proc	020
records__BRCHAR56:
.LLFB331:
	save	%sp, -152, %sp
.LLCFI331:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE331:
	.size	records__BRCHAR56, .-records__BRCHAR56
	.align 4
	.type	records__CRCHAR56, #function
	.proc	014
records__CRCHAR56:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+28], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE332:
	.size	records__CRCHAR56, .-records__CRCHAR56
	.align 4
	.type	records__DRCHAR56, #function
	.proc	020
records__DRCHAR56:
.LLFB333:
	save	%sp, -152, %sp
.LLCFI333:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE333:
	.size	records__DRCHAR56, .-records__DRCHAR56
	.align 4
	.type	records__ERCHAR56, #function
	.proc	014
records__ERCHAR56:
.LLFB334:
	save	%sp, -96, %sp
.LLCFI334:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+28], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE334:
	.size	records__ERCHAR56, .-records__ERCHAR56
	.align 4
	.type	records__FRCHAR56, #function
	.proc	020
records__FRCHAR56:
.LLFB335:
	save	%sp, -152, %sp
.LLCFI335:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE335:
	.size	records__FRCHAR56, .-records__FRCHAR56
	.align 4
	.type	records__ARCHAR57, #function
	.proc	014
records__ARCHAR57:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	mov	%i0, %g1
	ldub	[%g1+28], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	records__ARCHAR57, .-records__ARCHAR57
	.align 4
	.type	records__BRCHAR57, #function
	.proc	020
records__BRCHAR57:
.LLFB337:
	save	%sp, -160, %sp
.LLCFI337:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	57, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE337:
	.size	records__BRCHAR57, .-records__BRCHAR57
	.align 4
	.type	records__CRCHAR57, #function
	.proc	014
records__CRCHAR57:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+28], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	records__CRCHAR57, .-records__CRCHAR57
	.align 4
	.type	records__DRCHAR57, #function
	.proc	020
records__DRCHAR57:
.LLFB339:
	save	%sp, -160, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	57, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE339:
	.size	records__DRCHAR57, .-records__DRCHAR57
	.align 4
	.type	records__ERCHAR57, #function
	.proc	014
records__ERCHAR57:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+28], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE340:
	.size	records__ERCHAR57, .-records__ERCHAR57
	.align 4
	.type	records__FRCHAR57, #function
	.proc	020
records__FRCHAR57:
.LLFB341:
	save	%sp, -160, %sp
.LLCFI341:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	57, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE341:
	.size	records__FRCHAR57, .-records__FRCHAR57
	.align 4
	.type	records__ARCHAR58, #function
	.proc	014
records__ARCHAR58:
.LLFB342:
	save	%sp, -96, %sp
.LLCFI342:
	mov	%i0, %g1
	lduh	[%g1+28], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE342:
	.size	records__ARCHAR58, .-records__ARCHAR58
	.align 4
	.type	records__BRCHAR58, #function
	.proc	020
records__BRCHAR58:
.LLFB343:
	save	%sp, -160, %sp
.LLCFI343:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	58, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE343:
	.size	records__BRCHAR58, .-records__BRCHAR58
	.align 4
	.type	records__CRCHAR58, #function
	.proc	014
records__CRCHAR58:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+28], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE344:
	.size	records__CRCHAR58, .-records__CRCHAR58
	.align 4
	.type	records__DRCHAR58, #function
	.proc	020
records__DRCHAR58:
.LLFB345:
	save	%sp, -160, %sp
.LLCFI345:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	58, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE345:
	.size	records__DRCHAR58, .-records__DRCHAR58
	.align 4
	.type	records__ERCHAR58, #function
	.proc	014
records__ERCHAR58:
.LLFB346:
	save	%sp, -96, %sp
.LLCFI346:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+28], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE346:
	.size	records__ERCHAR58, .-records__ERCHAR58
	.align 4
	.type	records__FRCHAR58, #function
	.proc	020
records__FRCHAR58:
.LLFB347:
	save	%sp, -160, %sp
.LLCFI347:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	58, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE347:
	.size	records__FRCHAR58, .-records__FRCHAR58
	.align 4
	.type	records__ARCHAR59, #function
	.proc	014
records__ARCHAR59:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	mov	%i0, %g1
	ldub	[%g1+29], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	records__ARCHAR59, .-records__ARCHAR59
	.align 4
	.type	records__BRCHAR59, #function
	.proc	020
records__BRCHAR59:
.LLFB349:
	save	%sp, -160, %sp
.LLCFI349:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	59, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE349:
	.size	records__BRCHAR59, .-records__BRCHAR59
	.align 4
	.type	records__CRCHAR59, #function
	.proc	014
records__CRCHAR59:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+29], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	records__CRCHAR59, .-records__CRCHAR59
	.align 4
	.type	records__DRCHAR59, #function
	.proc	020
records__DRCHAR59:
.LLFB351:
	save	%sp, -160, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	59, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE351:
	.size	records__DRCHAR59, .-records__DRCHAR59
	.align 4
	.type	records__ERCHAR59, #function
	.proc	014
records__ERCHAR59:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+29], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	records__ERCHAR59, .-records__ERCHAR59
	.align 4
	.type	records__FRCHAR59, #function
	.proc	020
records__FRCHAR59:
.LLFB353:
	save	%sp, -160, %sp
.LLCFI353:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	59, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE353:
	.size	records__FRCHAR59, .-records__FRCHAR59
	.align 4
	.type	records__ARCHAR60, #function
	.proc	014
records__ARCHAR60:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	mov	%i0, %g1
	ld	[%g1+28], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	records__ARCHAR60, .-records__ARCHAR60
	.align 4
	.type	records__BRCHAR60, #function
	.proc	020
records__BRCHAR60:
.LLFB355:
	save	%sp, -160, %sp
.LLCFI355:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	60, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE355:
	.size	records__BRCHAR60, .-records__BRCHAR60
	.align 4
	.type	records__CRCHAR60, #function
	.proc	014
records__CRCHAR60:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+28], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	records__CRCHAR60, .-records__CRCHAR60
	.align 4
	.type	records__DRCHAR60, #function
	.proc	020
records__DRCHAR60:
.LLFB357:
	save	%sp, -160, %sp
.LLCFI357:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	60, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE357:
	.size	records__DRCHAR60, .-records__DRCHAR60
	.align 4
	.type	records__ERCHAR60, #function
	.proc	014
records__ERCHAR60:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+28], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	records__ERCHAR60, .-records__ERCHAR60
	.align 4
	.type	records__FRCHAR60, #function
	.proc	020
records__FRCHAR60:
.LLFB359:
	save	%sp, -160, %sp
.LLCFI359:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	60, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE359:
	.size	records__FRCHAR60, .-records__FRCHAR60
	.align 4
	.type	records__ARCHAR61, #function
	.proc	014
records__ARCHAR61:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	mov	%i0, %g1
	ldub	[%g1+30], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE360:
	.size	records__ARCHAR61, .-records__ARCHAR61
	.align 4
	.type	records__BRCHAR61, #function
	.proc	020
records__BRCHAR61:
.LLFB361:
	save	%sp, -160, %sp
.LLCFI361:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	61, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE361:
	.size	records__BRCHAR61, .-records__BRCHAR61
	.align 4
	.type	records__CRCHAR61, #function
	.proc	014
records__CRCHAR61:
.LLFB362:
	save	%sp, -96, %sp
.LLCFI362:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+30], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE362:
	.size	records__CRCHAR61, .-records__CRCHAR61
	.align 4
	.type	records__DRCHAR61, #function
	.proc	020
records__DRCHAR61:
.LLFB363:
	save	%sp, -160, %sp
.LLCFI363:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	61, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE363:
	.size	records__DRCHAR61, .-records__DRCHAR61
	.align 4
	.type	records__ERCHAR61, #function
	.proc	014
records__ERCHAR61:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+30], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	records__ERCHAR61, .-records__ERCHAR61
	.align 4
	.type	records__FRCHAR61, #function
	.proc	020
records__FRCHAR61:
.LLFB365:
	save	%sp, -160, %sp
.LLCFI365:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	61, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE365:
	.size	records__FRCHAR61, .-records__FRCHAR61
	.align 4
	.type	records__ARCHAR62, #function
	.proc	014
records__ARCHAR62:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	mov	%i0, %g1
	lduh	[%g1+30], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	records__ARCHAR62, .-records__ARCHAR62
	.align 4
	.type	records__BRCHAR62, #function
	.proc	020
records__BRCHAR62:
.LLFB367:
	save	%sp, -160, %sp
.LLCFI367:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	62, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE367:
	.size	records__BRCHAR62, .-records__BRCHAR62
	.align 4
	.type	records__CRCHAR62, #function
	.proc	014
records__CRCHAR62:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+30], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	records__CRCHAR62, .-records__CRCHAR62
	.align 4
	.type	records__DRCHAR62, #function
	.proc	020
records__DRCHAR62:
.LLFB369:
	save	%sp, -160, %sp
.LLCFI369:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	62, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE369:
	.size	records__DRCHAR62, .-records__DRCHAR62
	.align 4
	.type	records__ERCHAR62, #function
	.proc	014
records__ERCHAR62:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+30], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	records__ERCHAR62, .-records__ERCHAR62
	.align 4
	.type	records__FRCHAR62, #function
	.proc	020
records__FRCHAR62:
.LLFB371:
	save	%sp, -160, %sp
.LLCFI371:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	62, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE371:
	.size	records__FRCHAR62, .-records__FRCHAR62
	.align 4
	.type	records__ARCHAR63, #function
	.proc	014
records__ARCHAR63:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	mov	%i0, %g1
	ldub	[%g1+31], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE372:
	.size	records__ARCHAR63, .-records__ARCHAR63
	.align 4
	.type	records__BRCHAR63, #function
	.proc	020
records__BRCHAR63:
.LLFB373:
	save	%sp, -160, %sp
.LLCFI373:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	63, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE373:
	.size	records__BRCHAR63, .-records__BRCHAR63
	.align 4
	.type	records__CRCHAR63, #function
	.proc	014
records__CRCHAR63:
.LLFB374:
	save	%sp, -96, %sp
.LLCFI374:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+31], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE374:
	.size	records__CRCHAR63, .-records__CRCHAR63
	.align 4
	.type	records__DRCHAR63, #function
	.proc	020
records__DRCHAR63:
.LLFB375:
	save	%sp, -160, %sp
.LLCFI375:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	63, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE375:
	.size	records__DRCHAR63, .-records__DRCHAR63
	.align 4
	.type	records__ERCHAR63, #function
	.proc	014
records__ERCHAR63:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+31], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	records__ERCHAR63, .-records__ERCHAR63
	.align 4
	.type	records__FRCHAR63, #function
	.proc	020
records__FRCHAR63:
.LLFB377:
	save	%sp, -160, %sp
.LLCFI377:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	63, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE377:
	.size	records__FRCHAR63, .-records__FRCHAR63
	.align 4
	.type	records__ARCHAR64, #function
	.proc	014
records__ARCHAR64:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	mov	%i0, %g1
	ld	[%g1+32], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	records__ARCHAR64, .-records__ARCHAR64
	.align 4
	.type	records__BRCHAR64, #function
	.proc	020
records__BRCHAR64:
.LLFB379:
	save	%sp, -160, %sp
.LLCFI379:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE379:
	.size	records__BRCHAR64, .-records__BRCHAR64
	.align 4
	.type	records__CRCHAR64, #function
	.proc	014
records__CRCHAR64:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+32], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	records__CRCHAR64, .-records__CRCHAR64
	.align 4
	.type	records__DRCHAR64, #function
	.proc	020
records__DRCHAR64:
.LLFB381:
	save	%sp, -160, %sp
.LLCFI381:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE381:
	.size	records__DRCHAR64, .-records__DRCHAR64
	.align 4
	.type	records__ERCHAR64, #function
	.proc	014
records__ERCHAR64:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+32], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	records__ERCHAR64, .-records__ERCHAR64
	.align 4
	.type	records__FRCHAR64, #function
	.proc	020
records__FRCHAR64:
.LLFB383:
	save	%sp, -160, %sp
.LLCFI383:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE383:
	.size	records__FRCHAR64, .-records__FRCHAR64
	.align 4
	.type	records__ARCHAR65, #function
	.proc	014
records__ARCHAR65:
.LLFB384:
	save	%sp, -96, %sp
.LLCFI384:
	mov	%i0, %g1
	ldub	[%g1+32], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE384:
	.size	records__ARCHAR65, .-records__ARCHAR65
	.align 4
	.type	records__BRCHAR65, #function
	.proc	020
records__BRCHAR65:
.LLFB385:
	save	%sp, -168, %sp
.LLCFI385:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	65, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE385:
	.size	records__BRCHAR65, .-records__BRCHAR65
	.align 4
	.type	records__CRCHAR65, #function
	.proc	014
records__CRCHAR65:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+32], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	records__CRCHAR65, .-records__CRCHAR65
	.align 4
	.type	records__DRCHAR65, #function
	.proc	020
records__DRCHAR65:
.LLFB387:
	save	%sp, -168, %sp
.LLCFI387:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	65, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE387:
	.size	records__DRCHAR65, .-records__DRCHAR65
	.align 4
	.type	records__ERCHAR65, #function
	.proc	014
records__ERCHAR65:
.LLFB388:
	save	%sp, -96, %sp
.LLCFI388:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+32], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	records__ERCHAR65, .-records__ERCHAR65
	.align 4
	.type	records__FRCHAR65, #function
	.proc	020
records__FRCHAR65:
.LLFB389:
	save	%sp, -168, %sp
.LLCFI389:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	65, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE389:
	.size	records__FRCHAR65, .-records__FRCHAR65
	.align 4
	.type	records__ARCHAR66, #function
	.proc	014
records__ARCHAR66:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	mov	%i0, %g1
	lduh	[%g1+32], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE390:
	.size	records__ARCHAR66, .-records__ARCHAR66
	.align 4
	.type	records__BRCHAR66, #function
	.proc	020
records__BRCHAR66:
.LLFB391:
	save	%sp, -168, %sp
.LLCFI391:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	66, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE391:
	.size	records__BRCHAR66, .-records__BRCHAR66
	.align 4
	.type	records__CRCHAR66, #function
	.proc	014
records__CRCHAR66:
.LLFB392:
	save	%sp, -96, %sp
.LLCFI392:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+32], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE392:
	.size	records__CRCHAR66, .-records__CRCHAR66
	.align 4
	.type	records__DRCHAR66, #function
	.proc	020
records__DRCHAR66:
.LLFB393:
	save	%sp, -168, %sp
.LLCFI393:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	66, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE393:
	.size	records__DRCHAR66, .-records__DRCHAR66
	.align 4
	.type	records__ERCHAR66, #function
	.proc	014
records__ERCHAR66:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+32], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE394:
	.size	records__ERCHAR66, .-records__ERCHAR66
	.align 4
	.type	records__FRCHAR66, #function
	.proc	020
records__FRCHAR66:
.LLFB395:
	save	%sp, -168, %sp
.LLCFI395:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	66, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE395:
	.size	records__FRCHAR66, .-records__FRCHAR66
	.align 4
	.type	records__ARCHAR67, #function
	.proc	014
records__ARCHAR67:
.LLFB396:
	save	%sp, -96, %sp
.LLCFI396:
	mov	%i0, %g1
	ldub	[%g1+33], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE396:
	.size	records__ARCHAR67, .-records__ARCHAR67
	.align 4
	.type	records__BRCHAR67, #function
	.proc	020
records__BRCHAR67:
.LLFB397:
	save	%sp, -168, %sp
.LLCFI397:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	67, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE397:
	.size	records__BRCHAR67, .-records__BRCHAR67
	.align 4
	.type	records__CRCHAR67, #function
	.proc	014
records__CRCHAR67:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+33], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	records__CRCHAR67, .-records__CRCHAR67
	.align 4
	.type	records__DRCHAR67, #function
	.proc	020
records__DRCHAR67:
.LLFB399:
	save	%sp, -168, %sp
.LLCFI399:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	67, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE399:
	.size	records__DRCHAR67, .-records__DRCHAR67
	.align 4
	.type	records__ERCHAR67, #function
	.proc	014
records__ERCHAR67:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+33], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	records__ERCHAR67, .-records__ERCHAR67
	.align 4
	.type	records__FRCHAR67, #function
	.proc	020
records__FRCHAR67:
.LLFB401:
	save	%sp, -168, %sp
.LLCFI401:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	67, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE401:
	.size	records__FRCHAR67, .-records__FRCHAR67
	.align 4
	.type	records__ARCHAR68, #function
	.proc	014
records__ARCHAR68:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	mov	%i0, %g1
	ld	[%g1+32], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	records__ARCHAR68, .-records__ARCHAR68
	.align 4
	.type	records__BRCHAR68, #function
	.proc	020
records__BRCHAR68:
.LLFB403:
	save	%sp, -168, %sp
.LLCFI403:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	68, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE403:
	.size	records__BRCHAR68, .-records__BRCHAR68
	.align 4
	.type	records__CRCHAR68, #function
	.proc	014
records__CRCHAR68:
.LLFB404:
	save	%sp, -96, %sp
.LLCFI404:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+32], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	records__CRCHAR68, .-records__CRCHAR68
	.align 4
	.type	records__DRCHAR68, #function
	.proc	020
records__DRCHAR68:
.LLFB405:
	save	%sp, -168, %sp
.LLCFI405:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	68, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE405:
	.size	records__DRCHAR68, .-records__DRCHAR68
	.align 4
	.type	records__ERCHAR68, #function
	.proc	014
records__ERCHAR68:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+32], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	records__ERCHAR68, .-records__ERCHAR68
	.align 4
	.type	records__FRCHAR68, #function
	.proc	020
records__FRCHAR68:
.LLFB407:
	save	%sp, -168, %sp
.LLCFI407:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	68, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE407:
	.size	records__FRCHAR68, .-records__FRCHAR68
	.align 4
	.type	records__ARCHAR69, #function
	.proc	014
records__ARCHAR69:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	mov	%i0, %g1
	ldub	[%g1+34], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	records__ARCHAR69, .-records__ARCHAR69
	.align 4
	.type	records__BRCHAR69, #function
	.proc	020
records__BRCHAR69:
.LLFB409:
	save	%sp, -168, %sp
.LLCFI409:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	69, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE409:
	.size	records__BRCHAR69, .-records__BRCHAR69
	.align 4
	.type	records__CRCHAR69, #function
	.proc	014
records__CRCHAR69:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+34], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE410:
	.size	records__CRCHAR69, .-records__CRCHAR69
	.align 4
	.type	records__DRCHAR69, #function
	.proc	020
records__DRCHAR69:
.LLFB411:
	save	%sp, -168, %sp
.LLCFI411:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	69, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE411:
	.size	records__DRCHAR69, .-records__DRCHAR69
	.align 4
	.type	records__ERCHAR69, #function
	.proc	014
records__ERCHAR69:
.LLFB412:
	save	%sp, -96, %sp
.LLCFI412:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+34], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE412:
	.size	records__ERCHAR69, .-records__ERCHAR69
	.align 4
	.type	records__FRCHAR69, #function
	.proc	020
records__FRCHAR69:
.LLFB413:
	save	%sp, -168, %sp
.LLCFI413:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	69, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE413:
	.size	records__FRCHAR69, .-records__FRCHAR69
	.align 4
	.type	records__ARCHAR70, #function
	.proc	014
records__ARCHAR70:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	mov	%i0, %g1
	lduh	[%g1+34], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	records__ARCHAR70, .-records__ARCHAR70
	.align 4
	.type	records__BRCHAR70, #function
	.proc	020
records__BRCHAR70:
.LLFB415:
	save	%sp, -168, %sp
.LLCFI415:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	70, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE415:
	.size	records__BRCHAR70, .-records__BRCHAR70
	.align 4
	.type	records__CRCHAR70, #function
	.proc	014
records__CRCHAR70:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+34], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	records__CRCHAR70, .-records__CRCHAR70
	.align 4
	.type	records__DRCHAR70, #function
	.proc	020
records__DRCHAR70:
.LLFB417:
	save	%sp, -168, %sp
.LLCFI417:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	70, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE417:
	.size	records__DRCHAR70, .-records__DRCHAR70
	.align 4
	.type	records__ERCHAR70, #function
	.proc	014
records__ERCHAR70:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+34], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	records__ERCHAR70, .-records__ERCHAR70
	.align 4
	.type	records__FRCHAR70, #function
	.proc	020
records__FRCHAR70:
.LLFB419:
	save	%sp, -168, %sp
.LLCFI419:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	70, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE419:
	.size	records__FRCHAR70, .-records__FRCHAR70
	.align 4
	.type	records__ARCHAR71, #function
	.proc	014
records__ARCHAR71:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	mov	%i0, %g1
	ldub	[%g1+35], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	records__ARCHAR71, .-records__ARCHAR71
	.align 4
	.type	records__BRCHAR71, #function
	.proc	020
records__BRCHAR71:
.LLFB421:
	save	%sp, -168, %sp
.LLCFI421:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	71, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE421:
	.size	records__BRCHAR71, .-records__BRCHAR71
	.align 4
	.type	records__CRCHAR71, #function
	.proc	014
records__CRCHAR71:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+35], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE422:
	.size	records__CRCHAR71, .-records__CRCHAR71
	.align 4
	.type	records__DRCHAR71, #function
	.proc	020
records__DRCHAR71:
.LLFB423:
	save	%sp, -168, %sp
.LLCFI423:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	71, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE423:
	.size	records__DRCHAR71, .-records__DRCHAR71
	.align 4
	.type	records__ERCHAR71, #function
	.proc	014
records__ERCHAR71:
.LLFB424:
	save	%sp, -96, %sp
.LLCFI424:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+35], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE424:
	.size	records__ERCHAR71, .-records__ERCHAR71
	.align 4
	.type	records__FRCHAR71, #function
	.proc	020
records__FRCHAR71:
.LLFB425:
	save	%sp, -168, %sp
.LLCFI425:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	71, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE425:
	.size	records__FRCHAR71, .-records__FRCHAR71
	.align 4
	.type	records__ARCHAR72, #function
	.proc	014
records__ARCHAR72:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	mov	%i0, %g1
	ld	[%g1+36], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	records__ARCHAR72, .-records__ARCHAR72
	.align 4
	.type	records__BRCHAR72, #function
	.proc	020
records__BRCHAR72:
.LLFB427:
	save	%sp, -168, %sp
.LLCFI427:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE427:
	.size	records__BRCHAR72, .-records__BRCHAR72
	.align 4
	.type	records__CRCHAR72, #function
	.proc	014
records__CRCHAR72:
.LLFB428:
	save	%sp, -96, %sp
.LLCFI428:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+36], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	records__CRCHAR72, .-records__CRCHAR72
	.align 4
	.type	records__DRCHAR72, #function
	.proc	020
records__DRCHAR72:
.LLFB429:
	save	%sp, -168, %sp
.LLCFI429:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE429:
	.size	records__DRCHAR72, .-records__DRCHAR72
	.align 4
	.type	records__ERCHAR72, #function
	.proc	014
records__ERCHAR72:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+36], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	records__ERCHAR72, .-records__ERCHAR72
	.align 4
	.type	records__FRCHAR72, #function
	.proc	020
records__FRCHAR72:
.LLFB431:
	save	%sp, -168, %sp
.LLCFI431:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE431:
	.size	records__FRCHAR72, .-records__FRCHAR72
	.align 4
	.type	records__ARCHAR73, #function
	.proc	014
records__ARCHAR73:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	mov	%i0, %g1
	ldub	[%g1+36], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	records__ARCHAR73, .-records__ARCHAR73
	.align 4
	.type	records__BRCHAR73, #function
	.proc	020
records__BRCHAR73:
.LLFB433:
	save	%sp, -176, %sp
.LLCFI433:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	73, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE433:
	.size	records__BRCHAR73, .-records__BRCHAR73
	.align 4
	.type	records__CRCHAR73, #function
	.proc	014
records__CRCHAR73:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+36], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE434:
	.size	records__CRCHAR73, .-records__CRCHAR73
	.align 4
	.type	records__DRCHAR73, #function
	.proc	020
records__DRCHAR73:
.LLFB435:
	save	%sp, -176, %sp
.LLCFI435:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	73, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE435:
	.size	records__DRCHAR73, .-records__DRCHAR73
	.align 4
	.type	records__ERCHAR73, #function
	.proc	014
records__ERCHAR73:
.LLFB436:
	save	%sp, -96, %sp
.LLCFI436:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+36], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE436:
	.size	records__ERCHAR73, .-records__ERCHAR73
	.align 4
	.type	records__FRCHAR73, #function
	.proc	020
records__FRCHAR73:
.LLFB437:
	save	%sp, -176, %sp
.LLCFI437:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	73, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE437:
	.size	records__FRCHAR73, .-records__FRCHAR73
	.align 4
	.type	records__ARCHAR74, #function
	.proc	014
records__ARCHAR74:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	mov	%i0, %g1
	lduh	[%g1+36], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE438:
	.size	records__ARCHAR74, .-records__ARCHAR74
	.align 4
	.type	records__BRCHAR74, #function
	.proc	020
records__BRCHAR74:
.LLFB439:
	save	%sp, -176, %sp
.LLCFI439:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	74, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE439:
	.size	records__BRCHAR74, .-records__BRCHAR74
	.align 4
	.type	records__CRCHAR74, #function
	.proc	014
records__CRCHAR74:
.LLFB440:
	save	%sp, -96, %sp
.LLCFI440:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+36], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE440:
	.size	records__CRCHAR74, .-records__CRCHAR74
	.align 4
	.type	records__DRCHAR74, #function
	.proc	020
records__DRCHAR74:
.LLFB441:
	save	%sp, -176, %sp
.LLCFI441:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	74, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE441:
	.size	records__DRCHAR74, .-records__DRCHAR74
	.align 4
	.type	records__ERCHAR74, #function
	.proc	014
records__ERCHAR74:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+36], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE442:
	.size	records__ERCHAR74, .-records__ERCHAR74
	.align 4
	.type	records__FRCHAR74, #function
	.proc	020
records__FRCHAR74:
.LLFB443:
	save	%sp, -176, %sp
.LLCFI443:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	74, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE443:
	.size	records__FRCHAR74, .-records__FRCHAR74
	.align 4
	.type	records__ARCHAR75, #function
	.proc	014
records__ARCHAR75:
.LLFB444:
	save	%sp, -96, %sp
.LLCFI444:
	mov	%i0, %g1
	ldub	[%g1+37], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE444:
	.size	records__ARCHAR75, .-records__ARCHAR75
	.align 4
	.type	records__BRCHAR75, #function
	.proc	020
records__BRCHAR75:
.LLFB445:
	save	%sp, -176, %sp
.LLCFI445:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	75, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE445:
	.size	records__BRCHAR75, .-records__BRCHAR75
	.align 4
	.type	records__CRCHAR75, #function
	.proc	014
records__CRCHAR75:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+37], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE446:
	.size	records__CRCHAR75, .-records__CRCHAR75
	.align 4
	.type	records__DRCHAR75, #function
	.proc	020
records__DRCHAR75:
.LLFB447:
	save	%sp, -176, %sp
.LLCFI447:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	75, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE447:
	.size	records__DRCHAR75, .-records__DRCHAR75
	.align 4
	.type	records__ERCHAR75, #function
	.proc	014
records__ERCHAR75:
.LLFB448:
	save	%sp, -96, %sp
.LLCFI448:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+37], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE448:
	.size	records__ERCHAR75, .-records__ERCHAR75
	.align 4
	.type	records__FRCHAR75, #function
	.proc	020
records__FRCHAR75:
.LLFB449:
	save	%sp, -176, %sp
.LLCFI449:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	75, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE449:
	.size	records__FRCHAR75, .-records__FRCHAR75
	.align 4
	.type	records__ARCHAR76, #function
	.proc	014
records__ARCHAR76:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	mov	%i0, %g1
	ld	[%g1+36], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE450:
	.size	records__ARCHAR76, .-records__ARCHAR76
	.align 4
	.type	records__BRCHAR76, #function
	.proc	020
records__BRCHAR76:
.LLFB451:
	save	%sp, -176, %sp
.LLCFI451:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	76, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE451:
	.size	records__BRCHAR76, .-records__BRCHAR76
	.align 4
	.type	records__CRCHAR76, #function
	.proc	014
records__CRCHAR76:
.LLFB452:
	save	%sp, -96, %sp
.LLCFI452:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+36], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE452:
	.size	records__CRCHAR76, .-records__CRCHAR76
	.align 4
	.type	records__DRCHAR76, #function
	.proc	020
records__DRCHAR76:
.LLFB453:
	save	%sp, -176, %sp
.LLCFI453:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	76, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE453:
	.size	records__DRCHAR76, .-records__DRCHAR76
	.align 4
	.type	records__ERCHAR76, #function
	.proc	014
records__ERCHAR76:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+36], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE454:
	.size	records__ERCHAR76, .-records__ERCHAR76
	.align 4
	.type	records__FRCHAR76, #function
	.proc	020
records__FRCHAR76:
.LLFB455:
	save	%sp, -176, %sp
.LLCFI455:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	76, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE455:
	.size	records__FRCHAR76, .-records__FRCHAR76
	.align 4
	.type	records__ARCHAR77, #function
	.proc	014
records__ARCHAR77:
.LLFB456:
	save	%sp, -96, %sp
.LLCFI456:
	mov	%i0, %g1
	ldub	[%g1+38], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE456:
	.size	records__ARCHAR77, .-records__ARCHAR77
	.align 4
	.type	records__BRCHAR77, #function
	.proc	020
records__BRCHAR77:
.LLFB457:
	save	%sp, -176, %sp
.LLCFI457:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	77, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE457:
	.size	records__BRCHAR77, .-records__BRCHAR77
	.align 4
	.type	records__CRCHAR77, #function
	.proc	014
records__CRCHAR77:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+38], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE458:
	.size	records__CRCHAR77, .-records__CRCHAR77
	.align 4
	.type	records__DRCHAR77, #function
	.proc	020
records__DRCHAR77:
.LLFB459:
	save	%sp, -176, %sp
.LLCFI459:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	77, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE459:
	.size	records__DRCHAR77, .-records__DRCHAR77
	.align 4
	.type	records__ERCHAR77, #function
	.proc	014
records__ERCHAR77:
.LLFB460:
	save	%sp, -96, %sp
.LLCFI460:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+38], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE460:
	.size	records__ERCHAR77, .-records__ERCHAR77
	.align 4
	.type	records__FRCHAR77, #function
	.proc	020
records__FRCHAR77:
.LLFB461:
	save	%sp, -176, %sp
.LLCFI461:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	77, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE461:
	.size	records__FRCHAR77, .-records__FRCHAR77
	.align 4
	.type	records__ARCHAR78, #function
	.proc	014
records__ARCHAR78:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	mov	%i0, %g1
	lduh	[%g1+38], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE462:
	.size	records__ARCHAR78, .-records__ARCHAR78
	.align 4
	.type	records__BRCHAR78, #function
	.proc	020
records__BRCHAR78:
.LLFB463:
	save	%sp, -176, %sp
.LLCFI463:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	78, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE463:
	.size	records__BRCHAR78, .-records__BRCHAR78
	.align 4
	.type	records__CRCHAR78, #function
	.proc	014
records__CRCHAR78:
.LLFB464:
	save	%sp, -96, %sp
.LLCFI464:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+38], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE464:
	.size	records__CRCHAR78, .-records__CRCHAR78
	.align 4
	.type	records__DRCHAR78, #function
	.proc	020
records__DRCHAR78:
.LLFB465:
	save	%sp, -176, %sp
.LLCFI465:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	78, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE465:
	.size	records__DRCHAR78, .-records__DRCHAR78
	.align 4
	.type	records__ERCHAR78, #function
	.proc	014
records__ERCHAR78:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+38], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE466:
	.size	records__ERCHAR78, .-records__ERCHAR78
	.align 4
	.type	records__FRCHAR78, #function
	.proc	020
records__FRCHAR78:
.LLFB467:
	save	%sp, -176, %sp
.LLCFI467:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	78, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE467:
	.size	records__FRCHAR78, .-records__FRCHAR78
	.align 4
	.type	records__ARCHAR79, #function
	.proc	014
records__ARCHAR79:
.LLFB468:
	save	%sp, -96, %sp
.LLCFI468:
	mov	%i0, %g1
	ldub	[%g1+39], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE468:
	.size	records__ARCHAR79, .-records__ARCHAR79
	.align 4
	.type	records__BRCHAR79, #function
	.proc	020
records__BRCHAR79:
.LLFB469:
	save	%sp, -176, %sp
.LLCFI469:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	79, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE469:
	.size	records__BRCHAR79, .-records__BRCHAR79
	.align 4
	.type	records__CRCHAR79, #function
	.proc	014
records__CRCHAR79:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+39], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE470:
	.size	records__CRCHAR79, .-records__CRCHAR79
	.align 4
	.type	records__DRCHAR79, #function
	.proc	020
records__DRCHAR79:
.LLFB471:
	save	%sp, -176, %sp
.LLCFI471:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	79, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE471:
	.size	records__DRCHAR79, .-records__DRCHAR79
	.align 4
	.type	records__ERCHAR79, #function
	.proc	014
records__ERCHAR79:
.LLFB472:
	save	%sp, -96, %sp
.LLCFI472:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+39], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE472:
	.size	records__ERCHAR79, .-records__ERCHAR79
	.align 4
	.type	records__FRCHAR79, #function
	.proc	020
records__FRCHAR79:
.LLFB473:
	save	%sp, -176, %sp
.LLCFI473:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	79, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE473:
	.size	records__FRCHAR79, .-records__FRCHAR79
	.align 4
	.type	records__ARCHAR80, #function
	.proc	014
records__ARCHAR80:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	mov	%i0, %g1
	ld	[%g1+40], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE474:
	.size	records__ARCHAR80, .-records__ARCHAR80
	.align 4
	.type	records__BRCHAR80, #function
	.proc	020
records__BRCHAR80:
.LLFB475:
	save	%sp, -176, %sp
.LLCFI475:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE475:
	.size	records__BRCHAR80, .-records__BRCHAR80
	.align 4
	.type	records__CRCHAR80, #function
	.proc	014
records__CRCHAR80:
.LLFB476:
	save	%sp, -96, %sp
.LLCFI476:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+40], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE476:
	.size	records__CRCHAR80, .-records__CRCHAR80
	.align 4
	.type	records__DRCHAR80, #function
	.proc	020
records__DRCHAR80:
.LLFB477:
	save	%sp, -176, %sp
.LLCFI477:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE477:
	.size	records__DRCHAR80, .-records__DRCHAR80
	.align 4
	.type	records__ERCHAR80, #function
	.proc	014
records__ERCHAR80:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+40], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE478:
	.size	records__ERCHAR80, .-records__ERCHAR80
	.align 4
	.type	records__FRCHAR80, #function
	.proc	020
records__FRCHAR80:
.LLFB479:
	save	%sp, -176, %sp
.LLCFI479:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE479:
	.size	records__FRCHAR80, .-records__FRCHAR80
	.align 4
	.type	records__ARCHAR81, #function
	.proc	014
records__ARCHAR81:
.LLFB480:
	save	%sp, -96, %sp
.LLCFI480:
	mov	%i0, %g1
	ldub	[%g1+40], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE480:
	.size	records__ARCHAR81, .-records__ARCHAR81
	.align 4
	.type	records__BRCHAR81, #function
	.proc	020
records__BRCHAR81:
.LLFB481:
	save	%sp, -184, %sp
.LLCFI481:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	81, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE481:
	.size	records__BRCHAR81, .-records__BRCHAR81
	.align 4
	.type	records__CRCHAR81, #function
	.proc	014
records__CRCHAR81:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+40], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	records__CRCHAR81, .-records__CRCHAR81
	.align 4
	.type	records__DRCHAR81, #function
	.proc	020
records__DRCHAR81:
.LLFB483:
	save	%sp, -184, %sp
.LLCFI483:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	81, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE483:
	.size	records__DRCHAR81, .-records__DRCHAR81
	.align 4
	.type	records__ERCHAR81, #function
	.proc	014
records__ERCHAR81:
.LLFB484:
	save	%sp, -96, %sp
.LLCFI484:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+40], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	records__ERCHAR81, .-records__ERCHAR81
	.align 4
	.type	records__FRCHAR81, #function
	.proc	020
records__FRCHAR81:
.LLFB485:
	save	%sp, -184, %sp
.LLCFI485:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	81, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE485:
	.size	records__FRCHAR81, .-records__FRCHAR81
	.align 4
	.type	records__ARCHAR82, #function
	.proc	014
records__ARCHAR82:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	mov	%i0, %g1
	lduh	[%g1+40], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE486:
	.size	records__ARCHAR82, .-records__ARCHAR82
	.align 4
	.type	records__BRCHAR82, #function
	.proc	020
records__BRCHAR82:
.LLFB487:
	save	%sp, -184, %sp
.LLCFI487:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	82, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE487:
	.size	records__BRCHAR82, .-records__BRCHAR82
	.align 4
	.type	records__CRCHAR82, #function
	.proc	014
records__CRCHAR82:
.LLFB488:
	save	%sp, -96, %sp
.LLCFI488:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+40], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE488:
	.size	records__CRCHAR82, .-records__CRCHAR82
	.align 4
	.type	records__DRCHAR82, #function
	.proc	020
records__DRCHAR82:
.LLFB489:
	save	%sp, -184, %sp
.LLCFI489:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	82, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE489:
	.size	records__DRCHAR82, .-records__DRCHAR82
	.align 4
	.type	records__ERCHAR82, #function
	.proc	014
records__ERCHAR82:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+40], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE490:
	.size	records__ERCHAR82, .-records__ERCHAR82
	.align 4
	.type	records__FRCHAR82, #function
	.proc	020
records__FRCHAR82:
.LLFB491:
	save	%sp, -184, %sp
.LLCFI491:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	82, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE491:
	.size	records__FRCHAR82, .-records__FRCHAR82
	.align 4
	.type	records__ARCHAR83, #function
	.proc	014
records__ARCHAR83:
.LLFB492:
	save	%sp, -96, %sp
.LLCFI492:
	mov	%i0, %g1
	ldub	[%g1+41], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE492:
	.size	records__ARCHAR83, .-records__ARCHAR83
	.align 4
	.type	records__BRCHAR83, #function
	.proc	020
records__BRCHAR83:
.LLFB493:
	save	%sp, -184, %sp
.LLCFI493:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	83, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE493:
	.size	records__BRCHAR83, .-records__BRCHAR83
	.align 4
	.type	records__CRCHAR83, #function
	.proc	014
records__CRCHAR83:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+41], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	records__CRCHAR83, .-records__CRCHAR83
	.align 4
	.type	records__DRCHAR83, #function
	.proc	020
records__DRCHAR83:
.LLFB495:
	save	%sp, -184, %sp
.LLCFI495:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	83, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE495:
	.size	records__DRCHAR83, .-records__DRCHAR83
	.align 4
	.type	records__ERCHAR83, #function
	.proc	014
records__ERCHAR83:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+41], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	records__ERCHAR83, .-records__ERCHAR83
	.align 4
	.type	records__FRCHAR83, #function
	.proc	020
records__FRCHAR83:
.LLFB497:
	save	%sp, -184, %sp
.LLCFI497:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	83, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE497:
	.size	records__FRCHAR83, .-records__FRCHAR83
	.align 4
	.type	records__ARCHAR84, #function
	.proc	014
records__ARCHAR84:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	mov	%i0, %g1
	ld	[%g1+40], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	records__ARCHAR84, .-records__ARCHAR84
	.align 4
	.type	records__BRCHAR84, #function
	.proc	020
records__BRCHAR84:
.LLFB499:
	save	%sp, -184, %sp
.LLCFI499:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	84, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE499:
	.size	records__BRCHAR84, .-records__BRCHAR84
	.align 4
	.type	records__CRCHAR84, #function
	.proc	014
records__CRCHAR84:
.LLFB500:
	save	%sp, -96, %sp
.LLCFI500:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+40], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	records__CRCHAR84, .-records__CRCHAR84
	.align 4
	.type	records__DRCHAR84, #function
	.proc	020
records__DRCHAR84:
.LLFB501:
	save	%sp, -184, %sp
.LLCFI501:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	84, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE501:
	.size	records__DRCHAR84, .-records__DRCHAR84
	.align 4
	.type	records__ERCHAR84, #function
	.proc	014
records__ERCHAR84:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+40], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	records__ERCHAR84, .-records__ERCHAR84
	.align 4
	.type	records__FRCHAR84, #function
	.proc	020
records__FRCHAR84:
.LLFB503:
	save	%sp, -184, %sp
.LLCFI503:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	84, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE503:
	.size	records__FRCHAR84, .-records__FRCHAR84
	.align 4
	.type	records__ARCHAR85, #function
	.proc	014
records__ARCHAR85:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	mov	%i0, %g1
	ldub	[%g1+42], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	records__ARCHAR85, .-records__ARCHAR85
	.align 4
	.type	records__BRCHAR85, #function
	.proc	020
records__BRCHAR85:
.LLFB505:
	save	%sp, -184, %sp
.LLCFI505:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	85, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE505:
	.size	records__BRCHAR85, .-records__BRCHAR85
	.align 4
	.type	records__CRCHAR85, #function
	.proc	014
records__CRCHAR85:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+42], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE506:
	.size	records__CRCHAR85, .-records__CRCHAR85
	.align 4
	.type	records__DRCHAR85, #function
	.proc	020
records__DRCHAR85:
.LLFB507:
	save	%sp, -184, %sp
.LLCFI507:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	85, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE507:
	.size	records__DRCHAR85, .-records__DRCHAR85
	.align 4
	.type	records__ERCHAR85, #function
	.proc	014
records__ERCHAR85:
.LLFB508:
	save	%sp, -96, %sp
.LLCFI508:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+42], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE508:
	.size	records__ERCHAR85, .-records__ERCHAR85
	.align 4
	.type	records__FRCHAR85, #function
	.proc	020
records__FRCHAR85:
.LLFB509:
	save	%sp, -184, %sp
.LLCFI509:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	85, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE509:
	.size	records__FRCHAR85, .-records__FRCHAR85
	.align 4
	.type	records__ARCHAR86, #function
	.proc	014
records__ARCHAR86:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	mov	%i0, %g1
	lduh	[%g1+42], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	records__ARCHAR86, .-records__ARCHAR86
	.align 4
	.type	records__BRCHAR86, #function
	.proc	020
records__BRCHAR86:
.LLFB511:
	save	%sp, -184, %sp
.LLCFI511:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	86, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE511:
	.size	records__BRCHAR86, .-records__BRCHAR86
	.align 4
	.type	records__CRCHAR86, #function
	.proc	014
records__CRCHAR86:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+42], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	records__CRCHAR86, .-records__CRCHAR86
	.align 4
	.type	records__DRCHAR86, #function
	.proc	020
records__DRCHAR86:
.LLFB513:
	save	%sp, -184, %sp
.LLCFI513:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	86, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE513:
	.size	records__DRCHAR86, .-records__DRCHAR86
	.align 4
	.type	records__ERCHAR86, #function
	.proc	014
records__ERCHAR86:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+42], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	records__ERCHAR86, .-records__ERCHAR86
	.align 4
	.type	records__FRCHAR86, #function
	.proc	020
records__FRCHAR86:
.LLFB515:
	save	%sp, -184, %sp
.LLCFI515:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	86, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE515:
	.size	records__FRCHAR86, .-records__FRCHAR86
	.align 4
	.type	records__ARCHAR87, #function
	.proc	014
records__ARCHAR87:
.LLFB516:
	save	%sp, -96, %sp
.LLCFI516:
	mov	%i0, %g1
	ldub	[%g1+43], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	records__ARCHAR87, .-records__ARCHAR87
	.align 4
	.type	records__BRCHAR87, #function
	.proc	020
records__BRCHAR87:
.LLFB517:
	save	%sp, -184, %sp
.LLCFI517:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	87, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE517:
	.size	records__BRCHAR87, .-records__BRCHAR87
	.align 4
	.type	records__CRCHAR87, #function
	.proc	014
records__CRCHAR87:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+43], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE518:
	.size	records__CRCHAR87, .-records__CRCHAR87
	.align 4
	.type	records__DRCHAR87, #function
	.proc	020
records__DRCHAR87:
.LLFB519:
	save	%sp, -184, %sp
.LLCFI519:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	87, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE519:
	.size	records__DRCHAR87, .-records__DRCHAR87
	.align 4
	.type	records__ERCHAR87, #function
	.proc	014
records__ERCHAR87:
.LLFB520:
	save	%sp, -96, %sp
.LLCFI520:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+43], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE520:
	.size	records__ERCHAR87, .-records__ERCHAR87
	.align 4
	.type	records__FRCHAR87, #function
	.proc	020
records__FRCHAR87:
.LLFB521:
	save	%sp, -184, %sp
.LLCFI521:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	87, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE521:
	.size	records__FRCHAR87, .-records__FRCHAR87
	.align 4
	.type	records__ARCHAR88, #function
	.proc	014
records__ARCHAR88:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	mov	%i0, %g1
	ld	[%g1+44], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	records__ARCHAR88, .-records__ARCHAR88
	.align 4
	.type	records__BRCHAR88, #function
	.proc	020
records__BRCHAR88:
.LLFB523:
	save	%sp, -184, %sp
.LLCFI523:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	88, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE523:
	.size	records__BRCHAR88, .-records__BRCHAR88
	.align 4
	.type	records__CRCHAR88, #function
	.proc	014
records__CRCHAR88:
.LLFB524:
	save	%sp, -96, %sp
.LLCFI524:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+44], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	records__CRCHAR88, .-records__CRCHAR88
	.align 4
	.type	records__DRCHAR88, #function
	.proc	020
records__DRCHAR88:
.LLFB525:
	save	%sp, -184, %sp
.LLCFI525:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	88, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE525:
	.size	records__DRCHAR88, .-records__DRCHAR88
	.align 4
	.type	records__ERCHAR88, #function
	.proc	014
records__ERCHAR88:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+44], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	records__ERCHAR88, .-records__ERCHAR88
	.align 4
	.type	records__FRCHAR88, #function
	.proc	020
records__FRCHAR88:
.LLFB527:
	save	%sp, -184, %sp
.LLCFI527:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	88, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE527:
	.size	records__FRCHAR88, .-records__FRCHAR88
	.align 4
	.type	records__ARCHAR89, #function
	.proc	014
records__ARCHAR89:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	mov	%i0, %g1
	ldub	[%g1+44], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	records__ARCHAR89, .-records__ARCHAR89
	.align 4
	.type	records__BRCHAR89, #function
	.proc	020
records__BRCHAR89:
.LLFB529:
	save	%sp, -192, %sp
.LLCFI529:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	89, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE529:
	.size	records__BRCHAR89, .-records__BRCHAR89
	.align 4
	.type	records__CRCHAR89, #function
	.proc	014
records__CRCHAR89:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+44], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	records__CRCHAR89, .-records__CRCHAR89
	.align 4
	.type	records__DRCHAR89, #function
	.proc	020
records__DRCHAR89:
.LLFB531:
	save	%sp, -192, %sp
.LLCFI531:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	89, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE531:
	.size	records__DRCHAR89, .-records__DRCHAR89
	.align 4
	.type	records__ERCHAR89, #function
	.proc	014
records__ERCHAR89:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+44], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	records__ERCHAR89, .-records__ERCHAR89
	.align 4
	.type	records__FRCHAR89, #function
	.proc	020
records__FRCHAR89:
.LLFB533:
	save	%sp, -192, %sp
.LLCFI533:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	89, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE533:
	.size	records__FRCHAR89, .-records__FRCHAR89
	.align 4
	.type	records__ARCHAR90, #function
	.proc	014
records__ARCHAR90:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	mov	%i0, %g1
	lduh	[%g1+44], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE534:
	.size	records__ARCHAR90, .-records__ARCHAR90
	.align 4
	.type	records__BRCHAR90, #function
	.proc	020
records__BRCHAR90:
.LLFB535:
	save	%sp, -192, %sp
.LLCFI535:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	90, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE535:
	.size	records__BRCHAR90, .-records__BRCHAR90
	.align 4
	.type	records__CRCHAR90, #function
	.proc	014
records__CRCHAR90:
.LLFB536:
	save	%sp, -96, %sp
.LLCFI536:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+44], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE536:
	.size	records__CRCHAR90, .-records__CRCHAR90
	.align 4
	.type	records__DRCHAR90, #function
	.proc	020
records__DRCHAR90:
.LLFB537:
	save	%sp, -192, %sp
.LLCFI537:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	90, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE537:
	.size	records__DRCHAR90, .-records__DRCHAR90
	.align 4
	.type	records__ERCHAR90, #function
	.proc	014
records__ERCHAR90:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+44], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE538:
	.size	records__ERCHAR90, .-records__ERCHAR90
	.align 4
	.type	records__FRCHAR90, #function
	.proc	020
records__FRCHAR90:
.LLFB539:
	save	%sp, -192, %sp
.LLCFI539:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	90, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE539:
	.size	records__FRCHAR90, .-records__FRCHAR90
	.align 4
	.type	records__ARCHAR91, #function
	.proc	014
records__ARCHAR91:
.LLFB540:
	save	%sp, -96, %sp
.LLCFI540:
	mov	%i0, %g1
	ldub	[%g1+45], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE540:
	.size	records__ARCHAR91, .-records__ARCHAR91
	.align 4
	.type	records__BRCHAR91, #function
	.proc	020
records__BRCHAR91:
.LLFB541:
	save	%sp, -192, %sp
.LLCFI541:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	91, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE541:
	.size	records__BRCHAR91, .-records__BRCHAR91
	.align 4
	.type	records__CRCHAR91, #function
	.proc	014
records__CRCHAR91:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+45], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	records__CRCHAR91, .-records__CRCHAR91
	.align 4
	.type	records__DRCHAR91, #function
	.proc	020
records__DRCHAR91:
.LLFB543:
	save	%sp, -192, %sp
.LLCFI543:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	91, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE543:
	.size	records__DRCHAR91, .-records__DRCHAR91
	.align 4
	.type	records__ERCHAR91, #function
	.proc	014
records__ERCHAR91:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+45], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	records__ERCHAR91, .-records__ERCHAR91
	.align 4
	.type	records__FRCHAR91, #function
	.proc	020
records__FRCHAR91:
.LLFB545:
	save	%sp, -192, %sp
.LLCFI545:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	91, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE545:
	.size	records__FRCHAR91, .-records__FRCHAR91
	.align 4
	.type	records__ARCHAR92, #function
	.proc	014
records__ARCHAR92:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	mov	%i0, %g1
	ld	[%g1+44], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	records__ARCHAR92, .-records__ARCHAR92
	.align 4
	.type	records__BRCHAR92, #function
	.proc	020
records__BRCHAR92:
.LLFB547:
	save	%sp, -192, %sp
.LLCFI547:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	92, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE547:
	.size	records__BRCHAR92, .-records__BRCHAR92
	.align 4
	.type	records__CRCHAR92, #function
	.proc	014
records__CRCHAR92:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+44], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	records__CRCHAR92, .-records__CRCHAR92
	.align 4
	.type	records__DRCHAR92, #function
	.proc	020
records__DRCHAR92:
.LLFB549:
	save	%sp, -192, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	92, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE549:
	.size	records__DRCHAR92, .-records__DRCHAR92
	.align 4
	.type	records__ERCHAR92, #function
	.proc	014
records__ERCHAR92:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+44], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	records__ERCHAR92, .-records__ERCHAR92
	.align 4
	.type	records__FRCHAR92, #function
	.proc	020
records__FRCHAR92:
.LLFB551:
	save	%sp, -192, %sp
.LLCFI551:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	92, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE551:
	.size	records__FRCHAR92, .-records__FRCHAR92
	.align 4
	.type	records__ARCHAR93, #function
	.proc	014
records__ARCHAR93:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	mov	%i0, %g1
	ldub	[%g1+46], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	records__ARCHAR93, .-records__ARCHAR93
	.align 4
	.type	records__BRCHAR93, #function
	.proc	020
records__BRCHAR93:
.LLFB553:
	save	%sp, -192, %sp
.LLCFI553:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	93, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE553:
	.size	records__BRCHAR93, .-records__BRCHAR93
	.align 4
	.type	records__CRCHAR93, #function
	.proc	014
records__CRCHAR93:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+46], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE554:
	.size	records__CRCHAR93, .-records__CRCHAR93
	.align 4
	.type	records__DRCHAR93, #function
	.proc	020
records__DRCHAR93:
.LLFB555:
	save	%sp, -192, %sp
.LLCFI555:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	93, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE555:
	.size	records__DRCHAR93, .-records__DRCHAR93
	.align 4
	.type	records__ERCHAR93, #function
	.proc	014
records__ERCHAR93:
.LLFB556:
	save	%sp, -96, %sp
.LLCFI556:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+46], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE556:
	.size	records__ERCHAR93, .-records__ERCHAR93
	.align 4
	.type	records__FRCHAR93, #function
	.proc	020
records__FRCHAR93:
.LLFB557:
	save	%sp, -192, %sp
.LLCFI557:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	93, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE557:
	.size	records__FRCHAR93, .-records__FRCHAR93
	.align 4
	.type	records__ARCHAR94, #function
	.proc	014
records__ARCHAR94:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	mov	%i0, %g1
	lduh	[%g1+46], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	records__ARCHAR94, .-records__ARCHAR94
	.align 4
	.type	records__BRCHAR94, #function
	.proc	020
records__BRCHAR94:
.LLFB559:
	save	%sp, -192, %sp
.LLCFI559:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	94, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE559:
	.size	records__BRCHAR94, .-records__BRCHAR94
	.align 4
	.type	records__CRCHAR94, #function
	.proc	014
records__CRCHAR94:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+46], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	records__CRCHAR94, .-records__CRCHAR94
	.align 4
	.type	records__DRCHAR94, #function
	.proc	020
records__DRCHAR94:
.LLFB561:
	save	%sp, -192, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	94, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE561:
	.size	records__DRCHAR94, .-records__DRCHAR94
	.align 4
	.type	records__ERCHAR94, #function
	.proc	014
records__ERCHAR94:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+46], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	records__ERCHAR94, .-records__ERCHAR94
	.align 4
	.type	records__FRCHAR94, #function
	.proc	020
records__FRCHAR94:
.LLFB563:
	save	%sp, -192, %sp
.LLCFI563:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	94, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE563:
	.size	records__FRCHAR94, .-records__FRCHAR94
	.align 4
	.type	records__ARCHAR95, #function
	.proc	014
records__ARCHAR95:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	mov	%i0, %g1
	ldub	[%g1+47], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	records__ARCHAR95, .-records__ARCHAR95
	.align 4
	.type	records__BRCHAR95, #function
	.proc	020
records__BRCHAR95:
.LLFB565:
	save	%sp, -192, %sp
.LLCFI565:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	95, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE565:
	.size	records__BRCHAR95, .-records__BRCHAR95
	.align 4
	.type	records__CRCHAR95, #function
	.proc	014
records__CRCHAR95:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+47], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE566:
	.size	records__CRCHAR95, .-records__CRCHAR95
	.align 4
	.type	records__DRCHAR95, #function
	.proc	020
records__DRCHAR95:
.LLFB567:
	save	%sp, -192, %sp
.LLCFI567:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	95, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE567:
	.size	records__DRCHAR95, .-records__DRCHAR95
	.align 4
	.type	records__ERCHAR95, #function
	.proc	014
records__ERCHAR95:
.LLFB568:
	save	%sp, -96, %sp
.LLCFI568:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+47], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE568:
	.size	records__ERCHAR95, .-records__ERCHAR95
	.align 4
	.type	records__FRCHAR95, #function
	.proc	020
records__FRCHAR95:
.LLFB569:
	save	%sp, -192, %sp
.LLCFI569:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	95, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE569:
	.size	records__FRCHAR95, .-records__FRCHAR95
	.align 4
	.type	records__ARCHAR96, #function
	.proc	014
records__ARCHAR96:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	mov	%i0, %g1
	ld	[%g1+48], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	records__ARCHAR96, .-records__ARCHAR96
	.align 4
	.type	records__BRCHAR96, #function
	.proc	020
records__BRCHAR96:
.LLFB571:
	save	%sp, -192, %sp
.LLCFI571:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	96, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE571:
	.size	records__BRCHAR96, .-records__BRCHAR96
	.align 4
	.type	records__CRCHAR96, #function
	.proc	014
records__CRCHAR96:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+48], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	records__CRCHAR96, .-records__CRCHAR96
	.align 4
	.type	records__DRCHAR96, #function
	.proc	020
records__DRCHAR96:
.LLFB573:
	save	%sp, -192, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	96, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE573:
	.size	records__DRCHAR96, .-records__DRCHAR96
	.align 4
	.type	records__ERCHAR96, #function
	.proc	014
records__ERCHAR96:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+48], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	records__ERCHAR96, .-records__ERCHAR96
	.align 4
	.type	records__FRCHAR96, #function
	.proc	020
records__FRCHAR96:
.LLFB575:
	save	%sp, -192, %sp
.LLCFI575:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	96, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE575:
	.size	records__FRCHAR96, .-records__FRCHAR96
	.align 4
	.type	records__ARCHAR97, #function
	.proc	014
records__ARCHAR97:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	mov	%i0, %g1
	ldub	[%g1+48], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	records__ARCHAR97, .-records__ARCHAR97
	.align 4
	.type	records__BRCHAR97, #function
	.proc	020
records__BRCHAR97:
.LLFB577:
	save	%sp, -200, %sp
.LLCFI577:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	97, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE577:
	.size	records__BRCHAR97, .-records__BRCHAR97
	.align 4
	.type	records__CRCHAR97, #function
	.proc	014
records__CRCHAR97:
.LLFB578:
	save	%sp, -96, %sp
.LLCFI578:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+48], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE578:
	.size	records__CRCHAR97, .-records__CRCHAR97
	.align 4
	.type	records__DRCHAR97, #function
	.proc	020
records__DRCHAR97:
.LLFB579:
	save	%sp, -200, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	97, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE579:
	.size	records__DRCHAR97, .-records__DRCHAR97
	.align 4
	.type	records__ERCHAR97, #function
	.proc	014
records__ERCHAR97:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+48], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	records__ERCHAR97, .-records__ERCHAR97
	.align 4
	.type	records__FRCHAR97, #function
	.proc	020
records__FRCHAR97:
.LLFB581:
	save	%sp, -200, %sp
.LLCFI581:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	97, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE581:
	.size	records__FRCHAR97, .-records__FRCHAR97
	.align 4
	.type	records__ARCHAR98, #function
	.proc	014
records__ARCHAR98:
.LLFB582:
	save	%sp, -96, %sp
.LLCFI582:
	mov	%i0, %g1
	lduh	[%g1+48], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE582:
	.size	records__ARCHAR98, .-records__ARCHAR98
	.align 4
	.type	records__BRCHAR98, #function
	.proc	020
records__BRCHAR98:
.LLFB583:
	save	%sp, -200, %sp
.LLCFI583:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	98, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE583:
	.size	records__BRCHAR98, .-records__BRCHAR98
	.align 4
	.type	records__CRCHAR98, #function
	.proc	014
records__CRCHAR98:
.LLFB584:
	save	%sp, -96, %sp
.LLCFI584:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+48], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE584:
	.size	records__CRCHAR98, .-records__CRCHAR98
	.align 4
	.type	records__DRCHAR98, #function
	.proc	020
records__DRCHAR98:
.LLFB585:
	save	%sp, -200, %sp
.LLCFI585:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	98, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE585:
	.size	records__DRCHAR98, .-records__DRCHAR98
	.align 4
	.type	records__ERCHAR98, #function
	.proc	014
records__ERCHAR98:
.LLFB586:
	save	%sp, -96, %sp
.LLCFI586:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+48], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE586:
	.size	records__ERCHAR98, .-records__ERCHAR98
	.align 4
	.type	records__FRCHAR98, #function
	.proc	020
records__FRCHAR98:
.LLFB587:
	save	%sp, -200, %sp
.LLCFI587:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	98, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE587:
	.size	records__FRCHAR98, .-records__FRCHAR98
	.align 4
	.type	records__ARCHAR99, #function
	.proc	014
records__ARCHAR99:
.LLFB588:
	save	%sp, -96, %sp
.LLCFI588:
	mov	%i0, %g1
	ldub	[%g1+49], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE588:
	.size	records__ARCHAR99, .-records__ARCHAR99
	.align 4
	.type	records__BRCHAR99, #function
	.proc	020
records__BRCHAR99:
.LLFB589:
	save	%sp, -200, %sp
.LLCFI589:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	99, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE589:
	.size	records__BRCHAR99, .-records__BRCHAR99
	.align 4
	.type	records__CRCHAR99, #function
	.proc	014
records__CRCHAR99:
.LLFB590:
	save	%sp, -96, %sp
.LLCFI590:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+49], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE590:
	.size	records__CRCHAR99, .-records__CRCHAR99
	.align 4
	.type	records__DRCHAR99, #function
	.proc	020
records__DRCHAR99:
.LLFB591:
	save	%sp, -200, %sp
.LLCFI591:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	99, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE591:
	.size	records__DRCHAR99, .-records__DRCHAR99
	.align 4
	.type	records__ERCHAR99, #function
	.proc	014
records__ERCHAR99:
.LLFB592:
	save	%sp, -96, %sp
.LLCFI592:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+49], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE592:
	.size	records__ERCHAR99, .-records__ERCHAR99
	.align 4
	.type	records__FRCHAR99, #function
	.proc	020
records__FRCHAR99:
.LLFB593:
	save	%sp, -200, %sp
.LLCFI593:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	99, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE593:
	.size	records__FRCHAR99, .-records__FRCHAR99
	.align 4
	.type	records__ARCHAR100, #function
	.proc	014
records__ARCHAR100:
.LLFB594:
	save	%sp, -96, %sp
.LLCFI594:
	mov	%i0, %g1
	ld	[%g1+48], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE594:
	.size	records__ARCHAR100, .-records__ARCHAR100
	.align 4
	.type	records__BRCHAR100, #function
	.proc	020
records__BRCHAR100:
.LLFB595:
	save	%sp, -200, %sp
.LLCFI595:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	100, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE595:
	.size	records__BRCHAR100, .-records__BRCHAR100
	.align 4
	.type	records__CRCHAR100, #function
	.proc	014
records__CRCHAR100:
.LLFB596:
	save	%sp, -96, %sp
.LLCFI596:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+48], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE596:
	.size	records__CRCHAR100, .-records__CRCHAR100
	.align 4
	.type	records__DRCHAR100, #function
	.proc	020
records__DRCHAR100:
.LLFB597:
	save	%sp, -200, %sp
.LLCFI597:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	100, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE597:
	.size	records__DRCHAR100, .-records__DRCHAR100
	.align 4
	.type	records__ERCHAR100, #function
	.proc	014
records__ERCHAR100:
.LLFB598:
	save	%sp, -96, %sp
.LLCFI598:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+48], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE598:
	.size	records__ERCHAR100, .-records__ERCHAR100
	.align 4
	.type	records__FRCHAR100, #function
	.proc	020
records__FRCHAR100:
.LLFB599:
	save	%sp, -200, %sp
.LLCFI599:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	100, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE599:
	.size	records__FRCHAR100, .-records__FRCHAR100
	.align 4
	.type	records__ARBOOLEAN1, #function
	.proc	014
records__ARBOOLEAN1:
.LLFB600:
	save	%sp, -96, %sp
.LLCFI600:
	mov	%i0, %g1
	ldub	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE600:
	.size	records__ARBOOLEAN1, .-records__ARBOOLEAN1
	.align 4
	.type	records__BRBOOLEAN1, #function
	.proc	020
records__BRBOOLEAN1:
.LLFB601:
	save	%sp, -104, %sp
.LLCFI601:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	1, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE601:
	.size	records__BRBOOLEAN1, .-records__BRBOOLEAN1
	.align 4
	.type	records__CRBOOLEAN1, #function
	.proc	014
records__CRBOOLEAN1:
.LLFB602:
	save	%sp, -96, %sp
.LLCFI602:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE602:
	.size	records__CRBOOLEAN1, .-records__CRBOOLEAN1
	.align 4
	.type	records__DRBOOLEAN1, #function
	.proc	020
records__DRBOOLEAN1:
.LLFB603:
	save	%sp, -104, %sp
.LLCFI603:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	1, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE603:
	.size	records__DRBOOLEAN1, .-records__DRBOOLEAN1
	.align 4
	.type	records__ERBOOLEAN1, #function
	.proc	014
records__ERBOOLEAN1:
.LLFB604:
	save	%sp, -96, %sp
.LLCFI604:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE604:
	.size	records__ERBOOLEAN1, .-records__ERBOOLEAN1
	.align 4
	.type	records__FRBOOLEAN1, #function
	.proc	020
records__FRBOOLEAN1:
.LLFB605:
	save	%sp, -104, %sp
.LLCFI605:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	1, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE605:
	.size	records__FRBOOLEAN1, .-records__FRBOOLEAN1
	.align 4
	.type	records__ARBOOLEAN2, #function
	.proc	014
records__ARBOOLEAN2:
.LLFB606:
	save	%sp, -96, %sp
.LLCFI606:
	mov	%i0, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE606:
	.size	records__ARBOOLEAN2, .-records__ARBOOLEAN2
	.align 4
	.type	records__BRBOOLEAN2, #function
	.proc	020
records__BRBOOLEAN2:
.LLFB607:
	save	%sp, -104, %sp
.LLCFI607:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE607:
	.size	records__BRBOOLEAN2, .-records__BRBOOLEAN2
	.align 4
	.type	records__CRBOOLEAN2, #function
	.proc	014
records__CRBOOLEAN2:
.LLFB608:
	save	%sp, -96, %sp
.LLCFI608:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE608:
	.size	records__CRBOOLEAN2, .-records__CRBOOLEAN2
	.align 4
	.type	records__DRBOOLEAN2, #function
	.proc	020
records__DRBOOLEAN2:
.LLFB609:
	save	%sp, -104, %sp
.LLCFI609:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE609:
	.size	records__DRBOOLEAN2, .-records__DRBOOLEAN2
	.align 4
	.type	records__ERBOOLEAN2, #function
	.proc	014
records__ERBOOLEAN2:
.LLFB610:
	save	%sp, -96, %sp
.LLCFI610:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE610:
	.size	records__ERBOOLEAN2, .-records__ERBOOLEAN2
	.align 4
	.type	records__FRBOOLEAN2, #function
	.proc	020
records__FRBOOLEAN2:
.LLFB611:
	save	%sp, -104, %sp
.LLCFI611:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE611:
	.size	records__FRBOOLEAN2, .-records__FRBOOLEAN2
	.align 4
	.type	records__ARBOOLEAN3, #function
	.proc	014
records__ARBOOLEAN3:
.LLFB612:
	save	%sp, -96, %sp
.LLCFI612:
	mov	%i0, %g1
	ldub	[%g1+1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE612:
	.size	records__ARBOOLEAN3, .-records__ARBOOLEAN3
	.align 4
	.type	records__BRBOOLEAN3, #function
	.proc	020
records__BRBOOLEAN3:
.LLFB613:
	save	%sp, -104, %sp
.LLCFI613:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	3, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE613:
	.size	records__BRBOOLEAN3, .-records__BRBOOLEAN3
	.align 4
	.type	records__CRBOOLEAN3, #function
	.proc	014
records__CRBOOLEAN3:
.LLFB614:
	save	%sp, -96, %sp
.LLCFI614:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE614:
	.size	records__CRBOOLEAN3, .-records__CRBOOLEAN3
	.align 4
	.type	records__DRBOOLEAN3, #function
	.proc	020
records__DRBOOLEAN3:
.LLFB615:
	save	%sp, -104, %sp
.LLCFI615:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	3, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE615:
	.size	records__DRBOOLEAN3, .-records__DRBOOLEAN3
	.align 4
	.type	records__ERBOOLEAN3, #function
	.proc	014
records__ERBOOLEAN3:
.LLFB616:
	save	%sp, -96, %sp
.LLCFI616:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE616:
	.size	records__ERBOOLEAN3, .-records__ERBOOLEAN3
	.align 4
	.type	records__FRBOOLEAN3, #function
	.proc	020
records__FRBOOLEAN3:
.LLFB617:
	save	%sp, -104, %sp
.LLCFI617:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	3, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE617:
	.size	records__FRBOOLEAN3, .-records__FRBOOLEAN3
	.align 4
	.type	records__ARBOOLEAN4, #function
	.proc	014
records__ARBOOLEAN4:
.LLFB618:
	save	%sp, -96, %sp
.LLCFI618:
	mov	%i0, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE618:
	.size	records__ARBOOLEAN4, .-records__ARBOOLEAN4
	.align 4
	.type	records__BRBOOLEAN4, #function
	.proc	020
records__BRBOOLEAN4:
.LLFB619:
	save	%sp, -104, %sp
.LLCFI619:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE619:
	.size	records__BRBOOLEAN4, .-records__BRBOOLEAN4
	.align 4
	.type	records__CRBOOLEAN4, #function
	.proc	014
records__CRBOOLEAN4:
.LLFB620:
	save	%sp, -96, %sp
.LLCFI620:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE620:
	.size	records__CRBOOLEAN4, .-records__CRBOOLEAN4
	.align 4
	.type	records__DRBOOLEAN4, #function
	.proc	020
records__DRBOOLEAN4:
.LLFB621:
	save	%sp, -104, %sp
.LLCFI621:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE621:
	.size	records__DRBOOLEAN4, .-records__DRBOOLEAN4
	.align 4
	.type	records__ERBOOLEAN4, #function
	.proc	014
records__ERBOOLEAN4:
.LLFB622:
	save	%sp, -96, %sp
.LLCFI622:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	srl	%g1, 8, %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE622:
	.size	records__ERBOOLEAN4, .-records__ERBOOLEAN4
	.align 4
	.type	records__FRBOOLEAN4, #function
	.proc	020
records__FRBOOLEAN4:
.LLFB623:
	save	%sp, -104, %sp
.LLCFI623:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE623:
	.size	records__FRBOOLEAN4, .-records__FRBOOLEAN4
	.align 4
	.type	records__ARBOOLEAN5, #function
	.proc	014
records__ARBOOLEAN5:
.LLFB624:
	save	%sp, -96, %sp
.LLCFI624:
	mov	%i0, %g1
	ldub	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE624:
	.size	records__ARBOOLEAN5, .-records__ARBOOLEAN5
	.align 4
	.type	records__BRBOOLEAN5, #function
	.proc	020
records__BRBOOLEAN5:
.LLFB625:
	save	%sp, -104, %sp
.LLCFI625:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	5, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE625:
	.size	records__BRBOOLEAN5, .-records__BRBOOLEAN5
	.align 4
	.type	records__CRBOOLEAN5, #function
	.proc	014
records__CRBOOLEAN5:
.LLFB626:
	save	%sp, -96, %sp
.LLCFI626:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE626:
	.size	records__CRBOOLEAN5, .-records__CRBOOLEAN5
	.align 4
	.type	records__DRBOOLEAN5, #function
	.proc	020
records__DRBOOLEAN5:
.LLFB627:
	save	%sp, -104, %sp
.LLCFI627:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	5, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE627:
	.size	records__DRBOOLEAN5, .-records__DRBOOLEAN5
	.align 4
	.type	records__ERBOOLEAN5, #function
	.proc	014
records__ERBOOLEAN5:
.LLFB628:
	save	%sp, -96, %sp
.LLCFI628:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE628:
	.size	records__ERBOOLEAN5, .-records__ERBOOLEAN5
	.align 4
	.type	records__FRBOOLEAN5, #function
	.proc	020
records__FRBOOLEAN5:
.LLFB629:
	save	%sp, -104, %sp
.LLCFI629:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	5, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE629:
	.size	records__FRBOOLEAN5, .-records__FRBOOLEAN5
	.align 4
	.type	records__ARBOOLEAN6, #function
	.proc	014
records__ARBOOLEAN6:
.LLFB630:
	save	%sp, -96, %sp
.LLCFI630:
	mov	%i0, %g1
	lduh	[%g1+2], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE630:
	.size	records__ARBOOLEAN6, .-records__ARBOOLEAN6
	.align 4
	.type	records__BRBOOLEAN6, #function
	.proc	020
records__BRBOOLEAN6:
.LLFB631:
	save	%sp, -104, %sp
.LLCFI631:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE631:
	.size	records__BRBOOLEAN6, .-records__BRBOOLEAN6
	.align 4
	.type	records__CRBOOLEAN6, #function
	.proc	014
records__CRBOOLEAN6:
.LLFB632:
	save	%sp, -96, %sp
.LLCFI632:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+2], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE632:
	.size	records__CRBOOLEAN6, .-records__CRBOOLEAN6
	.align 4
	.type	records__DRBOOLEAN6, #function
	.proc	020
records__DRBOOLEAN6:
.LLFB633:
	save	%sp, -104, %sp
.LLCFI633:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE633:
	.size	records__DRBOOLEAN6, .-records__DRBOOLEAN6
	.align 4
	.type	records__ERBOOLEAN6, #function
	.proc	014
records__ERBOOLEAN6:
.LLFB634:
	save	%sp, -96, %sp
.LLCFI634:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+2], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE634:
	.size	records__ERBOOLEAN6, .-records__ERBOOLEAN6
	.align 4
	.type	records__FRBOOLEAN6, #function
	.proc	020
records__FRBOOLEAN6:
.LLFB635:
	save	%sp, -104, %sp
.LLCFI635:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE635:
	.size	records__FRBOOLEAN6, .-records__FRBOOLEAN6
	.align 4
	.type	records__ARBOOLEAN7, #function
	.proc	014
records__ARBOOLEAN7:
.LLFB636:
	save	%sp, -96, %sp
.LLCFI636:
	mov	%i0, %g1
	ldub	[%g1+3], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE636:
	.size	records__ARBOOLEAN7, .-records__ARBOOLEAN7
	.align 4
	.type	records__BRBOOLEAN7, #function
	.proc	020
records__BRBOOLEAN7:
.LLFB637:
	save	%sp, -104, %sp
.LLCFI637:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	7, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE637:
	.size	records__BRBOOLEAN7, .-records__BRBOOLEAN7
	.align 4
	.type	records__CRBOOLEAN7, #function
	.proc	014
records__CRBOOLEAN7:
.LLFB638:
	save	%sp, -96, %sp
.LLCFI638:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+3], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE638:
	.size	records__CRBOOLEAN7, .-records__CRBOOLEAN7
	.align 4
	.type	records__DRBOOLEAN7, #function
	.proc	020
records__DRBOOLEAN7:
.LLFB639:
	save	%sp, -104, %sp
.LLCFI639:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	7, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE639:
	.size	records__DRBOOLEAN7, .-records__DRBOOLEAN7
	.align 4
	.type	records__ERBOOLEAN7, #function
	.proc	014
records__ERBOOLEAN7:
.LLFB640:
	save	%sp, -96, %sp
.LLCFI640:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+3], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE640:
	.size	records__ERBOOLEAN7, .-records__ERBOOLEAN7
	.align 4
	.type	records__FRBOOLEAN7, #function
	.proc	020
records__FRBOOLEAN7:
.LLFB641:
	save	%sp, -104, %sp
.LLCFI641:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	7, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE641:
	.size	records__FRBOOLEAN7, .-records__FRBOOLEAN7
	.align 4
	.type	records__ARBOOLEAN8, #function
	.proc	014
records__ARBOOLEAN8:
.LLFB642:
	save	%sp, -96, %sp
.LLCFI642:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE642:
	.size	records__ARBOOLEAN8, .-records__ARBOOLEAN8
	.align 4
	.type	records__BRBOOLEAN8, #function
	.proc	020
records__BRBOOLEAN8:
.LLFB643:
	save	%sp, -104, %sp
.LLCFI643:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE643:
	.size	records__BRBOOLEAN8, .-records__BRBOOLEAN8
	.align 4
	.type	records__CRBOOLEAN8, #function
	.proc	014
records__CRBOOLEAN8:
.LLFB644:
	save	%sp, -96, %sp
.LLCFI644:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE644:
	.size	records__CRBOOLEAN8, .-records__CRBOOLEAN8
	.align 4
	.type	records__DRBOOLEAN8, #function
	.proc	020
records__DRBOOLEAN8:
.LLFB645:
	save	%sp, -104, %sp
.LLCFI645:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE645:
	.size	records__DRBOOLEAN8, .-records__DRBOOLEAN8
	.align 4
	.type	records__ERBOOLEAN8, #function
	.proc	014
records__ERBOOLEAN8:
.LLFB646:
	save	%sp, -96, %sp
.LLCFI646:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE646:
	.size	records__ERBOOLEAN8, .-records__ERBOOLEAN8
	.align 4
	.type	records__FRBOOLEAN8, #function
	.proc	020
records__FRBOOLEAN8:
.LLFB647:
	save	%sp, -104, %sp
.LLCFI647:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE647:
	.size	records__FRBOOLEAN8, .-records__FRBOOLEAN8
	.align 4
	.type	records__ARBOOLEAN9, #function
	.proc	014
records__ARBOOLEAN9:
.LLFB648:
	save	%sp, -96, %sp
.LLCFI648:
	mov	%i0, %g1
	ldub	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE648:
	.size	records__ARBOOLEAN9, .-records__ARBOOLEAN9
	.align 4
	.type	records__BRBOOLEAN9, #function
	.proc	020
records__BRBOOLEAN9:
.LLFB649:
	save	%sp, -112, %sp
.LLCFI649:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	9, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE649:
	.size	records__BRBOOLEAN9, .-records__BRBOOLEAN9
	.align 4
	.type	records__CRBOOLEAN9, #function
	.proc	014
records__CRBOOLEAN9:
.LLFB650:
	save	%sp, -96, %sp
.LLCFI650:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldub	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE650:
	.size	records__CRBOOLEAN9, .-records__CRBOOLEAN9
	.align 4
	.type	records__DRBOOLEAN9, #function
	.proc	020
records__DRBOOLEAN9:
.LLFB651:
	save	%sp, -112, %sp
.LLCFI651:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	9, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE651:
	.size	records__DRBOOLEAN9, .-records__DRBOOLEAN9
	.align 4
	.type	records__ERBOOLEAN9, #function
	.proc	014
records__ERBOOLEAN9:
.LLFB652:
	save	%sp, -96, %sp
.LLCFI652:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldub	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE652:
	.size	records__ERBOOLEAN9, .-records__ERBOOLEAN9
	.align 4
	.type	records__FRBOOLEAN9, #function
	.proc	020
records__FRBOOLEAN9:
.LLFB653:
	save	%sp, -112, %sp
.LLCFI653:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	9, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE653:
	.size	records__FRBOOLEAN9, .-records__FRBOOLEAN9
	.align 4
	.type	records__ARBOOLEAN10, #function
	.proc	014
records__ARBOOLEAN10:
.LLFB654:
	save	%sp, -96, %sp
.LLCFI654:
	mov	%i0, %g1
	lduh	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE654:
	.size	records__ARBOOLEAN10, .-records__ARBOOLEAN10
	.align 4
	.type	records__BRBOOLEAN10, #function
	.proc	020
records__BRBOOLEAN10:
.LLFB655:
	save	%sp, -112, %sp
.LLCFI655:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE655:
	.size	records__BRBOOLEAN10, .-records__BRBOOLEAN10
	.align 4
	.type	records__CRBOOLEAN10, #function
	.proc	014
records__CRBOOLEAN10:
.LLFB656:
	save	%sp, -96, %sp
.LLCFI656:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE656:
	.size	records__CRBOOLEAN10, .-records__CRBOOLEAN10
	.align 4
	.type	records__DRBOOLEAN10, #function
	.proc	020
records__DRBOOLEAN10:
.LLFB657:
	save	%sp, -112, %sp
.LLCFI657:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE657:
	.size	records__DRBOOLEAN10, .-records__DRBOOLEAN10
	.align 4
	.type	records__ERBOOLEAN10, #function
	.proc	014
records__ERBOOLEAN10:
.LLFB658:
	save	%sp, -96, %sp
.LLCFI658:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE658:
	.size	records__ERBOOLEAN10, .-records__ERBOOLEAN10
	.align 4
	.type	records__FRBOOLEAN10, #function
	.proc	020
records__FRBOOLEAN10:
.LLFB659:
	save	%sp, -112, %sp
.LLCFI659:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE659:
	.size	records__FRBOOLEAN10, .-records__FRBOOLEAN10
	.align 4
	.type	records__ARWIDECHAR1, #function
	.proc	015
records__ARWIDECHAR1:
.LLFB660:
	save	%sp, -96, %sp
.LLCFI660:
	mov	%i0, %g1
	lduh	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE660:
	.size	records__ARWIDECHAR1, .-records__ARWIDECHAR1
	.align 4
	.type	records__BRWIDECHAR1, #function
	.proc	020
records__BRWIDECHAR1:
.LLFB661:
	save	%sp, -104, %sp
.LLCFI661:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE661:
	.size	records__BRWIDECHAR1, .-records__BRWIDECHAR1
	.align 4
	.type	records__CRWIDECHAR1, #function
	.proc	015
records__CRWIDECHAR1:
.LLFB662:
	save	%sp, -96, %sp
.LLCFI662:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE662:
	.size	records__CRWIDECHAR1, .-records__CRWIDECHAR1
	.align 4
	.type	records__DRWIDECHAR1, #function
	.proc	020
records__DRWIDECHAR1:
.LLFB663:
	save	%sp, -104, %sp
.LLCFI663:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE663:
	.size	records__DRWIDECHAR1, .-records__DRWIDECHAR1
	.align 4
	.type	records__ERWIDECHAR1, #function
	.proc	015
records__ERWIDECHAR1:
.LLFB664:
	save	%sp, -96, %sp
.LLCFI664:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE664:
	.size	records__ERWIDECHAR1, .-records__ERWIDECHAR1
	.align 4
	.type	records__FRWIDECHAR1, #function
	.proc	020
records__FRWIDECHAR1:
.LLFB665:
	save	%sp, -104, %sp
.LLCFI665:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	2, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE665:
	.size	records__FRWIDECHAR1, .-records__FRWIDECHAR1
	.align 4
	.type	records__ARWIDECHAR2, #function
	.proc	015
records__ARWIDECHAR2:
.LLFB666:
	save	%sp, -96, %sp
.LLCFI666:
	mov	%i0, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE666:
	.size	records__ARWIDECHAR2, .-records__ARWIDECHAR2
	.align 4
	.type	records__BRWIDECHAR2, #function
	.proc	020
records__BRWIDECHAR2:
.LLFB667:
	save	%sp, -104, %sp
.LLCFI667:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE667:
	.size	records__BRWIDECHAR2, .-records__BRWIDECHAR2
	.align 4
	.type	records__CRWIDECHAR2, #function
	.proc	015
records__CRWIDECHAR2:
.LLFB668:
	save	%sp, -96, %sp
.LLCFI668:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE668:
	.size	records__CRWIDECHAR2, .-records__CRWIDECHAR2
	.align 4
	.type	records__DRWIDECHAR2, #function
	.proc	020
records__DRWIDECHAR2:
.LLFB669:
	save	%sp, -104, %sp
.LLCFI669:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE669:
	.size	records__DRWIDECHAR2, .-records__DRWIDECHAR2
	.align 4
	.type	records__ERWIDECHAR2, #function
	.proc	015
records__ERWIDECHAR2:
.LLFB670:
	save	%sp, -96, %sp
.LLCFI670:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE670:
	.size	records__ERWIDECHAR2, .-records__ERWIDECHAR2
	.align 4
	.type	records__FRWIDECHAR2, #function
	.proc	020
records__FRWIDECHAR2:
.LLFB671:
	save	%sp, -104, %sp
.LLCFI671:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE671:
	.size	records__FRWIDECHAR2, .-records__FRWIDECHAR2
	.align 4
	.type	records__ARWIDECHAR3, #function
	.proc	015
records__ARWIDECHAR3:
.LLFB672:
	save	%sp, -96, %sp
.LLCFI672:
	mov	%i0, %g1
	lduh	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE672:
	.size	records__ARWIDECHAR3, .-records__ARWIDECHAR3
	.align 4
	.type	records__BRWIDECHAR3, #function
	.proc	020
records__BRWIDECHAR3:
.LLFB673:
	save	%sp, -104, %sp
.LLCFI673:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE673:
	.size	records__BRWIDECHAR3, .-records__BRWIDECHAR3
	.align 4
	.type	records__CRWIDECHAR3, #function
	.proc	015
records__CRWIDECHAR3:
.LLFB674:
	save	%sp, -96, %sp
.LLCFI674:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE674:
	.size	records__CRWIDECHAR3, .-records__CRWIDECHAR3
	.align 4
	.type	records__DRWIDECHAR3, #function
	.proc	020
records__DRWIDECHAR3:
.LLFB675:
	save	%sp, -104, %sp
.LLCFI675:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE675:
	.size	records__DRWIDECHAR3, .-records__DRWIDECHAR3
	.align 4
	.type	records__ERWIDECHAR3, #function
	.proc	015
records__ERWIDECHAR3:
.LLFB676:
	save	%sp, -96, %sp
.LLCFI676:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+2], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE676:
	.size	records__ERWIDECHAR3, .-records__ERWIDECHAR3
	.align 4
	.type	records__FRWIDECHAR3, #function
	.proc	020
records__FRWIDECHAR3:
.LLFB677:
	save	%sp, -104, %sp
.LLCFI677:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	6, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE677:
	.size	records__FRWIDECHAR3, .-records__FRWIDECHAR3
	.align 4
	.type	records__ARWIDECHAR4, #function
	.proc	015
records__ARWIDECHAR4:
.LLFB678:
	save	%sp, -96, %sp
.LLCFI678:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE678:
	.size	records__ARWIDECHAR4, .-records__ARWIDECHAR4
	.align 4
	.type	records__BRWIDECHAR4, #function
	.proc	020
records__BRWIDECHAR4:
.LLFB679:
	save	%sp, -104, %sp
.LLCFI679:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE679:
	.size	records__BRWIDECHAR4, .-records__BRWIDECHAR4
	.align 4
	.type	records__CRWIDECHAR4, #function
	.proc	015
records__CRWIDECHAR4:
.LLFB680:
	save	%sp, -96, %sp
.LLCFI680:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE680:
	.size	records__CRWIDECHAR4, .-records__CRWIDECHAR4
	.align 4
	.type	records__DRWIDECHAR4, #function
	.proc	020
records__DRWIDECHAR4:
.LLFB681:
	save	%sp, -104, %sp
.LLCFI681:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE681:
	.size	records__DRWIDECHAR4, .-records__DRWIDECHAR4
	.align 4
	.type	records__ERWIDECHAR4, #function
	.proc	015
records__ERWIDECHAR4:
.LLFB682:
	save	%sp, -96, %sp
.LLCFI682:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE682:
	.size	records__ERWIDECHAR4, .-records__ERWIDECHAR4
	.align 4
	.type	records__FRWIDECHAR4, #function
	.proc	020
records__FRWIDECHAR4:
.LLFB683:
	save	%sp, -104, %sp
.LLCFI683:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE683:
	.size	records__FRWIDECHAR4, .-records__FRWIDECHAR4
	.align 4
	.type	records__ARWIDECHAR5, #function
	.proc	015
records__ARWIDECHAR5:
.LLFB684:
	save	%sp, -96, %sp
.LLCFI684:
	mov	%i0, %g1
	lduh	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE684:
	.size	records__ARWIDECHAR5, .-records__ARWIDECHAR5
	.align 4
	.type	records__BRWIDECHAR5, #function
	.proc	020
records__BRWIDECHAR5:
.LLFB685:
	save	%sp, -112, %sp
.LLCFI685:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE685:
	.size	records__BRWIDECHAR5, .-records__BRWIDECHAR5
	.align 4
	.type	records__CRWIDECHAR5, #function
	.proc	015
records__CRWIDECHAR5:
.LLFB686:
	save	%sp, -96, %sp
.LLCFI686:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE686:
	.size	records__CRWIDECHAR5, .-records__CRWIDECHAR5
	.align 4
	.type	records__DRWIDECHAR5, #function
	.proc	020
records__DRWIDECHAR5:
.LLFB687:
	save	%sp, -112, %sp
.LLCFI687:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE687:
	.size	records__DRWIDECHAR5, .-records__DRWIDECHAR5
	.align 4
	.type	records__ERWIDECHAR5, #function
	.proc	015
records__ERWIDECHAR5:
.LLFB688:
	save	%sp, -96, %sp
.LLCFI688:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE688:
	.size	records__ERWIDECHAR5, .-records__ERWIDECHAR5
	.align 4
	.type	records__FRWIDECHAR5, #function
	.proc	020
records__FRWIDECHAR5:
.LLFB689:
	save	%sp, -112, %sp
.LLCFI689:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	10, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE689:
	.size	records__FRWIDECHAR5, .-records__FRWIDECHAR5
	.align 4
	.type	records__ARWIDECHAR6, #function
	.proc	015
records__ARWIDECHAR6:
.LLFB690:
	save	%sp, -96, %sp
.LLCFI690:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE690:
	.size	records__ARWIDECHAR6, .-records__ARWIDECHAR6
	.align 4
	.type	records__BRWIDECHAR6, #function
	.proc	020
records__BRWIDECHAR6:
.LLFB691:
	save	%sp, -112, %sp
.LLCFI691:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE691:
	.size	records__BRWIDECHAR6, .-records__BRWIDECHAR6
	.align 4
	.type	records__CRWIDECHAR6, #function
	.proc	015
records__CRWIDECHAR6:
.LLFB692:
	save	%sp, -96, %sp
.LLCFI692:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE692:
	.size	records__CRWIDECHAR6, .-records__CRWIDECHAR6
	.align 4
	.type	records__DRWIDECHAR6, #function
	.proc	020
records__DRWIDECHAR6:
.LLFB693:
	save	%sp, -112, %sp
.LLCFI693:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE693:
	.size	records__DRWIDECHAR6, .-records__DRWIDECHAR6
	.align 4
	.type	records__ERWIDECHAR6, #function
	.proc	015
records__ERWIDECHAR6:
.LLFB694:
	save	%sp, -96, %sp
.LLCFI694:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE694:
	.size	records__ERWIDECHAR6, .-records__ERWIDECHAR6
	.align 4
	.type	records__FRWIDECHAR6, #function
	.proc	020
records__FRWIDECHAR6:
.LLFB695:
	save	%sp, -112, %sp
.LLCFI695:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE695:
	.size	records__FRWIDECHAR6, .-records__FRWIDECHAR6
	.align 4
	.type	records__ARWIDECHAR7, #function
	.proc	015
records__ARWIDECHAR7:
.LLFB696:
	save	%sp, -96, %sp
.LLCFI696:
	mov	%i0, %g1
	lduh	[%g1+6], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE696:
	.size	records__ARWIDECHAR7, .-records__ARWIDECHAR7
	.align 4
	.type	records__BRWIDECHAR7, #function
	.proc	020
records__BRWIDECHAR7:
.LLFB697:
	save	%sp, -112, %sp
.LLCFI697:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	14, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE697:
	.size	records__BRWIDECHAR7, .-records__BRWIDECHAR7
	.align 4
	.type	records__CRWIDECHAR7, #function
	.proc	015
records__CRWIDECHAR7:
.LLFB698:
	save	%sp, -96, %sp
.LLCFI698:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+6], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE698:
	.size	records__CRWIDECHAR7, .-records__CRWIDECHAR7
	.align 4
	.type	records__DRWIDECHAR7, #function
	.proc	020
records__DRWIDECHAR7:
.LLFB699:
	save	%sp, -112, %sp
.LLCFI699:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	14, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE699:
	.size	records__DRWIDECHAR7, .-records__DRWIDECHAR7
	.align 4
	.type	records__ERWIDECHAR7, #function
	.proc	015
records__ERWIDECHAR7:
.LLFB700:
	save	%sp, -96, %sp
.LLCFI700:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+6], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE700:
	.size	records__ERWIDECHAR7, .-records__ERWIDECHAR7
	.align 4
	.type	records__FRWIDECHAR7, #function
	.proc	020
records__FRWIDECHAR7:
.LLFB701:
	save	%sp, -112, %sp
.LLCFI701:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	14, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE701:
	.size	records__FRWIDECHAR7, .-records__FRWIDECHAR7
	.align 4
	.type	records__ARWIDECHAR8, #function
	.proc	015
records__ARWIDECHAR8:
.LLFB702:
	save	%sp, -96, %sp
.LLCFI702:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE702:
	.size	records__ARWIDECHAR8, .-records__ARWIDECHAR8
	.align 4
	.type	records__BRWIDECHAR8, #function
	.proc	020
records__BRWIDECHAR8:
.LLFB703:
	save	%sp, -112, %sp
.LLCFI703:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE703:
	.size	records__BRWIDECHAR8, .-records__BRWIDECHAR8
	.align 4
	.type	records__CRWIDECHAR8, #function
	.proc	015
records__CRWIDECHAR8:
.LLFB704:
	save	%sp, -96, %sp
.LLCFI704:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE704:
	.size	records__CRWIDECHAR8, .-records__CRWIDECHAR8
	.align 4
	.type	records__DRWIDECHAR8, #function
	.proc	020
records__DRWIDECHAR8:
.LLFB705:
	save	%sp, -112, %sp
.LLCFI705:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE705:
	.size	records__DRWIDECHAR8, .-records__DRWIDECHAR8
	.align 4
	.type	records__ERWIDECHAR8, #function
	.proc	015
records__ERWIDECHAR8:
.LLFB706:
	save	%sp, -96, %sp
.LLCFI706:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE706:
	.size	records__ERWIDECHAR8, .-records__ERWIDECHAR8
	.align 4
	.type	records__FRWIDECHAR8, #function
	.proc	020
records__FRWIDECHAR8:
.LLFB707:
	save	%sp, -112, %sp
.LLCFI707:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE707:
	.size	records__FRWIDECHAR8, .-records__FRWIDECHAR8
	.align 4
	.type	records__ARWIDECHAR9, #function
	.proc	015
records__ARWIDECHAR9:
.LLFB708:
	save	%sp, -96, %sp
.LLCFI708:
	mov	%i0, %g1
	lduh	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE708:
	.size	records__ARWIDECHAR9, .-records__ARWIDECHAR9
	.align 4
	.type	records__BRWIDECHAR9, #function
	.proc	020
records__BRWIDECHAR9:
.LLFB709:
	save	%sp, -120, %sp
.LLCFI709:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	18, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE709:
	.size	records__BRWIDECHAR9, .-records__BRWIDECHAR9
	.align 4
	.type	records__CRWIDECHAR9, #function
	.proc	015
records__CRWIDECHAR9:
.LLFB710:
	save	%sp, -96, %sp
.LLCFI710:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	lduh	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE710:
	.size	records__CRWIDECHAR9, .-records__CRWIDECHAR9
	.align 4
	.type	records__DRWIDECHAR9, #function
	.proc	020
records__DRWIDECHAR9:
.LLFB711:
	save	%sp, -120, %sp
.LLCFI711:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	18, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE711:
	.size	records__DRWIDECHAR9, .-records__DRWIDECHAR9
	.align 4
	.type	records__ERWIDECHAR9, #function
	.proc	015
records__ERWIDECHAR9:
.LLFB712:
	save	%sp, -96, %sp
.LLCFI712:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	lduh	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE712:
	.size	records__ERWIDECHAR9, .-records__ERWIDECHAR9
	.align 4
	.type	records__FRWIDECHAR9, #function
	.proc	020
records__FRWIDECHAR9:
.LLFB713:
	save	%sp, -120, %sp
.LLCFI713:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	18, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE713:
	.size	records__FRWIDECHAR9, .-records__FRWIDECHAR9
	.align 4
	.type	records__ARWIDECHAR10, #function
	.proc	015
records__ARWIDECHAR10:
.LLFB714:
	save	%sp, -96, %sp
.LLCFI714:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE714:
	.size	records__ARWIDECHAR10, .-records__ARWIDECHAR10
	.align 4
	.type	records__BRWIDECHAR10, #function
	.proc	020
records__BRWIDECHAR10:
.LLFB715:
	save	%sp, -120, %sp
.LLCFI715:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE715:
	.size	records__BRWIDECHAR10, .-records__BRWIDECHAR10
	.align 4
	.type	records__CRWIDECHAR10, #function
	.proc	015
records__CRWIDECHAR10:
.LLFB716:
	save	%sp, -96, %sp
.LLCFI716:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE716:
	.size	records__CRWIDECHAR10, .-records__CRWIDECHAR10
	.align 4
	.type	records__DRWIDECHAR10, #function
	.proc	020
records__DRWIDECHAR10:
.LLFB717:
	save	%sp, -120, %sp
.LLCFI717:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE717:
	.size	records__DRWIDECHAR10, .-records__DRWIDECHAR10
	.align 4
	.type	records__ERWIDECHAR10, #function
	.proc	015
records__ERWIDECHAR10:
.LLFB718:
	save	%sp, -96, %sp
.LLCFI718:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	and	%g1, -1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE718:
	.size	records__ERWIDECHAR10, .-records__ERWIDECHAR10
	.align 4
	.type	records__FRWIDECHAR10, #function
	.proc	020
records__FRWIDECHAR10:
.LLFB719:
	save	%sp, -120, %sp
.LLCFI719:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE719:
	.size	records__FRWIDECHAR10, .-records__FRWIDECHAR10
	.align 4
	.type	records__ARINT321, #function
	.proc	04
records__ARINT321:
.LLFB720:
	save	%sp, -96, %sp
.LLCFI720:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE720:
	.size	records__ARINT321, .-records__ARINT321
	.align 4
	.type	records__BRINT321, #function
	.proc	020
records__BRINT321:
.LLFB721:
	save	%sp, -104, %sp
.LLCFI721:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE721:
	.size	records__BRINT321, .-records__BRINT321
	.align 4
	.type	records__CRINT321, #function
	.proc	04
records__CRINT321:
.LLFB722:
	save	%sp, -96, %sp
.LLCFI722:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE722:
	.size	records__CRINT321, .-records__CRINT321
	.align 4
	.type	records__DRINT321, #function
	.proc	020
records__DRINT321:
.LLFB723:
	save	%sp, -104, %sp
.LLCFI723:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE723:
	.size	records__DRINT321, .-records__DRINT321
	.align 4
	.type	records__ERINT321, #function
	.proc	04
records__ERINT321:
.LLFB724:
	save	%sp, -96, %sp
.LLCFI724:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE724:
	.size	records__ERINT321, .-records__ERINT321
	.align 4
	.type	records__FRINT321, #function
	.proc	020
records__FRINT321:
.LLFB725:
	save	%sp, -104, %sp
.LLCFI725:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE725:
	.size	records__FRINT321, .-records__FRINT321
	.align 4
	.type	records__ARINT322, #function
	.proc	04
records__ARINT322:
.LLFB726:
	save	%sp, -96, %sp
.LLCFI726:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE726:
	.size	records__ARINT322, .-records__ARINT322
	.align 4
	.type	records__BRINT322, #function
	.proc	020
records__BRINT322:
.LLFB727:
	save	%sp, -104, %sp
.LLCFI727:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE727:
	.size	records__BRINT322, .-records__BRINT322
	.align 4
	.type	records__CRINT322, #function
	.proc	04
records__CRINT322:
.LLFB728:
	save	%sp, -96, %sp
.LLCFI728:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE728:
	.size	records__CRINT322, .-records__CRINT322
	.align 4
	.type	records__DRINT322, #function
	.proc	020
records__DRINT322:
.LLFB729:
	save	%sp, -104, %sp
.LLCFI729:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE729:
	.size	records__DRINT322, .-records__DRINT322
	.align 4
	.type	records__ERINT322, #function
	.proc	04
records__ERINT322:
.LLFB730:
	save	%sp, -96, %sp
.LLCFI730:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE730:
	.size	records__ERINT322, .-records__ERINT322
	.align 4
	.type	records__FRINT322, #function
	.proc	020
records__FRINT322:
.LLFB731:
	save	%sp, -104, %sp
.LLCFI731:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE731:
	.size	records__FRINT322, .-records__FRINT322
	.align 4
	.type	records__ARINT323, #function
	.proc	04
records__ARINT323:
.LLFB732:
	save	%sp, -96, %sp
.LLCFI732:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE732:
	.size	records__ARINT323, .-records__ARINT323
	.align 4
	.type	records__BRINT323, #function
	.proc	020
records__BRINT323:
.LLFB733:
	save	%sp, -112, %sp
.LLCFI733:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE733:
	.size	records__BRINT323, .-records__BRINT323
	.align 4
	.type	records__CRINT323, #function
	.proc	04
records__CRINT323:
.LLFB734:
	save	%sp, -96, %sp
.LLCFI734:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE734:
	.size	records__CRINT323, .-records__CRINT323
	.align 4
	.type	records__DRINT323, #function
	.proc	020
records__DRINT323:
.LLFB735:
	save	%sp, -112, %sp
.LLCFI735:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE735:
	.size	records__DRINT323, .-records__DRINT323
	.align 4
	.type	records__ERINT323, #function
	.proc	04
records__ERINT323:
.LLFB736:
	save	%sp, -96, %sp
.LLCFI736:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE736:
	.size	records__ERINT323, .-records__ERINT323
	.align 4
	.type	records__FRINT323, #function
	.proc	020
records__FRINT323:
.LLFB737:
	save	%sp, -112, %sp
.LLCFI737:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE737:
	.size	records__FRINT323, .-records__FRINT323
	.align 4
	.type	records__ARINT324, #function
	.proc	04
records__ARINT324:
.LLFB738:
	save	%sp, -96, %sp
.LLCFI738:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE738:
	.size	records__ARINT324, .-records__ARINT324
	.align 4
	.type	records__BRINT324, #function
	.proc	020
records__BRINT324:
.LLFB739:
	save	%sp, -112, %sp
.LLCFI739:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE739:
	.size	records__BRINT324, .-records__BRINT324
	.align 4
	.type	records__CRINT324, #function
	.proc	04
records__CRINT324:
.LLFB740:
	save	%sp, -96, %sp
.LLCFI740:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE740:
	.size	records__CRINT324, .-records__CRINT324
	.align 4
	.type	records__DRINT324, #function
	.proc	020
records__DRINT324:
.LLFB741:
	save	%sp, -112, %sp
.LLCFI741:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE741:
	.size	records__DRINT324, .-records__DRINT324
	.align 4
	.type	records__ERINT324, #function
	.proc	04
records__ERINT324:
.LLFB742:
	save	%sp, -96, %sp
.LLCFI742:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE742:
	.size	records__ERINT324, .-records__ERINT324
	.align 4
	.type	records__FRINT324, #function
	.proc	020
records__FRINT324:
.LLFB743:
	save	%sp, -112, %sp
.LLCFI743:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE743:
	.size	records__FRINT324, .-records__FRINT324
	.align 4
	.type	records__ARINT325, #function
	.proc	04
records__ARINT325:
.LLFB744:
	save	%sp, -96, %sp
.LLCFI744:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE744:
	.size	records__ARINT325, .-records__ARINT325
	.align 4
	.type	records__BRINT325, #function
	.proc	020
records__BRINT325:
.LLFB745:
	save	%sp, -120, %sp
.LLCFI745:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE745:
	.size	records__BRINT325, .-records__BRINT325
	.align 4
	.type	records__CRINT325, #function
	.proc	04
records__CRINT325:
.LLFB746:
	save	%sp, -96, %sp
.LLCFI746:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE746:
	.size	records__CRINT325, .-records__CRINT325
	.align 4
	.type	records__DRINT325, #function
	.proc	020
records__DRINT325:
.LLFB747:
	save	%sp, -120, %sp
.LLCFI747:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE747:
	.size	records__DRINT325, .-records__DRINT325
	.align 4
	.type	records__ERINT325, #function
	.proc	04
records__ERINT325:
.LLFB748:
	save	%sp, -96, %sp
.LLCFI748:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE748:
	.size	records__ERINT325, .-records__ERINT325
	.align 4
	.type	records__FRINT325, #function
	.proc	020
records__FRINT325:
.LLFB749:
	save	%sp, -120, %sp
.LLCFI749:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE749:
	.size	records__FRINT325, .-records__FRINT325
	.align 4
	.type	records__ARINT326, #function
	.proc	04
records__ARINT326:
.LLFB750:
	save	%sp, -96, %sp
.LLCFI750:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE750:
	.size	records__ARINT326, .-records__ARINT326
	.align 4
	.type	records__BRINT326, #function
	.proc	020
records__BRINT326:
.LLFB751:
	save	%sp, -120, %sp
.LLCFI751:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE751:
	.size	records__BRINT326, .-records__BRINT326
	.align 4
	.type	records__CRINT326, #function
	.proc	04
records__CRINT326:
.LLFB752:
	save	%sp, -96, %sp
.LLCFI752:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE752:
	.size	records__CRINT326, .-records__CRINT326
	.align 4
	.type	records__DRINT326, #function
	.proc	020
records__DRINT326:
.LLFB753:
	save	%sp, -120, %sp
.LLCFI753:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE753:
	.size	records__DRINT326, .-records__DRINT326
	.align 4
	.type	records__ERINT326, #function
	.proc	04
records__ERINT326:
.LLFB754:
	save	%sp, -96, %sp
.LLCFI754:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE754:
	.size	records__ERINT326, .-records__ERINT326
	.align 4
	.type	records__FRINT326, #function
	.proc	020
records__FRINT326:
.LLFB755:
	save	%sp, -120, %sp
.LLCFI755:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE755:
	.size	records__FRINT326, .-records__FRINT326
	.align 4
	.type	records__ARINT327, #function
	.proc	04
records__ARINT327:
.LLFB756:
	save	%sp, -96, %sp
.LLCFI756:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE756:
	.size	records__ARINT327, .-records__ARINT327
	.align 4
	.type	records__BRINT327, #function
	.proc	020
records__BRINT327:
.LLFB757:
	save	%sp, -128, %sp
.LLCFI757:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE757:
	.size	records__BRINT327, .-records__BRINT327
	.align 4
	.type	records__CRINT327, #function
	.proc	04
records__CRINT327:
.LLFB758:
	save	%sp, -96, %sp
.LLCFI758:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE758:
	.size	records__CRINT327, .-records__CRINT327
	.align 4
	.type	records__DRINT327, #function
	.proc	020
records__DRINT327:
.LLFB759:
	save	%sp, -128, %sp
.LLCFI759:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE759:
	.size	records__DRINT327, .-records__DRINT327
	.align 4
	.type	records__ERINT327, #function
	.proc	04
records__ERINT327:
.LLFB760:
	save	%sp, -96, %sp
.LLCFI760:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE760:
	.size	records__ERINT327, .-records__ERINT327
	.align 4
	.type	records__FRINT327, #function
	.proc	020
records__FRINT327:
.LLFB761:
	save	%sp, -128, %sp
.LLCFI761:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE761:
	.size	records__FRINT327, .-records__FRINT327
	.align 4
	.type	records__ARINT328, #function
	.proc	04
records__ARINT328:
.LLFB762:
	save	%sp, -96, %sp
.LLCFI762:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE762:
	.size	records__ARINT328, .-records__ARINT328
	.align 4
	.type	records__BRINT328, #function
	.proc	020
records__BRINT328:
.LLFB763:
	save	%sp, -128, %sp
.LLCFI763:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE763:
	.size	records__BRINT328, .-records__BRINT328
	.align 4
	.type	records__CRINT328, #function
	.proc	04
records__CRINT328:
.LLFB764:
	save	%sp, -96, %sp
.LLCFI764:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE764:
	.size	records__CRINT328, .-records__CRINT328
	.align 4
	.type	records__DRINT328, #function
	.proc	020
records__DRINT328:
.LLFB765:
	save	%sp, -128, %sp
.LLCFI765:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE765:
	.size	records__DRINT328, .-records__DRINT328
	.align 4
	.type	records__ERINT328, #function
	.proc	04
records__ERINT328:
.LLFB766:
	save	%sp, -96, %sp
.LLCFI766:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE766:
	.size	records__ERINT328, .-records__ERINT328
	.align 4
	.type	records__FRINT328, #function
	.proc	020
records__FRINT328:
.LLFB767:
	save	%sp, -128, %sp
.LLCFI767:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE767:
	.size	records__FRINT328, .-records__FRINT328
	.align 4
	.type	records__ARINT329, #function
	.proc	04
records__ARINT329:
.LLFB768:
	save	%sp, -96, %sp
.LLCFI768:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE768:
	.size	records__ARINT329, .-records__ARINT329
	.align 4
	.type	records__BRINT329, #function
	.proc	020
records__BRINT329:
.LLFB769:
	save	%sp, -136, %sp
.LLCFI769:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE769:
	.size	records__BRINT329, .-records__BRINT329
	.align 4
	.type	records__CRINT329, #function
	.proc	04
records__CRINT329:
.LLFB770:
	save	%sp, -96, %sp
.LLCFI770:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE770:
	.size	records__CRINT329, .-records__CRINT329
	.align 4
	.type	records__DRINT329, #function
	.proc	020
records__DRINT329:
.LLFB771:
	save	%sp, -136, %sp
.LLCFI771:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE771:
	.size	records__DRINT329, .-records__DRINT329
	.align 4
	.type	records__ERINT329, #function
	.proc	04
records__ERINT329:
.LLFB772:
	save	%sp, -96, %sp
.LLCFI772:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE772:
	.size	records__ERINT329, .-records__ERINT329
	.align 4
	.type	records__FRINT329, #function
	.proc	020
records__FRINT329:
.LLFB773:
	save	%sp, -136, %sp
.LLCFI773:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE773:
	.size	records__FRINT329, .-records__FRINT329
	.align 4
	.type	records__ARINT3210, #function
	.proc	04
records__ARINT3210:
.LLFB774:
	save	%sp, -96, %sp
.LLCFI774:
	mov	%i0, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE774:
	.size	records__ARINT3210, .-records__ARINT3210
	.align 4
	.type	records__BRINT3210, #function
	.proc	020
records__BRINT3210:
.LLFB775:
	save	%sp, -136, %sp
.LLCFI775:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE775:
	.size	records__BRINT3210, .-records__BRINT3210
	.align 4
	.type	records__CRINT3210, #function
	.proc	04
records__CRINT3210:
.LLFB776:
	save	%sp, -96, %sp
.LLCFI776:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE776:
	.size	records__CRINT3210, .-records__CRINT3210
	.align 4
	.type	records__DRINT3210, #function
	.proc	020
records__DRINT3210:
.LLFB777:
	save	%sp, -136, %sp
.LLCFI777:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE777:
	.size	records__DRINT3210, .-records__DRINT3210
	.align 4
	.type	records__ERINT3210, #function
	.proc	04
records__ERINT3210:
.LLFB778:
	save	%sp, -96, %sp
.LLCFI778:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE778:
	.size	records__ERINT3210, .-records__ERINT3210
	.align 4
	.type	records__FRINT3210, #function
	.proc	020
records__FRINT3210:
.LLFB779:
	save	%sp, -136, %sp
.LLCFI779:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE779:
	.size	records__FRINT3210, .-records__FRINT3210
	.align 4
	.type	records__ARUINT321, #function
	.proc	04
records__ARUINT321:
.LLFB780:
	save	%sp, -96, %sp
.LLCFI780:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE780:
	.size	records__ARUINT321, .-records__ARUINT321
	.align 4
	.type	records__BRUINT321, #function
	.proc	020
records__BRUINT321:
.LLFB781:
	save	%sp, -104, %sp
.LLCFI781:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE781:
	.size	records__BRUINT321, .-records__BRUINT321
	.align 4
	.type	records__CRUINT321, #function
	.proc	04
records__CRUINT321:
.LLFB782:
	save	%sp, -96, %sp
.LLCFI782:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE782:
	.size	records__CRUINT321, .-records__CRUINT321
	.align 4
	.type	records__DRUINT321, #function
	.proc	020
records__DRUINT321:
.LLFB783:
	save	%sp, -104, %sp
.LLCFI783:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE783:
	.size	records__DRUINT321, .-records__DRUINT321
	.align 4
	.type	records__ERUINT321, #function
	.proc	04
records__ERUINT321:
.LLFB784:
	save	%sp, -96, %sp
.LLCFI784:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE784:
	.size	records__ERUINT321, .-records__ERUINT321
	.align 4
	.type	records__FRUINT321, #function
	.proc	020
records__FRUINT321:
.LLFB785:
	save	%sp, -104, %sp
.LLCFI785:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE785:
	.size	records__FRUINT321, .-records__FRUINT321
	.align 4
	.type	records__ARUINT322, #function
	.proc	04
records__ARUINT322:
.LLFB786:
	save	%sp, -96, %sp
.LLCFI786:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE786:
	.size	records__ARUINT322, .-records__ARUINT322
	.align 4
	.type	records__BRUINT322, #function
	.proc	020
records__BRUINT322:
.LLFB787:
	save	%sp, -104, %sp
.LLCFI787:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE787:
	.size	records__BRUINT322, .-records__BRUINT322
	.align 4
	.type	records__CRUINT322, #function
	.proc	04
records__CRUINT322:
.LLFB788:
	save	%sp, -96, %sp
.LLCFI788:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE788:
	.size	records__CRUINT322, .-records__CRUINT322
	.align 4
	.type	records__DRUINT322, #function
	.proc	020
records__DRUINT322:
.LLFB789:
	save	%sp, -104, %sp
.LLCFI789:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE789:
	.size	records__DRUINT322, .-records__DRUINT322
	.align 4
	.type	records__ERUINT322, #function
	.proc	04
records__ERUINT322:
.LLFB790:
	save	%sp, -96, %sp
.LLCFI790:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE790:
	.size	records__ERUINT322, .-records__ERUINT322
	.align 4
	.type	records__FRUINT322, #function
	.proc	020
records__FRUINT322:
.LLFB791:
	save	%sp, -104, %sp
.LLCFI791:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE791:
	.size	records__FRUINT322, .-records__FRUINT322
	.align 4
	.type	records__ARUINT323, #function
	.proc	04
records__ARUINT323:
.LLFB792:
	save	%sp, -96, %sp
.LLCFI792:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE792:
	.size	records__ARUINT323, .-records__ARUINT323
	.align 4
	.type	records__BRUINT323, #function
	.proc	020
records__BRUINT323:
.LLFB793:
	save	%sp, -112, %sp
.LLCFI793:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE793:
	.size	records__BRUINT323, .-records__BRUINT323
	.align 4
	.type	records__CRUINT323, #function
	.proc	04
records__CRUINT323:
.LLFB794:
	save	%sp, -96, %sp
.LLCFI794:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE794:
	.size	records__CRUINT323, .-records__CRUINT323
	.align 4
	.type	records__DRUINT323, #function
	.proc	020
records__DRUINT323:
.LLFB795:
	save	%sp, -112, %sp
.LLCFI795:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE795:
	.size	records__DRUINT323, .-records__DRUINT323
	.align 4
	.type	records__ERUINT323, #function
	.proc	04
records__ERUINT323:
.LLFB796:
	save	%sp, -96, %sp
.LLCFI796:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE796:
	.size	records__ERUINT323, .-records__ERUINT323
	.align 4
	.type	records__FRUINT323, #function
	.proc	020
records__FRUINT323:
.LLFB797:
	save	%sp, -112, %sp
.LLCFI797:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE797:
	.size	records__FRUINT323, .-records__FRUINT323
	.align 4
	.type	records__ARUINT324, #function
	.proc	04
records__ARUINT324:
.LLFB798:
	save	%sp, -96, %sp
.LLCFI798:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE798:
	.size	records__ARUINT324, .-records__ARUINT324
	.align 4
	.type	records__BRUINT324, #function
	.proc	020
records__BRUINT324:
.LLFB799:
	save	%sp, -112, %sp
.LLCFI799:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE799:
	.size	records__BRUINT324, .-records__BRUINT324
	.align 4
	.type	records__CRUINT324, #function
	.proc	04
records__CRUINT324:
.LLFB800:
	save	%sp, -96, %sp
.LLCFI800:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE800:
	.size	records__CRUINT324, .-records__CRUINT324
	.align 4
	.type	records__DRUINT324, #function
	.proc	020
records__DRUINT324:
.LLFB801:
	save	%sp, -112, %sp
.LLCFI801:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE801:
	.size	records__DRUINT324, .-records__DRUINT324
	.align 4
	.type	records__ERUINT324, #function
	.proc	04
records__ERUINT324:
.LLFB802:
	save	%sp, -96, %sp
.LLCFI802:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE802:
	.size	records__ERUINT324, .-records__ERUINT324
	.align 4
	.type	records__FRUINT324, #function
	.proc	020
records__FRUINT324:
.LLFB803:
	save	%sp, -112, %sp
.LLCFI803:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE803:
	.size	records__FRUINT324, .-records__FRUINT324
	.align 4
	.type	records__ARUINT325, #function
	.proc	04
records__ARUINT325:
.LLFB804:
	save	%sp, -96, %sp
.LLCFI804:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE804:
	.size	records__ARUINT325, .-records__ARUINT325
	.align 4
	.type	records__BRUINT325, #function
	.proc	020
records__BRUINT325:
.LLFB805:
	save	%sp, -120, %sp
.LLCFI805:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE805:
	.size	records__BRUINT325, .-records__BRUINT325
	.align 4
	.type	records__CRUINT325, #function
	.proc	04
records__CRUINT325:
.LLFB806:
	save	%sp, -96, %sp
.LLCFI806:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE806:
	.size	records__CRUINT325, .-records__CRUINT325
	.align 4
	.type	records__DRUINT325, #function
	.proc	020
records__DRUINT325:
.LLFB807:
	save	%sp, -120, %sp
.LLCFI807:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE807:
	.size	records__DRUINT325, .-records__DRUINT325
	.align 4
	.type	records__ERUINT325, #function
	.proc	04
records__ERUINT325:
.LLFB808:
	save	%sp, -96, %sp
.LLCFI808:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE808:
	.size	records__ERUINT325, .-records__ERUINT325
	.align 4
	.type	records__FRUINT325, #function
	.proc	020
records__FRUINT325:
.LLFB809:
	save	%sp, -120, %sp
.LLCFI809:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE809:
	.size	records__FRUINT325, .-records__FRUINT325
	.align 4
	.type	records__ARUINT326, #function
	.proc	04
records__ARUINT326:
.LLFB810:
	save	%sp, -96, %sp
.LLCFI810:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE810:
	.size	records__ARUINT326, .-records__ARUINT326
	.align 4
	.type	records__BRUINT326, #function
	.proc	020
records__BRUINT326:
.LLFB811:
	save	%sp, -120, %sp
.LLCFI811:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE811:
	.size	records__BRUINT326, .-records__BRUINT326
	.align 4
	.type	records__CRUINT326, #function
	.proc	04
records__CRUINT326:
.LLFB812:
	save	%sp, -96, %sp
.LLCFI812:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE812:
	.size	records__CRUINT326, .-records__CRUINT326
	.align 4
	.type	records__DRUINT326, #function
	.proc	020
records__DRUINT326:
.LLFB813:
	save	%sp, -120, %sp
.LLCFI813:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE813:
	.size	records__DRUINT326, .-records__DRUINT326
	.align 4
	.type	records__ERUINT326, #function
	.proc	04
records__ERUINT326:
.LLFB814:
	save	%sp, -96, %sp
.LLCFI814:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE814:
	.size	records__ERUINT326, .-records__ERUINT326
	.align 4
	.type	records__FRUINT326, #function
	.proc	020
records__FRUINT326:
.LLFB815:
	save	%sp, -120, %sp
.LLCFI815:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE815:
	.size	records__FRUINT326, .-records__FRUINT326
	.align 4
	.type	records__ARUINT327, #function
	.proc	04
records__ARUINT327:
.LLFB816:
	save	%sp, -96, %sp
.LLCFI816:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE816:
	.size	records__ARUINT327, .-records__ARUINT327
	.align 4
	.type	records__BRUINT327, #function
	.proc	020
records__BRUINT327:
.LLFB817:
	save	%sp, -128, %sp
.LLCFI817:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE817:
	.size	records__BRUINT327, .-records__BRUINT327
	.align 4
	.type	records__CRUINT327, #function
	.proc	04
records__CRUINT327:
.LLFB818:
	save	%sp, -96, %sp
.LLCFI818:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE818:
	.size	records__CRUINT327, .-records__CRUINT327
	.align 4
	.type	records__DRUINT327, #function
	.proc	020
records__DRUINT327:
.LLFB819:
	save	%sp, -128, %sp
.LLCFI819:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE819:
	.size	records__DRUINT327, .-records__DRUINT327
	.align 4
	.type	records__ERUINT327, #function
	.proc	04
records__ERUINT327:
.LLFB820:
	save	%sp, -96, %sp
.LLCFI820:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE820:
	.size	records__ERUINT327, .-records__ERUINT327
	.align 4
	.type	records__FRUINT327, #function
	.proc	020
records__FRUINT327:
.LLFB821:
	save	%sp, -128, %sp
.LLCFI821:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE821:
	.size	records__FRUINT327, .-records__FRUINT327
	.align 4
	.type	records__ARUINT328, #function
	.proc	04
records__ARUINT328:
.LLFB822:
	save	%sp, -96, %sp
.LLCFI822:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE822:
	.size	records__ARUINT328, .-records__ARUINT328
	.align 4
	.type	records__BRUINT328, #function
	.proc	020
records__BRUINT328:
.LLFB823:
	save	%sp, -128, %sp
.LLCFI823:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE823:
	.size	records__BRUINT328, .-records__BRUINT328
	.align 4
	.type	records__CRUINT328, #function
	.proc	04
records__CRUINT328:
.LLFB824:
	save	%sp, -96, %sp
.LLCFI824:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE824:
	.size	records__CRUINT328, .-records__CRUINT328
	.align 4
	.type	records__DRUINT328, #function
	.proc	020
records__DRUINT328:
.LLFB825:
	save	%sp, -128, %sp
.LLCFI825:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE825:
	.size	records__DRUINT328, .-records__DRUINT328
	.align 4
	.type	records__ERUINT328, #function
	.proc	04
records__ERUINT328:
.LLFB826:
	save	%sp, -96, %sp
.LLCFI826:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE826:
	.size	records__ERUINT328, .-records__ERUINT328
	.align 4
	.type	records__FRUINT328, #function
	.proc	020
records__FRUINT328:
.LLFB827:
	save	%sp, -128, %sp
.LLCFI827:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE827:
	.size	records__FRUINT328, .-records__FRUINT328
	.align 4
	.type	records__ARUINT329, #function
	.proc	04
records__ARUINT329:
.LLFB828:
	save	%sp, -96, %sp
.LLCFI828:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE828:
	.size	records__ARUINT329, .-records__ARUINT329
	.align 4
	.type	records__BRUINT329, #function
	.proc	020
records__BRUINT329:
.LLFB829:
	save	%sp, -136, %sp
.LLCFI829:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE829:
	.size	records__BRUINT329, .-records__BRUINT329
	.align 4
	.type	records__CRUINT329, #function
	.proc	04
records__CRUINT329:
.LLFB830:
	save	%sp, -96, %sp
.LLCFI830:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE830:
	.size	records__CRUINT329, .-records__CRUINT329
	.align 4
	.type	records__DRUINT329, #function
	.proc	020
records__DRUINT329:
.LLFB831:
	save	%sp, -136, %sp
.LLCFI831:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE831:
	.size	records__DRUINT329, .-records__DRUINT329
	.align 4
	.type	records__ERUINT329, #function
	.proc	04
records__ERUINT329:
.LLFB832:
	save	%sp, -96, %sp
.LLCFI832:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE832:
	.size	records__ERUINT329, .-records__ERUINT329
	.align 4
	.type	records__FRUINT329, #function
	.proc	020
records__FRUINT329:
.LLFB833:
	save	%sp, -136, %sp
.LLCFI833:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE833:
	.size	records__FRUINT329, .-records__FRUINT329
	.align 4
	.type	records__ARUINT3210, #function
	.proc	04
records__ARUINT3210:
.LLFB834:
	save	%sp, -96, %sp
.LLCFI834:
	mov	%i0, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE834:
	.size	records__ARUINT3210, .-records__ARUINT3210
	.align 4
	.type	records__BRUINT3210, #function
	.proc	020
records__BRUINT3210:
.LLFB835:
	save	%sp, -136, %sp
.LLCFI835:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE835:
	.size	records__BRUINT3210, .-records__BRUINT3210
	.align 4
	.type	records__CRUINT3210, #function
	.proc	04
records__CRUINT3210:
.LLFB836:
	save	%sp, -96, %sp
.LLCFI836:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE836:
	.size	records__CRUINT3210, .-records__CRUINT3210
	.align 4
	.type	records__DRUINT3210, #function
	.proc	020
records__DRUINT3210:
.LLFB837:
	save	%sp, -136, %sp
.LLCFI837:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE837:
	.size	records__DRUINT3210, .-records__DRUINT3210
	.align 4
	.type	records__ERUINT3210, #function
	.proc	04
records__ERUINT3210:
.LLFB838:
	save	%sp, -96, %sp
.LLCFI838:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE838:
	.size	records__ERUINT3210, .-records__ERUINT3210
	.align 4
	.type	records__FRUINT3210, #function
	.proc	020
records__FRUINT3210:
.LLFB839:
	save	%sp, -136, %sp
.LLCFI839:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE839:
	.size	records__FRUINT3210, .-records__FRUINT3210
	.align 4
	.type	records__ARINT641, #function
	.proc	05
records__ARINT641:
.LLFB840:
	save	%sp, -96, %sp
.LLCFI840:
	mov	%i0, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE840:
	.size	records__ARINT641, .-records__ARINT641
	.align 4
	.type	records__BRINT641, #function
	.proc	020
records__BRINT641:
.LLFB841:
	save	%sp, -104, %sp
.LLCFI841:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE841:
	.size	records__BRINT641, .-records__BRINT641
	.align 4
	.type	records__CRINT641, #function
	.proc	05
records__CRINT641:
.LLFB842:
	save	%sp, -96, %sp
.LLCFI842:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE842:
	.size	records__CRINT641, .-records__CRINT641
	.align 4
	.type	records__DRINT641, #function
	.proc	020
records__DRINT641:
.LLFB843:
	save	%sp, -104, %sp
.LLCFI843:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE843:
	.size	records__DRINT641, .-records__DRINT641
	.align 4
	.type	records__ERINT641, #function
	.proc	05
records__ERINT641:
.LLFB844:
	save	%sp, -96, %sp
.LLCFI844:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE844:
	.size	records__ERINT641, .-records__ERINT641
	.align 4
	.type	records__FRINT641, #function
	.proc	020
records__FRINT641:
.LLFB845:
	save	%sp, -104, %sp
.LLCFI845:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE845:
	.size	records__FRINT641, .-records__FRINT641
	.align 4
	.type	records__ARINT642, #function
	.proc	05
records__ARINT642:
.LLFB846:
	save	%sp, -96, %sp
.LLCFI846:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE846:
	.size	records__ARINT642, .-records__ARINT642
	.align 4
	.type	records__BRINT642, #function
	.proc	020
records__BRINT642:
.LLFB847:
	save	%sp, -112, %sp
.LLCFI847:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE847:
	.size	records__BRINT642, .-records__BRINT642
	.align 4
	.type	records__CRINT642, #function
	.proc	05
records__CRINT642:
.LLFB848:
	save	%sp, -96, %sp
.LLCFI848:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE848:
	.size	records__CRINT642, .-records__CRINT642
	.align 4
	.type	records__DRINT642, #function
	.proc	020
records__DRINT642:
.LLFB849:
	save	%sp, -112, %sp
.LLCFI849:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE849:
	.size	records__DRINT642, .-records__DRINT642
	.align 4
	.type	records__ERINT642, #function
	.proc	05
records__ERINT642:
.LLFB850:
	save	%sp, -96, %sp
.LLCFI850:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE850:
	.size	records__ERINT642, .-records__ERINT642
	.align 4
	.type	records__FRINT642, #function
	.proc	020
records__FRINT642:
.LLFB851:
	save	%sp, -112, %sp
.LLCFI851:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE851:
	.size	records__FRINT642, .-records__FRINT642
	.align 4
	.type	records__ARINT643, #function
	.proc	05
records__ARINT643:
.LLFB852:
	save	%sp, -96, %sp
.LLCFI852:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE852:
	.size	records__ARINT643, .-records__ARINT643
	.align 4
	.type	records__BRINT643, #function
	.proc	020
records__BRINT643:
.LLFB853:
	save	%sp, -120, %sp
.LLCFI853:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE853:
	.size	records__BRINT643, .-records__BRINT643
	.align 4
	.type	records__CRINT643, #function
	.proc	05
records__CRINT643:
.LLFB854:
	save	%sp, -96, %sp
.LLCFI854:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE854:
	.size	records__CRINT643, .-records__CRINT643
	.align 4
	.type	records__DRINT643, #function
	.proc	020
records__DRINT643:
.LLFB855:
	save	%sp, -120, %sp
.LLCFI855:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE855:
	.size	records__DRINT643, .-records__DRINT643
	.align 4
	.type	records__ERINT643, #function
	.proc	05
records__ERINT643:
.LLFB856:
	save	%sp, -96, %sp
.LLCFI856:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE856:
	.size	records__ERINT643, .-records__ERINT643
	.align 4
	.type	records__FRINT643, #function
	.proc	020
records__FRINT643:
.LLFB857:
	save	%sp, -120, %sp
.LLCFI857:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE857:
	.size	records__FRINT643, .-records__FRINT643
	.align 4
	.type	records__ARINT644, #function
	.proc	05
records__ARINT644:
.LLFB858:
	save	%sp, -96, %sp
.LLCFI858:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE858:
	.size	records__ARINT644, .-records__ARINT644
	.align 4
	.type	records__BRINT644, #function
	.proc	020
records__BRINT644:
.LLFB859:
	save	%sp, -128, %sp
.LLCFI859:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE859:
	.size	records__BRINT644, .-records__BRINT644
	.align 4
	.type	records__CRINT644, #function
	.proc	05
records__CRINT644:
.LLFB860:
	save	%sp, -96, %sp
.LLCFI860:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE860:
	.size	records__CRINT644, .-records__CRINT644
	.align 4
	.type	records__DRINT644, #function
	.proc	020
records__DRINT644:
.LLFB861:
	save	%sp, -128, %sp
.LLCFI861:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE861:
	.size	records__DRINT644, .-records__DRINT644
	.align 4
	.type	records__ERINT644, #function
	.proc	05
records__ERINT644:
.LLFB862:
	save	%sp, -96, %sp
.LLCFI862:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE862:
	.size	records__ERINT644, .-records__ERINT644
	.align 4
	.type	records__FRINT644, #function
	.proc	020
records__FRINT644:
.LLFB863:
	save	%sp, -128, %sp
.LLCFI863:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE863:
	.size	records__FRINT644, .-records__FRINT644
	.align 4
	.type	records__ARINT645, #function
	.proc	05
records__ARINT645:
.LLFB864:
	save	%sp, -96, %sp
.LLCFI864:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE864:
	.size	records__ARINT645, .-records__ARINT645
	.align 4
	.type	records__BRINT645, #function
	.proc	020
records__BRINT645:
.LLFB865:
	save	%sp, -136, %sp
.LLCFI865:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE865:
	.size	records__BRINT645, .-records__BRINT645
	.align 4
	.type	records__CRINT645, #function
	.proc	05
records__CRINT645:
.LLFB866:
	save	%sp, -96, %sp
.LLCFI866:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE866:
	.size	records__CRINT645, .-records__CRINT645
	.align 4
	.type	records__DRINT645, #function
	.proc	020
records__DRINT645:
.LLFB867:
	save	%sp, -136, %sp
.LLCFI867:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE867:
	.size	records__DRINT645, .-records__DRINT645
	.align 4
	.type	records__ERINT645, #function
	.proc	05
records__ERINT645:
.LLFB868:
	save	%sp, -96, %sp
.LLCFI868:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE868:
	.size	records__ERINT645, .-records__ERINT645
	.align 4
	.type	records__FRINT645, #function
	.proc	020
records__FRINT645:
.LLFB869:
	save	%sp, -136, %sp
.LLCFI869:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE869:
	.size	records__FRINT645, .-records__FRINT645
	.align 4
	.type	records__ARINT646, #function
	.proc	05
records__ARINT646:
.LLFB870:
	save	%sp, -96, %sp
.LLCFI870:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE870:
	.size	records__ARINT646, .-records__ARINT646
	.align 4
	.type	records__BRINT646, #function
	.proc	020
records__BRINT646:
.LLFB871:
	save	%sp, -144, %sp
.LLCFI871:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE871:
	.size	records__BRINT646, .-records__BRINT646
	.align 4
	.type	records__CRINT646, #function
	.proc	05
records__CRINT646:
.LLFB872:
	save	%sp, -96, %sp
.LLCFI872:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE872:
	.size	records__CRINT646, .-records__CRINT646
	.align 4
	.type	records__DRINT646, #function
	.proc	020
records__DRINT646:
.LLFB873:
	save	%sp, -144, %sp
.LLCFI873:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE873:
	.size	records__DRINT646, .-records__DRINT646
	.align 4
	.type	records__ERINT646, #function
	.proc	05
records__ERINT646:
.LLFB874:
	save	%sp, -96, %sp
.LLCFI874:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE874:
	.size	records__ERINT646, .-records__ERINT646
	.align 4
	.type	records__FRINT646, #function
	.proc	020
records__FRINT646:
.LLFB875:
	save	%sp, -144, %sp
.LLCFI875:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE875:
	.size	records__FRINT646, .-records__FRINT646
	.align 4
	.type	records__ARINT647, #function
	.proc	05
records__ARINT647:
.LLFB876:
	save	%sp, -96, %sp
.LLCFI876:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE876:
	.size	records__ARINT647, .-records__ARINT647
	.align 4
	.type	records__BRINT647, #function
	.proc	020
records__BRINT647:
.LLFB877:
	save	%sp, -152, %sp
.LLCFI877:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE877:
	.size	records__BRINT647, .-records__BRINT647
	.align 4
	.type	records__CRINT647, #function
	.proc	05
records__CRINT647:
.LLFB878:
	save	%sp, -96, %sp
.LLCFI878:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE878:
	.size	records__CRINT647, .-records__CRINT647
	.align 4
	.type	records__DRINT647, #function
	.proc	020
records__DRINT647:
.LLFB879:
	save	%sp, -152, %sp
.LLCFI879:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE879:
	.size	records__DRINT647, .-records__DRINT647
	.align 4
	.type	records__ERINT647, #function
	.proc	05
records__ERINT647:
.LLFB880:
	save	%sp, -96, %sp
.LLCFI880:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE880:
	.size	records__ERINT647, .-records__ERINT647
	.align 4
	.type	records__FRINT647, #function
	.proc	020
records__FRINT647:
.LLFB881:
	save	%sp, -152, %sp
.LLCFI881:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE881:
	.size	records__FRINT647, .-records__FRINT647
	.align 4
	.type	records__ARINT648, #function
	.proc	05
records__ARINT648:
.LLFB882:
	save	%sp, -96, %sp
.LLCFI882:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE882:
	.size	records__ARINT648, .-records__ARINT648
	.align 4
	.type	records__BRINT648, #function
	.proc	020
records__BRINT648:
.LLFB883:
	save	%sp, -160, %sp
.LLCFI883:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE883:
	.size	records__BRINT648, .-records__BRINT648
	.align 4
	.type	records__CRINT648, #function
	.proc	05
records__CRINT648:
.LLFB884:
	save	%sp, -96, %sp
.LLCFI884:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE884:
	.size	records__CRINT648, .-records__CRINT648
	.align 4
	.type	records__DRINT648, #function
	.proc	020
records__DRINT648:
.LLFB885:
	save	%sp, -160, %sp
.LLCFI885:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE885:
	.size	records__DRINT648, .-records__DRINT648
	.align 4
	.type	records__ERINT648, #function
	.proc	05
records__ERINT648:
.LLFB886:
	save	%sp, -96, %sp
.LLCFI886:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE886:
	.size	records__ERINT648, .-records__ERINT648
	.align 4
	.type	records__FRINT648, #function
	.proc	020
records__FRINT648:
.LLFB887:
	save	%sp, -160, %sp
.LLCFI887:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE887:
	.size	records__FRINT648, .-records__FRINT648
	.align 4
	.type	records__ARINT649, #function
	.proc	05
records__ARINT649:
.LLFB888:
	save	%sp, -96, %sp
.LLCFI888:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE888:
	.size	records__ARINT649, .-records__ARINT649
	.align 4
	.type	records__BRINT649, #function
	.proc	020
records__BRINT649:
.LLFB889:
	save	%sp, -168, %sp
.LLCFI889:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE889:
	.size	records__BRINT649, .-records__BRINT649
	.align 4
	.type	records__CRINT649, #function
	.proc	05
records__CRINT649:
.LLFB890:
	save	%sp, -96, %sp
.LLCFI890:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE890:
	.size	records__CRINT649, .-records__CRINT649
	.align 4
	.type	records__DRINT649, #function
	.proc	020
records__DRINT649:
.LLFB891:
	save	%sp, -168, %sp
.LLCFI891:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE891:
	.size	records__DRINT649, .-records__DRINT649
	.align 4
	.type	records__ERINT649, #function
	.proc	05
records__ERINT649:
.LLFB892:
	save	%sp, -96, %sp
.LLCFI892:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE892:
	.size	records__ERINT649, .-records__ERINT649
	.align 4
	.type	records__FRINT649, #function
	.proc	020
records__FRINT649:
.LLFB893:
	save	%sp, -168, %sp
.LLCFI893:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE893:
	.size	records__FRINT649, .-records__FRINT649
	.align 4
	.type	records__ARINT6410, #function
	.proc	05
records__ARINT6410:
.LLFB894:
	save	%sp, -96, %sp
.LLCFI894:
	mov	%i0, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE894:
	.size	records__ARINT6410, .-records__ARINT6410
	.align 4
	.type	records__BRINT6410, #function
	.proc	020
records__BRINT6410:
.LLFB895:
	save	%sp, -176, %sp
.LLCFI895:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE895:
	.size	records__BRINT6410, .-records__BRINT6410
	.align 4
	.type	records__CRINT6410, #function
	.proc	05
records__CRINT6410:
.LLFB896:
	save	%sp, -96, %sp
.LLCFI896:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE896:
	.size	records__CRINT6410, .-records__CRINT6410
	.align 4
	.type	records__DRINT6410, #function
	.proc	020
records__DRINT6410:
.LLFB897:
	save	%sp, -176, %sp
.LLCFI897:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE897:
	.size	records__DRINT6410, .-records__DRINT6410
	.align 4
	.type	records__ERINT6410, #function
	.proc	05
records__ERINT6410:
.LLFB898:
	save	%sp, -96, %sp
.LLCFI898:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE898:
	.size	records__ERINT6410, .-records__ERINT6410
	.align 4
	.type	records__FRINT6410, #function
	.proc	020
records__FRINT6410:
.LLFB899:
	save	%sp, -176, %sp
.LLCFI899:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE899:
	.size	records__FRINT6410, .-records__FRINT6410
	.align 4
	.type	records__ARUINT641, #function
	.proc	05
records__ARUINT641:
.LLFB900:
	save	%sp, -96, %sp
.LLCFI900:
	mov	%i0, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE900:
	.size	records__ARUINT641, .-records__ARUINT641
	.align 4
	.type	records__BRUINT641, #function
	.proc	020
records__BRUINT641:
.LLFB901:
	save	%sp, -104, %sp
.LLCFI901:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE901:
	.size	records__BRUINT641, .-records__BRUINT641
	.align 4
	.type	records__CRUINT641, #function
	.proc	05
records__CRUINT641:
.LLFB902:
	save	%sp, -96, %sp
.LLCFI902:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE902:
	.size	records__CRUINT641, .-records__CRUINT641
	.align 4
	.type	records__DRUINT641, #function
	.proc	020
records__DRUINT641:
.LLFB903:
	save	%sp, -104, %sp
.LLCFI903:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE903:
	.size	records__DRUINT641, .-records__DRUINT641
	.align 4
	.type	records__ERUINT641, #function
	.proc	05
records__ERUINT641:
.LLFB904:
	save	%sp, -96, %sp
.LLCFI904:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE904:
	.size	records__ERUINT641, .-records__ERUINT641
	.align 4
	.type	records__FRUINT641, #function
	.proc	020
records__FRUINT641:
.LLFB905:
	save	%sp, -104, %sp
.LLCFI905:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE905:
	.size	records__FRUINT641, .-records__FRUINT641
	.align 4
	.type	records__ARUINT642, #function
	.proc	05
records__ARUINT642:
.LLFB906:
	save	%sp, -96, %sp
.LLCFI906:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE906:
	.size	records__ARUINT642, .-records__ARUINT642
	.align 4
	.type	records__BRUINT642, #function
	.proc	020
records__BRUINT642:
.LLFB907:
	save	%sp, -112, %sp
.LLCFI907:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE907:
	.size	records__BRUINT642, .-records__BRUINT642
	.align 4
	.type	records__CRUINT642, #function
	.proc	05
records__CRUINT642:
.LLFB908:
	save	%sp, -96, %sp
.LLCFI908:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE908:
	.size	records__CRUINT642, .-records__CRUINT642
	.align 4
	.type	records__DRUINT642, #function
	.proc	020
records__DRUINT642:
.LLFB909:
	save	%sp, -112, %sp
.LLCFI909:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE909:
	.size	records__DRUINT642, .-records__DRUINT642
	.align 4
	.type	records__ERUINT642, #function
	.proc	05
records__ERUINT642:
.LLFB910:
	save	%sp, -96, %sp
.LLCFI910:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE910:
	.size	records__ERUINT642, .-records__ERUINT642
	.align 4
	.type	records__FRUINT642, #function
	.proc	020
records__FRUINT642:
.LLFB911:
	save	%sp, -112, %sp
.LLCFI911:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE911:
	.size	records__FRUINT642, .-records__FRUINT642
	.align 4
	.type	records__ARUINT643, #function
	.proc	05
records__ARUINT643:
.LLFB912:
	save	%sp, -96, %sp
.LLCFI912:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE912:
	.size	records__ARUINT643, .-records__ARUINT643
	.align 4
	.type	records__BRUINT643, #function
	.proc	020
records__BRUINT643:
.LLFB913:
	save	%sp, -120, %sp
.LLCFI913:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE913:
	.size	records__BRUINT643, .-records__BRUINT643
	.align 4
	.type	records__CRUINT643, #function
	.proc	05
records__CRUINT643:
.LLFB914:
	save	%sp, -96, %sp
.LLCFI914:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE914:
	.size	records__CRUINT643, .-records__CRUINT643
	.align 4
	.type	records__DRUINT643, #function
	.proc	020
records__DRUINT643:
.LLFB915:
	save	%sp, -120, %sp
.LLCFI915:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE915:
	.size	records__DRUINT643, .-records__DRUINT643
	.align 4
	.type	records__ERUINT643, #function
	.proc	05
records__ERUINT643:
.LLFB916:
	save	%sp, -96, %sp
.LLCFI916:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE916:
	.size	records__ERUINT643, .-records__ERUINT643
	.align 4
	.type	records__FRUINT643, #function
	.proc	020
records__FRUINT643:
.LLFB917:
	save	%sp, -120, %sp
.LLCFI917:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE917:
	.size	records__FRUINT643, .-records__FRUINT643
	.align 4
	.type	records__ARUINT644, #function
	.proc	05
records__ARUINT644:
.LLFB918:
	save	%sp, -96, %sp
.LLCFI918:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE918:
	.size	records__ARUINT644, .-records__ARUINT644
	.align 4
	.type	records__BRUINT644, #function
	.proc	020
records__BRUINT644:
.LLFB919:
	save	%sp, -128, %sp
.LLCFI919:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE919:
	.size	records__BRUINT644, .-records__BRUINT644
	.align 4
	.type	records__CRUINT644, #function
	.proc	05
records__CRUINT644:
.LLFB920:
	save	%sp, -96, %sp
.LLCFI920:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE920:
	.size	records__CRUINT644, .-records__CRUINT644
	.align 4
	.type	records__DRUINT644, #function
	.proc	020
records__DRUINT644:
.LLFB921:
	save	%sp, -128, %sp
.LLCFI921:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE921:
	.size	records__DRUINT644, .-records__DRUINT644
	.align 4
	.type	records__ERUINT644, #function
	.proc	05
records__ERUINT644:
.LLFB922:
	save	%sp, -96, %sp
.LLCFI922:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE922:
	.size	records__ERUINT644, .-records__ERUINT644
	.align 4
	.type	records__FRUINT644, #function
	.proc	020
records__FRUINT644:
.LLFB923:
	save	%sp, -128, %sp
.LLCFI923:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE923:
	.size	records__FRUINT644, .-records__FRUINT644
	.align 4
	.type	records__ARUINT645, #function
	.proc	05
records__ARUINT645:
.LLFB924:
	save	%sp, -96, %sp
.LLCFI924:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE924:
	.size	records__ARUINT645, .-records__ARUINT645
	.align 4
	.type	records__BRUINT645, #function
	.proc	020
records__BRUINT645:
.LLFB925:
	save	%sp, -136, %sp
.LLCFI925:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE925:
	.size	records__BRUINT645, .-records__BRUINT645
	.align 4
	.type	records__CRUINT645, #function
	.proc	05
records__CRUINT645:
.LLFB926:
	save	%sp, -96, %sp
.LLCFI926:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE926:
	.size	records__CRUINT645, .-records__CRUINT645
	.align 4
	.type	records__DRUINT645, #function
	.proc	020
records__DRUINT645:
.LLFB927:
	save	%sp, -136, %sp
.LLCFI927:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE927:
	.size	records__DRUINT645, .-records__DRUINT645
	.align 4
	.type	records__ERUINT645, #function
	.proc	05
records__ERUINT645:
.LLFB928:
	save	%sp, -96, %sp
.LLCFI928:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE928:
	.size	records__ERUINT645, .-records__ERUINT645
	.align 4
	.type	records__FRUINT645, #function
	.proc	020
records__FRUINT645:
.LLFB929:
	save	%sp, -136, %sp
.LLCFI929:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE929:
	.size	records__FRUINT645, .-records__FRUINT645
	.align 4
	.type	records__ARUINT646, #function
	.proc	05
records__ARUINT646:
.LLFB930:
	save	%sp, -96, %sp
.LLCFI930:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE930:
	.size	records__ARUINT646, .-records__ARUINT646
	.align 4
	.type	records__BRUINT646, #function
	.proc	020
records__BRUINT646:
.LLFB931:
	save	%sp, -144, %sp
.LLCFI931:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE931:
	.size	records__BRUINT646, .-records__BRUINT646
	.align 4
	.type	records__CRUINT646, #function
	.proc	05
records__CRUINT646:
.LLFB932:
	save	%sp, -96, %sp
.LLCFI932:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE932:
	.size	records__CRUINT646, .-records__CRUINT646
	.align 4
	.type	records__DRUINT646, #function
	.proc	020
records__DRUINT646:
.LLFB933:
	save	%sp, -144, %sp
.LLCFI933:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE933:
	.size	records__DRUINT646, .-records__DRUINT646
	.align 4
	.type	records__ERUINT646, #function
	.proc	05
records__ERUINT646:
.LLFB934:
	save	%sp, -96, %sp
.LLCFI934:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE934:
	.size	records__ERUINT646, .-records__ERUINT646
	.align 4
	.type	records__FRUINT646, #function
	.proc	020
records__FRUINT646:
.LLFB935:
	save	%sp, -144, %sp
.LLCFI935:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE935:
	.size	records__FRUINT646, .-records__FRUINT646
	.align 4
	.type	records__ARUINT647, #function
	.proc	05
records__ARUINT647:
.LLFB936:
	save	%sp, -96, %sp
.LLCFI936:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE936:
	.size	records__ARUINT647, .-records__ARUINT647
	.align 4
	.type	records__BRUINT647, #function
	.proc	020
records__BRUINT647:
.LLFB937:
	save	%sp, -152, %sp
.LLCFI937:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE937:
	.size	records__BRUINT647, .-records__BRUINT647
	.align 4
	.type	records__CRUINT647, #function
	.proc	05
records__CRUINT647:
.LLFB938:
	save	%sp, -96, %sp
.LLCFI938:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE938:
	.size	records__CRUINT647, .-records__CRUINT647
	.align 4
	.type	records__DRUINT647, #function
	.proc	020
records__DRUINT647:
.LLFB939:
	save	%sp, -152, %sp
.LLCFI939:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE939:
	.size	records__DRUINT647, .-records__DRUINT647
	.align 4
	.type	records__ERUINT647, #function
	.proc	05
records__ERUINT647:
.LLFB940:
	save	%sp, -96, %sp
.LLCFI940:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE940:
	.size	records__ERUINT647, .-records__ERUINT647
	.align 4
	.type	records__FRUINT647, #function
	.proc	020
records__FRUINT647:
.LLFB941:
	save	%sp, -152, %sp
.LLCFI941:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE941:
	.size	records__FRUINT647, .-records__FRUINT647
	.align 4
	.type	records__ARUINT648, #function
	.proc	05
records__ARUINT648:
.LLFB942:
	save	%sp, -96, %sp
.LLCFI942:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE942:
	.size	records__ARUINT648, .-records__ARUINT648
	.align 4
	.type	records__BRUINT648, #function
	.proc	020
records__BRUINT648:
.LLFB943:
	save	%sp, -160, %sp
.LLCFI943:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE943:
	.size	records__BRUINT648, .-records__BRUINT648
	.align 4
	.type	records__CRUINT648, #function
	.proc	05
records__CRUINT648:
.LLFB944:
	save	%sp, -96, %sp
.LLCFI944:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE944:
	.size	records__CRUINT648, .-records__CRUINT648
	.align 4
	.type	records__DRUINT648, #function
	.proc	020
records__DRUINT648:
.LLFB945:
	save	%sp, -160, %sp
.LLCFI945:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE945:
	.size	records__DRUINT648, .-records__DRUINT648
	.align 4
	.type	records__ERUINT648, #function
	.proc	05
records__ERUINT648:
.LLFB946:
	save	%sp, -96, %sp
.LLCFI946:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE946:
	.size	records__ERUINT648, .-records__ERUINT648
	.align 4
	.type	records__FRUINT648, #function
	.proc	020
records__FRUINT648:
.LLFB947:
	save	%sp, -160, %sp
.LLCFI947:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE947:
	.size	records__FRUINT648, .-records__FRUINT648
	.align 4
	.type	records__ARUINT649, #function
	.proc	05
records__ARUINT649:
.LLFB948:
	save	%sp, -96, %sp
.LLCFI948:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE948:
	.size	records__ARUINT649, .-records__ARUINT649
	.align 4
	.type	records__BRUINT649, #function
	.proc	020
records__BRUINT649:
.LLFB949:
	save	%sp, -168, %sp
.LLCFI949:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE949:
	.size	records__BRUINT649, .-records__BRUINT649
	.align 4
	.type	records__CRUINT649, #function
	.proc	05
records__CRUINT649:
.LLFB950:
	save	%sp, -96, %sp
.LLCFI950:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE950:
	.size	records__CRUINT649, .-records__CRUINT649
	.align 4
	.type	records__DRUINT649, #function
	.proc	020
records__DRUINT649:
.LLFB951:
	save	%sp, -168, %sp
.LLCFI951:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE951:
	.size	records__DRUINT649, .-records__DRUINT649
	.align 4
	.type	records__ERUINT649, #function
	.proc	05
records__ERUINT649:
.LLFB952:
	save	%sp, -96, %sp
.LLCFI952:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE952:
	.size	records__ERUINT649, .-records__ERUINT649
	.align 4
	.type	records__FRUINT649, #function
	.proc	020
records__FRUINT649:
.LLFB953:
	save	%sp, -168, %sp
.LLCFI953:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE953:
	.size	records__FRUINT649, .-records__FRUINT649
	.align 4
	.type	records__ARUINT6410, #function
	.proc	05
records__ARUINT6410:
.LLFB954:
	save	%sp, -96, %sp
.LLCFI954:
	mov	%i0, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE954:
	.size	records__ARUINT6410, .-records__ARUINT6410
	.align 4
	.type	records__BRUINT6410, #function
	.proc	020
records__BRUINT6410:
.LLFB955:
	save	%sp, -176, %sp
.LLCFI955:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE955:
	.size	records__BRUINT6410, .-records__BRUINT6410
	.align 4
	.type	records__CRUINT6410, #function
	.proc	05
records__CRUINT6410:
.LLFB956:
	save	%sp, -96, %sp
.LLCFI956:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE956:
	.size	records__CRUINT6410, .-records__CRUINT6410
	.align 4
	.type	records__DRUINT6410, #function
	.proc	020
records__DRUINT6410:
.LLFB957:
	save	%sp, -176, %sp
.LLCFI957:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE957:
	.size	records__DRUINT6410, .-records__DRUINT6410
	.align 4
	.type	records__ERUINT6410, #function
	.proc	05
records__ERUINT6410:
.LLFB958:
	save	%sp, -96, %sp
.LLCFI958:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE958:
	.size	records__ERUINT6410, .-records__ERUINT6410
	.align 4
	.type	records__FRUINT6410, #function
	.proc	020
records__FRUINT6410:
.LLFB959:
	save	%sp, -176, %sp
.LLCFI959:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE959:
	.size	records__FRUINT6410, .-records__FRUINT6410
	.align 4
	.type	records__ARINTEGER1, #function
	.proc	04
records__ARINTEGER1:
.LLFB960:
	save	%sp, -96, %sp
.LLCFI960:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE960:
	.size	records__ARINTEGER1, .-records__ARINTEGER1
	.align 4
	.type	records__BRINTEGER1, #function
	.proc	020
records__BRINTEGER1:
.LLFB961:
	save	%sp, -104, %sp
.LLCFI961:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE961:
	.size	records__BRINTEGER1, .-records__BRINTEGER1
	.align 4
	.type	records__CRINTEGER1, #function
	.proc	04
records__CRINTEGER1:
.LLFB962:
	save	%sp, -96, %sp
.LLCFI962:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE962:
	.size	records__CRINTEGER1, .-records__CRINTEGER1
	.align 4
	.type	records__DRINTEGER1, #function
	.proc	020
records__DRINTEGER1:
.LLFB963:
	save	%sp, -104, %sp
.LLCFI963:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE963:
	.size	records__DRINTEGER1, .-records__DRINTEGER1
	.align 4
	.type	records__ERINTEGER1, #function
	.proc	04
records__ERINTEGER1:
.LLFB964:
	save	%sp, -96, %sp
.LLCFI964:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE964:
	.size	records__ERINTEGER1, .-records__ERINTEGER1
	.align 4
	.type	records__FRINTEGER1, #function
	.proc	020
records__FRINTEGER1:
.LLFB965:
	save	%sp, -104, %sp
.LLCFI965:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE965:
	.size	records__FRINTEGER1, .-records__FRINTEGER1
	.align 4
	.type	records__ARINTEGER2, #function
	.proc	04
records__ARINTEGER2:
.LLFB966:
	save	%sp, -96, %sp
.LLCFI966:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE966:
	.size	records__ARINTEGER2, .-records__ARINTEGER2
	.align 4
	.type	records__BRINTEGER2, #function
	.proc	020
records__BRINTEGER2:
.LLFB967:
	save	%sp, -104, %sp
.LLCFI967:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE967:
	.size	records__BRINTEGER2, .-records__BRINTEGER2
	.align 4
	.type	records__CRINTEGER2, #function
	.proc	04
records__CRINTEGER2:
.LLFB968:
	save	%sp, -96, %sp
.LLCFI968:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE968:
	.size	records__CRINTEGER2, .-records__CRINTEGER2
	.align 4
	.type	records__DRINTEGER2, #function
	.proc	020
records__DRINTEGER2:
.LLFB969:
	save	%sp, -104, %sp
.LLCFI969:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE969:
	.size	records__DRINTEGER2, .-records__DRINTEGER2
	.align 4
	.type	records__ERINTEGER2, #function
	.proc	04
records__ERINTEGER2:
.LLFB970:
	save	%sp, -96, %sp
.LLCFI970:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE970:
	.size	records__ERINTEGER2, .-records__ERINTEGER2
	.align 4
	.type	records__FRINTEGER2, #function
	.proc	020
records__FRINTEGER2:
.LLFB971:
	save	%sp, -104, %sp
.LLCFI971:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE971:
	.size	records__FRINTEGER2, .-records__FRINTEGER2
	.align 4
	.type	records__ARINTEGER3, #function
	.proc	04
records__ARINTEGER3:
.LLFB972:
	save	%sp, -96, %sp
.LLCFI972:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE972:
	.size	records__ARINTEGER3, .-records__ARINTEGER3
	.align 4
	.type	records__BRINTEGER3, #function
	.proc	020
records__BRINTEGER3:
.LLFB973:
	save	%sp, -112, %sp
.LLCFI973:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE973:
	.size	records__BRINTEGER3, .-records__BRINTEGER3
	.align 4
	.type	records__CRINTEGER3, #function
	.proc	04
records__CRINTEGER3:
.LLFB974:
	save	%sp, -96, %sp
.LLCFI974:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE974:
	.size	records__CRINTEGER3, .-records__CRINTEGER3
	.align 4
	.type	records__DRINTEGER3, #function
	.proc	020
records__DRINTEGER3:
.LLFB975:
	save	%sp, -112, %sp
.LLCFI975:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE975:
	.size	records__DRINTEGER3, .-records__DRINTEGER3
	.align 4
	.type	records__ERINTEGER3, #function
	.proc	04
records__ERINTEGER3:
.LLFB976:
	save	%sp, -96, %sp
.LLCFI976:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE976:
	.size	records__ERINTEGER3, .-records__ERINTEGER3
	.align 4
	.type	records__FRINTEGER3, #function
	.proc	020
records__FRINTEGER3:
.LLFB977:
	save	%sp, -112, %sp
.LLCFI977:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE977:
	.size	records__FRINTEGER3, .-records__FRINTEGER3
	.align 4
	.type	records__ARINTEGER4, #function
	.proc	04
records__ARINTEGER4:
.LLFB978:
	save	%sp, -96, %sp
.LLCFI978:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE978:
	.size	records__ARINTEGER4, .-records__ARINTEGER4
	.align 4
	.type	records__BRINTEGER4, #function
	.proc	020
records__BRINTEGER4:
.LLFB979:
	save	%sp, -112, %sp
.LLCFI979:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE979:
	.size	records__BRINTEGER4, .-records__BRINTEGER4
	.align 4
	.type	records__CRINTEGER4, #function
	.proc	04
records__CRINTEGER4:
.LLFB980:
	save	%sp, -96, %sp
.LLCFI980:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE980:
	.size	records__CRINTEGER4, .-records__CRINTEGER4
	.align 4
	.type	records__DRINTEGER4, #function
	.proc	020
records__DRINTEGER4:
.LLFB981:
	save	%sp, -112, %sp
.LLCFI981:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE981:
	.size	records__DRINTEGER4, .-records__DRINTEGER4
	.align 4
	.type	records__ERINTEGER4, #function
	.proc	04
records__ERINTEGER4:
.LLFB982:
	save	%sp, -96, %sp
.LLCFI982:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE982:
	.size	records__ERINTEGER4, .-records__ERINTEGER4
	.align 4
	.type	records__FRINTEGER4, #function
	.proc	020
records__FRINTEGER4:
.LLFB983:
	save	%sp, -112, %sp
.LLCFI983:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE983:
	.size	records__FRINTEGER4, .-records__FRINTEGER4
	.align 4
	.type	records__ARINTEGER5, #function
	.proc	04
records__ARINTEGER5:
.LLFB984:
	save	%sp, -96, %sp
.LLCFI984:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE984:
	.size	records__ARINTEGER5, .-records__ARINTEGER5
	.align 4
	.type	records__BRINTEGER5, #function
	.proc	020
records__BRINTEGER5:
.LLFB985:
	save	%sp, -120, %sp
.LLCFI985:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE985:
	.size	records__BRINTEGER5, .-records__BRINTEGER5
	.align 4
	.type	records__CRINTEGER5, #function
	.proc	04
records__CRINTEGER5:
.LLFB986:
	save	%sp, -96, %sp
.LLCFI986:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE986:
	.size	records__CRINTEGER5, .-records__CRINTEGER5
	.align 4
	.type	records__DRINTEGER5, #function
	.proc	020
records__DRINTEGER5:
.LLFB987:
	save	%sp, -120, %sp
.LLCFI987:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE987:
	.size	records__DRINTEGER5, .-records__DRINTEGER5
	.align 4
	.type	records__ERINTEGER5, #function
	.proc	04
records__ERINTEGER5:
.LLFB988:
	save	%sp, -96, %sp
.LLCFI988:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE988:
	.size	records__ERINTEGER5, .-records__ERINTEGER5
	.align 4
	.type	records__FRINTEGER5, #function
	.proc	020
records__FRINTEGER5:
.LLFB989:
	save	%sp, -120, %sp
.LLCFI989:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE989:
	.size	records__FRINTEGER5, .-records__FRINTEGER5
	.align 4
	.type	records__ARINTEGER6, #function
	.proc	04
records__ARINTEGER6:
.LLFB990:
	save	%sp, -96, %sp
.LLCFI990:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE990:
	.size	records__ARINTEGER6, .-records__ARINTEGER6
	.align 4
	.type	records__BRINTEGER6, #function
	.proc	020
records__BRINTEGER6:
.LLFB991:
	save	%sp, -120, %sp
.LLCFI991:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE991:
	.size	records__BRINTEGER6, .-records__BRINTEGER6
	.align 4
	.type	records__CRINTEGER6, #function
	.proc	04
records__CRINTEGER6:
.LLFB992:
	save	%sp, -96, %sp
.LLCFI992:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE992:
	.size	records__CRINTEGER6, .-records__CRINTEGER6
	.align 4
	.type	records__DRINTEGER6, #function
	.proc	020
records__DRINTEGER6:
.LLFB993:
	save	%sp, -120, %sp
.LLCFI993:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE993:
	.size	records__DRINTEGER6, .-records__DRINTEGER6
	.align 4
	.type	records__ERINTEGER6, #function
	.proc	04
records__ERINTEGER6:
.LLFB994:
	save	%sp, -96, %sp
.LLCFI994:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE994:
	.size	records__ERINTEGER6, .-records__ERINTEGER6
	.align 4
	.type	records__FRINTEGER6, #function
	.proc	020
records__FRINTEGER6:
.LLFB995:
	save	%sp, -120, %sp
.LLCFI995:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE995:
	.size	records__FRINTEGER6, .-records__FRINTEGER6
	.align 4
	.type	records__ARINTEGER7, #function
	.proc	04
records__ARINTEGER7:
.LLFB996:
	save	%sp, -96, %sp
.LLCFI996:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE996:
	.size	records__ARINTEGER7, .-records__ARINTEGER7
	.align 4
	.type	records__BRINTEGER7, #function
	.proc	020
records__BRINTEGER7:
.LLFB997:
	save	%sp, -128, %sp
.LLCFI997:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE997:
	.size	records__BRINTEGER7, .-records__BRINTEGER7
	.align 4
	.type	records__CRINTEGER7, #function
	.proc	04
records__CRINTEGER7:
.LLFB998:
	save	%sp, -96, %sp
.LLCFI998:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE998:
	.size	records__CRINTEGER7, .-records__CRINTEGER7
	.align 4
	.type	records__DRINTEGER7, #function
	.proc	020
records__DRINTEGER7:
.LLFB999:
	save	%sp, -128, %sp
.LLCFI999:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE999:
	.size	records__DRINTEGER7, .-records__DRINTEGER7
	.align 4
	.type	records__ERINTEGER7, #function
	.proc	04
records__ERINTEGER7:
.LLFB1000:
	save	%sp, -96, %sp
.LLCFI1000:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1000:
	.size	records__ERINTEGER7, .-records__ERINTEGER7
	.align 4
	.type	records__FRINTEGER7, #function
	.proc	020
records__FRINTEGER7:
.LLFB1001:
	save	%sp, -128, %sp
.LLCFI1001:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1001:
	.size	records__FRINTEGER7, .-records__FRINTEGER7
	.align 4
	.type	records__ARINTEGER8, #function
	.proc	04
records__ARINTEGER8:
.LLFB1002:
	save	%sp, -96, %sp
.LLCFI1002:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1002:
	.size	records__ARINTEGER8, .-records__ARINTEGER8
	.align 4
	.type	records__BRINTEGER8, #function
	.proc	020
records__BRINTEGER8:
.LLFB1003:
	save	%sp, -128, %sp
.LLCFI1003:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1003:
	.size	records__BRINTEGER8, .-records__BRINTEGER8
	.align 4
	.type	records__CRINTEGER8, #function
	.proc	04
records__CRINTEGER8:
.LLFB1004:
	save	%sp, -96, %sp
.LLCFI1004:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1004:
	.size	records__CRINTEGER8, .-records__CRINTEGER8
	.align 4
	.type	records__DRINTEGER8, #function
	.proc	020
records__DRINTEGER8:
.LLFB1005:
	save	%sp, -128, %sp
.LLCFI1005:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1005:
	.size	records__DRINTEGER8, .-records__DRINTEGER8
	.align 4
	.type	records__ERINTEGER8, #function
	.proc	04
records__ERINTEGER8:
.LLFB1006:
	save	%sp, -96, %sp
.LLCFI1006:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1006:
	.size	records__ERINTEGER8, .-records__ERINTEGER8
	.align 4
	.type	records__FRINTEGER8, #function
	.proc	020
records__FRINTEGER8:
.LLFB1007:
	save	%sp, -128, %sp
.LLCFI1007:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1007:
	.size	records__FRINTEGER8, .-records__FRINTEGER8
	.align 4
	.type	records__ARINTEGER9, #function
	.proc	04
records__ARINTEGER9:
.LLFB1008:
	save	%sp, -96, %sp
.LLCFI1008:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1008:
	.size	records__ARINTEGER9, .-records__ARINTEGER9
	.align 4
	.type	records__BRINTEGER9, #function
	.proc	020
records__BRINTEGER9:
.LLFB1009:
	save	%sp, -136, %sp
.LLCFI1009:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1009:
	.size	records__BRINTEGER9, .-records__BRINTEGER9
	.align 4
	.type	records__CRINTEGER9, #function
	.proc	04
records__CRINTEGER9:
.LLFB1010:
	save	%sp, -96, %sp
.LLCFI1010:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1010:
	.size	records__CRINTEGER9, .-records__CRINTEGER9
	.align 4
	.type	records__DRINTEGER9, #function
	.proc	020
records__DRINTEGER9:
.LLFB1011:
	save	%sp, -136, %sp
.LLCFI1011:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1011:
	.size	records__DRINTEGER9, .-records__DRINTEGER9
	.align 4
	.type	records__ERINTEGER9, #function
	.proc	04
records__ERINTEGER9:
.LLFB1012:
	save	%sp, -96, %sp
.LLCFI1012:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1012:
	.size	records__ERINTEGER9, .-records__ERINTEGER9
	.align 4
	.type	records__FRINTEGER9, #function
	.proc	020
records__FRINTEGER9:
.LLFB1013:
	save	%sp, -136, %sp
.LLCFI1013:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1013:
	.size	records__FRINTEGER9, .-records__FRINTEGER9
	.align 4
	.type	records__ARINTEGER10, #function
	.proc	04
records__ARINTEGER10:
.LLFB1014:
	save	%sp, -96, %sp
.LLCFI1014:
	mov	%i0, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1014:
	.size	records__ARINTEGER10, .-records__ARINTEGER10
	.align 4
	.type	records__BRINTEGER10, #function
	.proc	020
records__BRINTEGER10:
.LLFB1015:
	save	%sp, -136, %sp
.LLCFI1015:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1015:
	.size	records__BRINTEGER10, .-records__BRINTEGER10
	.align 4
	.type	records__CRINTEGER10, #function
	.proc	04
records__CRINTEGER10:
.LLFB1016:
	save	%sp, -96, %sp
.LLCFI1016:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1016:
	.size	records__CRINTEGER10, .-records__CRINTEGER10
	.align 4
	.type	records__DRINTEGER10, #function
	.proc	020
records__DRINTEGER10:
.LLFB1017:
	save	%sp, -136, %sp
.LLCFI1017:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1017:
	.size	records__DRINTEGER10, .-records__DRINTEGER10
	.align 4
	.type	records__ERINTEGER10, #function
	.proc	04
records__ERINTEGER10:
.LLFB1018:
	save	%sp, -96, %sp
.LLCFI1018:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1018:
	.size	records__ERINTEGER10, .-records__ERINTEGER10
	.align 4
	.type	records__FRINTEGER10, #function
	.proc	020
records__FRINTEGER10:
.LLFB1019:
	save	%sp, -136, %sp
.LLCFI1019:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1019:
	.size	records__FRINTEGER10, .-records__FRINTEGER10
	.align 4
	.type	records__ARCARDINAL1, #function
	.proc	016
records__ARCARDINAL1:
.LLFB1020:
	save	%sp, -96, %sp
.LLCFI1020:
	mov	%i0, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1020:
	.size	records__ARCARDINAL1, .-records__ARCARDINAL1
	.align 4
	.type	records__BRCARDINAL1, #function
	.proc	020
records__BRCARDINAL1:
.LLFB1021:
	save	%sp, -104, %sp
.LLCFI1021:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1021:
	.size	records__BRCARDINAL1, .-records__BRCARDINAL1
	.align 4
	.type	records__CRCARDINAL1, #function
	.proc	016
records__CRCARDINAL1:
.LLFB1022:
	save	%sp, -96, %sp
.LLCFI1022:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1022:
	.size	records__CRCARDINAL1, .-records__CRCARDINAL1
	.align 4
	.type	records__DRCARDINAL1, #function
	.proc	020
records__DRCARDINAL1:
.LLFB1023:
	save	%sp, -104, %sp
.LLCFI1023:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1023:
	.size	records__DRCARDINAL1, .-records__DRCARDINAL1
	.align 4
	.type	records__ERCARDINAL1, #function
	.proc	016
records__ERCARDINAL1:
.LLFB1024:
	save	%sp, -96, %sp
.LLCFI1024:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1024:
	.size	records__ERCARDINAL1, .-records__ERCARDINAL1
	.align 4
	.type	records__FRCARDINAL1, #function
	.proc	020
records__FRCARDINAL1:
.LLFB1025:
	save	%sp, -104, %sp
.LLCFI1025:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1025:
	.size	records__FRCARDINAL1, .-records__FRCARDINAL1
	.align 4
	.type	records__ARCARDINAL2, #function
	.proc	016
records__ARCARDINAL2:
.LLFB1026:
	save	%sp, -96, %sp
.LLCFI1026:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1026:
	.size	records__ARCARDINAL2, .-records__ARCARDINAL2
	.align 4
	.type	records__BRCARDINAL2, #function
	.proc	020
records__BRCARDINAL2:
.LLFB1027:
	save	%sp, -104, %sp
.LLCFI1027:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1027:
	.size	records__BRCARDINAL2, .-records__BRCARDINAL2
	.align 4
	.type	records__CRCARDINAL2, #function
	.proc	016
records__CRCARDINAL2:
.LLFB1028:
	save	%sp, -96, %sp
.LLCFI1028:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1028:
	.size	records__CRCARDINAL2, .-records__CRCARDINAL2
	.align 4
	.type	records__DRCARDINAL2, #function
	.proc	020
records__DRCARDINAL2:
.LLFB1029:
	save	%sp, -104, %sp
.LLCFI1029:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1029:
	.size	records__DRCARDINAL2, .-records__DRCARDINAL2
	.align 4
	.type	records__ERCARDINAL2, #function
	.proc	016
records__ERCARDINAL2:
.LLFB1030:
	save	%sp, -96, %sp
.LLCFI1030:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1030:
	.size	records__ERCARDINAL2, .-records__ERCARDINAL2
	.align 4
	.type	records__FRCARDINAL2, #function
	.proc	020
records__FRCARDINAL2:
.LLFB1031:
	save	%sp, -104, %sp
.LLCFI1031:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1031:
	.size	records__FRCARDINAL2, .-records__FRCARDINAL2
	.align 4
	.type	records__ARCARDINAL3, #function
	.proc	016
records__ARCARDINAL3:
.LLFB1032:
	save	%sp, -96, %sp
.LLCFI1032:
	mov	%i0, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1032:
	.size	records__ARCARDINAL3, .-records__ARCARDINAL3
	.align 4
	.type	records__BRCARDINAL3, #function
	.proc	020
records__BRCARDINAL3:
.LLFB1033:
	save	%sp, -112, %sp
.LLCFI1033:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1033:
	.size	records__BRCARDINAL3, .-records__BRCARDINAL3
	.align 4
	.type	records__CRCARDINAL3, #function
	.proc	016
records__CRCARDINAL3:
.LLFB1034:
	save	%sp, -96, %sp
.LLCFI1034:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1034:
	.size	records__CRCARDINAL3, .-records__CRCARDINAL3
	.align 4
	.type	records__DRCARDINAL3, #function
	.proc	020
records__DRCARDINAL3:
.LLFB1035:
	save	%sp, -112, %sp
.LLCFI1035:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1035:
	.size	records__DRCARDINAL3, .-records__DRCARDINAL3
	.align 4
	.type	records__ERCARDINAL3, #function
	.proc	016
records__ERCARDINAL3:
.LLFB1036:
	save	%sp, -96, %sp
.LLCFI1036:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+4], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1036:
	.size	records__ERCARDINAL3, .-records__ERCARDINAL3
	.align 4
	.type	records__FRCARDINAL3, #function
	.proc	020
records__FRCARDINAL3:
.LLFB1037:
	save	%sp, -112, %sp
.LLCFI1037:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1037:
	.size	records__FRCARDINAL3, .-records__FRCARDINAL3
	.align 4
	.type	records__ARCARDINAL4, #function
	.proc	016
records__ARCARDINAL4:
.LLFB1038:
	save	%sp, -96, %sp
.LLCFI1038:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1038:
	.size	records__ARCARDINAL4, .-records__ARCARDINAL4
	.align 4
	.type	records__BRCARDINAL4, #function
	.proc	020
records__BRCARDINAL4:
.LLFB1039:
	save	%sp, -112, %sp
.LLCFI1039:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1039:
	.size	records__BRCARDINAL4, .-records__BRCARDINAL4
	.align 4
	.type	records__CRCARDINAL4, #function
	.proc	016
records__CRCARDINAL4:
.LLFB1040:
	save	%sp, -96, %sp
.LLCFI1040:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1040:
	.size	records__CRCARDINAL4, .-records__CRCARDINAL4
	.align 4
	.type	records__DRCARDINAL4, #function
	.proc	020
records__DRCARDINAL4:
.LLFB1041:
	save	%sp, -112, %sp
.LLCFI1041:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1041:
	.size	records__DRCARDINAL4, .-records__DRCARDINAL4
	.align 4
	.type	records__ERCARDINAL4, #function
	.proc	016
records__ERCARDINAL4:
.LLFB1042:
	save	%sp, -96, %sp
.LLCFI1042:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1042:
	.size	records__ERCARDINAL4, .-records__ERCARDINAL4
	.align 4
	.type	records__FRCARDINAL4, #function
	.proc	020
records__FRCARDINAL4:
.LLFB1043:
	save	%sp, -112, %sp
.LLCFI1043:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1043:
	.size	records__FRCARDINAL4, .-records__FRCARDINAL4
	.align 4
	.type	records__ARCARDINAL5, #function
	.proc	016
records__ARCARDINAL5:
.LLFB1044:
	save	%sp, -96, %sp
.LLCFI1044:
	mov	%i0, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1044:
	.size	records__ARCARDINAL5, .-records__ARCARDINAL5
	.align 4
	.type	records__BRCARDINAL5, #function
	.proc	020
records__BRCARDINAL5:
.LLFB1045:
	save	%sp, -120, %sp
.LLCFI1045:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1045:
	.size	records__BRCARDINAL5, .-records__BRCARDINAL5
	.align 4
	.type	records__CRCARDINAL5, #function
	.proc	016
records__CRCARDINAL5:
.LLFB1046:
	save	%sp, -96, %sp
.LLCFI1046:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1046:
	.size	records__CRCARDINAL5, .-records__CRCARDINAL5
	.align 4
	.type	records__DRCARDINAL5, #function
	.proc	020
records__DRCARDINAL5:
.LLFB1047:
	save	%sp, -120, %sp
.LLCFI1047:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1047:
	.size	records__DRCARDINAL5, .-records__DRCARDINAL5
	.align 4
	.type	records__ERCARDINAL5, #function
	.proc	016
records__ERCARDINAL5:
.LLFB1048:
	save	%sp, -96, %sp
.LLCFI1048:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+8], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1048:
	.size	records__ERCARDINAL5, .-records__ERCARDINAL5
	.align 4
	.type	records__FRCARDINAL5, #function
	.proc	020
records__FRCARDINAL5:
.LLFB1049:
	save	%sp, -120, %sp
.LLCFI1049:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1049:
	.size	records__FRCARDINAL5, .-records__FRCARDINAL5
	.align 4
	.type	records__ARCARDINAL6, #function
	.proc	016
records__ARCARDINAL6:
.LLFB1050:
	save	%sp, -96, %sp
.LLCFI1050:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1050:
	.size	records__ARCARDINAL6, .-records__ARCARDINAL6
	.align 4
	.type	records__BRCARDINAL6, #function
	.proc	020
records__BRCARDINAL6:
.LLFB1051:
	save	%sp, -120, %sp
.LLCFI1051:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1051:
	.size	records__BRCARDINAL6, .-records__BRCARDINAL6
	.align 4
	.type	records__CRCARDINAL6, #function
	.proc	016
records__CRCARDINAL6:
.LLFB1052:
	save	%sp, -96, %sp
.LLCFI1052:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1052:
	.size	records__CRCARDINAL6, .-records__CRCARDINAL6
	.align 4
	.type	records__DRCARDINAL6, #function
	.proc	020
records__DRCARDINAL6:
.LLFB1053:
	save	%sp, -120, %sp
.LLCFI1053:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1053:
	.size	records__DRCARDINAL6, .-records__DRCARDINAL6
	.align 4
	.type	records__ERCARDINAL6, #function
	.proc	016
records__ERCARDINAL6:
.LLFB1054:
	save	%sp, -96, %sp
.LLCFI1054:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1054:
	.size	records__ERCARDINAL6, .-records__ERCARDINAL6
	.align 4
	.type	records__FRCARDINAL6, #function
	.proc	020
records__FRCARDINAL6:
.LLFB1055:
	save	%sp, -120, %sp
.LLCFI1055:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1055:
	.size	records__FRCARDINAL6, .-records__FRCARDINAL6
	.align 4
	.type	records__ARCARDINAL7, #function
	.proc	016
records__ARCARDINAL7:
.LLFB1056:
	save	%sp, -96, %sp
.LLCFI1056:
	mov	%i0, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1056:
	.size	records__ARCARDINAL7, .-records__ARCARDINAL7
	.align 4
	.type	records__BRCARDINAL7, #function
	.proc	020
records__BRCARDINAL7:
.LLFB1057:
	save	%sp, -128, %sp
.LLCFI1057:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1057:
	.size	records__BRCARDINAL7, .-records__BRCARDINAL7
	.align 4
	.type	records__CRCARDINAL7, #function
	.proc	016
records__CRCARDINAL7:
.LLFB1058:
	save	%sp, -96, %sp
.LLCFI1058:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1058:
	.size	records__CRCARDINAL7, .-records__CRCARDINAL7
	.align 4
	.type	records__DRCARDINAL7, #function
	.proc	020
records__DRCARDINAL7:
.LLFB1059:
	save	%sp, -128, %sp
.LLCFI1059:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1059:
	.size	records__DRCARDINAL7, .-records__DRCARDINAL7
	.align 4
	.type	records__ERCARDINAL7, #function
	.proc	016
records__ERCARDINAL7:
.LLFB1060:
	save	%sp, -96, %sp
.LLCFI1060:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+12], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1060:
	.size	records__ERCARDINAL7, .-records__ERCARDINAL7
	.align 4
	.type	records__FRCARDINAL7, #function
	.proc	020
records__FRCARDINAL7:
.LLFB1061:
	save	%sp, -128, %sp
.LLCFI1061:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1061:
	.size	records__FRCARDINAL7, .-records__FRCARDINAL7
	.align 4
	.type	records__ARCARDINAL8, #function
	.proc	016
records__ARCARDINAL8:
.LLFB1062:
	save	%sp, -96, %sp
.LLCFI1062:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1062:
	.size	records__ARCARDINAL8, .-records__ARCARDINAL8
	.align 4
	.type	records__BRCARDINAL8, #function
	.proc	020
records__BRCARDINAL8:
.LLFB1063:
	save	%sp, -128, %sp
.LLCFI1063:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1063:
	.size	records__BRCARDINAL8, .-records__BRCARDINAL8
	.align 4
	.type	records__CRCARDINAL8, #function
	.proc	016
records__CRCARDINAL8:
.LLFB1064:
	save	%sp, -96, %sp
.LLCFI1064:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1064:
	.size	records__CRCARDINAL8, .-records__CRCARDINAL8
	.align 4
	.type	records__DRCARDINAL8, #function
	.proc	020
records__DRCARDINAL8:
.LLFB1065:
	save	%sp, -128, %sp
.LLCFI1065:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1065:
	.size	records__DRCARDINAL8, .-records__DRCARDINAL8
	.align 4
	.type	records__ERCARDINAL8, #function
	.proc	016
records__ERCARDINAL8:
.LLFB1066:
	save	%sp, -96, %sp
.LLCFI1066:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1066:
	.size	records__ERCARDINAL8, .-records__ERCARDINAL8
	.align 4
	.type	records__FRCARDINAL8, #function
	.proc	020
records__FRCARDINAL8:
.LLFB1067:
	save	%sp, -128, %sp
.LLCFI1067:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1067:
	.size	records__FRCARDINAL8, .-records__FRCARDINAL8
	.align 4
	.type	records__ARCARDINAL9, #function
	.proc	016
records__ARCARDINAL9:
.LLFB1068:
	save	%sp, -96, %sp
.LLCFI1068:
	mov	%i0, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1068:
	.size	records__ARCARDINAL9, .-records__ARCARDINAL9
	.align 4
	.type	records__BRCARDINAL9, #function
	.proc	020
records__BRCARDINAL9:
.LLFB1069:
	save	%sp, -136, %sp
.LLCFI1069:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1069:
	.size	records__BRCARDINAL9, .-records__BRCARDINAL9
	.align 4
	.type	records__CRCARDINAL9, #function
	.proc	016
records__CRCARDINAL9:
.LLFB1070:
	save	%sp, -96, %sp
.LLCFI1070:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1070:
	.size	records__CRCARDINAL9, .-records__CRCARDINAL9
	.align 4
	.type	records__DRCARDINAL9, #function
	.proc	020
records__DRCARDINAL9:
.LLFB1071:
	save	%sp, -136, %sp
.LLCFI1071:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1071:
	.size	records__DRCARDINAL9, .-records__DRCARDINAL9
	.align 4
	.type	records__ERCARDINAL9, #function
	.proc	016
records__ERCARDINAL9:
.LLFB1072:
	save	%sp, -96, %sp
.LLCFI1072:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+16], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1072:
	.size	records__ERCARDINAL9, .-records__ERCARDINAL9
	.align 4
	.type	records__FRCARDINAL9, #function
	.proc	020
records__FRCARDINAL9:
.LLFB1073:
	save	%sp, -136, %sp
.LLCFI1073:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1073:
	.size	records__FRCARDINAL9, .-records__FRCARDINAL9
	.align 4
	.type	records__ARCARDINAL10, #function
	.proc	016
records__ARCARDINAL10:
.LLFB1074:
	save	%sp, -96, %sp
.LLCFI1074:
	mov	%i0, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1074:
	.size	records__ARCARDINAL10, .-records__ARCARDINAL10
	.align 4
	.type	records__BRCARDINAL10, #function
	.proc	020
records__BRCARDINAL10:
.LLFB1075:
	save	%sp, -136, %sp
.LLCFI1075:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1075:
	.size	records__BRCARDINAL10, .-records__BRCARDINAL10
	.align 4
	.type	records__CRCARDINAL10, #function
	.proc	016
records__CRCARDINAL10:
.LLFB1076:
	save	%sp, -96, %sp
.LLCFI1076:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1076:
	.size	records__CRCARDINAL10, .-records__CRCARDINAL10
	.align 4
	.type	records__DRCARDINAL10, #function
	.proc	020
records__DRCARDINAL10:
.LLFB1077:
	save	%sp, -136, %sp
.LLCFI1077:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1077:
	.size	records__DRCARDINAL10, .-records__DRCARDINAL10
	.align 4
	.type	records__ERCARDINAL10, #function
	.proc	016
records__ERCARDINAL10:
.LLFB1078:
	save	%sp, -96, %sp
.LLCFI1078:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1+20], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1078:
	.size	records__ERCARDINAL10, .-records__ERCARDINAL10
	.align 4
	.type	records__FRCARDINAL10, #function
	.proc	020
records__FRCARDINAL10:
.LLFB1079:
	save	%sp, -136, %sp
.LLCFI1079:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1079:
	.size	records__FRCARDINAL10, .-records__FRCARDINAL10
	.align 4
	.type	records__ARLONGINT1, #function
	.proc	05
records__ARLONGINT1:
.LLFB1080:
	save	%sp, -96, %sp
.LLCFI1080:
	mov	%i0, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1080:
	.size	records__ARLONGINT1, .-records__ARLONGINT1
	.align 4
	.type	records__BRLONGINT1, #function
	.proc	020
records__BRLONGINT1:
.LLFB1081:
	save	%sp, -104, %sp
.LLCFI1081:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1081:
	.size	records__BRLONGINT1, .-records__BRLONGINT1
	.align 4
	.type	records__CRLONGINT1, #function
	.proc	05
records__CRLONGINT1:
.LLFB1082:
	save	%sp, -96, %sp
.LLCFI1082:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1082:
	.size	records__CRLONGINT1, .-records__CRLONGINT1
	.align 4
	.type	records__DRLONGINT1, #function
	.proc	020
records__DRLONGINT1:
.LLFB1083:
	save	%sp, -104, %sp
.LLCFI1083:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1083:
	.size	records__DRLONGINT1, .-records__DRLONGINT1
	.align 4
	.type	records__ERLONGINT1, #function
	.proc	05
records__ERLONGINT1:
.LLFB1084:
	save	%sp, -96, %sp
.LLCFI1084:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1084:
	.size	records__ERLONGINT1, .-records__ERLONGINT1
	.align 4
	.type	records__FRLONGINT1, #function
	.proc	020
records__FRLONGINT1:
.LLFB1085:
	save	%sp, -104, %sp
.LLCFI1085:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1085:
	.size	records__FRLONGINT1, .-records__FRLONGINT1
	.align 4
	.type	records__ARLONGINT2, #function
	.proc	05
records__ARLONGINT2:
.LLFB1086:
	save	%sp, -96, %sp
.LLCFI1086:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1086:
	.size	records__ARLONGINT2, .-records__ARLONGINT2
	.align 4
	.type	records__BRLONGINT2, #function
	.proc	020
records__BRLONGINT2:
.LLFB1087:
	save	%sp, -112, %sp
.LLCFI1087:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1087:
	.size	records__BRLONGINT2, .-records__BRLONGINT2
	.align 4
	.type	records__CRLONGINT2, #function
	.proc	05
records__CRLONGINT2:
.LLFB1088:
	save	%sp, -96, %sp
.LLCFI1088:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1088:
	.size	records__CRLONGINT2, .-records__CRLONGINT2
	.align 4
	.type	records__DRLONGINT2, #function
	.proc	020
records__DRLONGINT2:
.LLFB1089:
	save	%sp, -112, %sp
.LLCFI1089:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1089:
	.size	records__DRLONGINT2, .-records__DRLONGINT2
	.align 4
	.type	records__ERLONGINT2, #function
	.proc	05
records__ERLONGINT2:
.LLFB1090:
	save	%sp, -96, %sp
.LLCFI1090:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1090:
	.size	records__ERLONGINT2, .-records__ERLONGINT2
	.align 4
	.type	records__FRLONGINT2, #function
	.proc	020
records__FRLONGINT2:
.LLFB1091:
	save	%sp, -112, %sp
.LLCFI1091:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1091:
	.size	records__FRLONGINT2, .-records__FRLONGINT2
	.align 4
	.type	records__ARLONGINT3, #function
	.proc	05
records__ARLONGINT3:
.LLFB1092:
	save	%sp, -96, %sp
.LLCFI1092:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1092:
	.size	records__ARLONGINT3, .-records__ARLONGINT3
	.align 4
	.type	records__BRLONGINT3, #function
	.proc	020
records__BRLONGINT3:
.LLFB1093:
	save	%sp, -120, %sp
.LLCFI1093:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1093:
	.size	records__BRLONGINT3, .-records__BRLONGINT3
	.align 4
	.type	records__CRLONGINT3, #function
	.proc	05
records__CRLONGINT3:
.LLFB1094:
	save	%sp, -96, %sp
.LLCFI1094:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1094:
	.size	records__CRLONGINT3, .-records__CRLONGINT3
	.align 4
	.type	records__DRLONGINT3, #function
	.proc	020
records__DRLONGINT3:
.LLFB1095:
	save	%sp, -120, %sp
.LLCFI1095:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1095:
	.size	records__DRLONGINT3, .-records__DRLONGINT3
	.align 4
	.type	records__ERLONGINT3, #function
	.proc	05
records__ERLONGINT3:
.LLFB1096:
	save	%sp, -96, %sp
.LLCFI1096:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1096:
	.size	records__ERLONGINT3, .-records__ERLONGINT3
	.align 4
	.type	records__FRLONGINT3, #function
	.proc	020
records__FRLONGINT3:
.LLFB1097:
	save	%sp, -120, %sp
.LLCFI1097:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1097:
	.size	records__FRLONGINT3, .-records__FRLONGINT3
	.align 4
	.type	records__ARLONGINT4, #function
	.proc	05
records__ARLONGINT4:
.LLFB1098:
	save	%sp, -96, %sp
.LLCFI1098:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1098:
	.size	records__ARLONGINT4, .-records__ARLONGINT4
	.align 4
	.type	records__BRLONGINT4, #function
	.proc	020
records__BRLONGINT4:
.LLFB1099:
	save	%sp, -128, %sp
.LLCFI1099:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1099:
	.size	records__BRLONGINT4, .-records__BRLONGINT4
	.align 4
	.type	records__CRLONGINT4, #function
	.proc	05
records__CRLONGINT4:
.LLFB1100:
	save	%sp, -96, %sp
.LLCFI1100:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1100:
	.size	records__CRLONGINT4, .-records__CRLONGINT4
	.align 4
	.type	records__DRLONGINT4, #function
	.proc	020
records__DRLONGINT4:
.LLFB1101:
	save	%sp, -128, %sp
.LLCFI1101:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1101:
	.size	records__DRLONGINT4, .-records__DRLONGINT4
	.align 4
	.type	records__ERLONGINT4, #function
	.proc	05
records__ERLONGINT4:
.LLFB1102:
	save	%sp, -96, %sp
.LLCFI1102:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1102:
	.size	records__ERLONGINT4, .-records__ERLONGINT4
	.align 4
	.type	records__FRLONGINT4, #function
	.proc	020
records__FRLONGINT4:
.LLFB1103:
	save	%sp, -128, %sp
.LLCFI1103:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1103:
	.size	records__FRLONGINT4, .-records__FRLONGINT4
	.align 4
	.type	records__ARLONGINT5, #function
	.proc	05
records__ARLONGINT5:
.LLFB1104:
	save	%sp, -96, %sp
.LLCFI1104:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1104:
	.size	records__ARLONGINT5, .-records__ARLONGINT5
	.align 4
	.type	records__BRLONGINT5, #function
	.proc	020
records__BRLONGINT5:
.LLFB1105:
	save	%sp, -136, %sp
.LLCFI1105:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1105:
	.size	records__BRLONGINT5, .-records__BRLONGINT5
	.align 4
	.type	records__CRLONGINT5, #function
	.proc	05
records__CRLONGINT5:
.LLFB1106:
	save	%sp, -96, %sp
.LLCFI1106:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1106:
	.size	records__CRLONGINT5, .-records__CRLONGINT5
	.align 4
	.type	records__DRLONGINT5, #function
	.proc	020
records__DRLONGINT5:
.LLFB1107:
	save	%sp, -136, %sp
.LLCFI1107:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1107:
	.size	records__DRLONGINT5, .-records__DRLONGINT5
	.align 4
	.type	records__ERLONGINT5, #function
	.proc	05
records__ERLONGINT5:
.LLFB1108:
	save	%sp, -96, %sp
.LLCFI1108:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1108:
	.size	records__ERLONGINT5, .-records__ERLONGINT5
	.align 4
	.type	records__FRLONGINT5, #function
	.proc	020
records__FRLONGINT5:
.LLFB1109:
	save	%sp, -136, %sp
.LLCFI1109:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1109:
	.size	records__FRLONGINT5, .-records__FRLONGINT5
	.align 4
	.type	records__ARLONGINT6, #function
	.proc	05
records__ARLONGINT6:
.LLFB1110:
	save	%sp, -96, %sp
.LLCFI1110:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1110:
	.size	records__ARLONGINT6, .-records__ARLONGINT6
	.align 4
	.type	records__BRLONGINT6, #function
	.proc	020
records__BRLONGINT6:
.LLFB1111:
	save	%sp, -144, %sp
.LLCFI1111:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1111:
	.size	records__BRLONGINT6, .-records__BRLONGINT6
	.align 4
	.type	records__CRLONGINT6, #function
	.proc	05
records__CRLONGINT6:
.LLFB1112:
	save	%sp, -96, %sp
.LLCFI1112:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1112:
	.size	records__CRLONGINT6, .-records__CRLONGINT6
	.align 4
	.type	records__DRLONGINT6, #function
	.proc	020
records__DRLONGINT6:
.LLFB1113:
	save	%sp, -144, %sp
.LLCFI1113:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1113:
	.size	records__DRLONGINT6, .-records__DRLONGINT6
	.align 4
	.type	records__ERLONGINT6, #function
	.proc	05
records__ERLONGINT6:
.LLFB1114:
	save	%sp, -96, %sp
.LLCFI1114:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1114:
	.size	records__ERLONGINT6, .-records__ERLONGINT6
	.align 4
	.type	records__FRLONGINT6, #function
	.proc	020
records__FRLONGINT6:
.LLFB1115:
	save	%sp, -144, %sp
.LLCFI1115:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1115:
	.size	records__FRLONGINT6, .-records__FRLONGINT6
	.align 4
	.type	records__ARLONGINT7, #function
	.proc	05
records__ARLONGINT7:
.LLFB1116:
	save	%sp, -96, %sp
.LLCFI1116:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1116:
	.size	records__ARLONGINT7, .-records__ARLONGINT7
	.align 4
	.type	records__BRLONGINT7, #function
	.proc	020
records__BRLONGINT7:
.LLFB1117:
	save	%sp, -152, %sp
.LLCFI1117:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1117:
	.size	records__BRLONGINT7, .-records__BRLONGINT7
	.align 4
	.type	records__CRLONGINT7, #function
	.proc	05
records__CRLONGINT7:
.LLFB1118:
	save	%sp, -96, %sp
.LLCFI1118:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1118:
	.size	records__CRLONGINT7, .-records__CRLONGINT7
	.align 4
	.type	records__DRLONGINT7, #function
	.proc	020
records__DRLONGINT7:
.LLFB1119:
	save	%sp, -152, %sp
.LLCFI1119:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1119:
	.size	records__DRLONGINT7, .-records__DRLONGINT7
	.align 4
	.type	records__ERLONGINT7, #function
	.proc	05
records__ERLONGINT7:
.LLFB1120:
	save	%sp, -96, %sp
.LLCFI1120:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1120:
	.size	records__ERLONGINT7, .-records__ERLONGINT7
	.align 4
	.type	records__FRLONGINT7, #function
	.proc	020
records__FRLONGINT7:
.LLFB1121:
	save	%sp, -152, %sp
.LLCFI1121:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1121:
	.size	records__FRLONGINT7, .-records__FRLONGINT7
	.align 4
	.type	records__ARLONGINT8, #function
	.proc	05
records__ARLONGINT8:
.LLFB1122:
	save	%sp, -96, %sp
.LLCFI1122:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1122:
	.size	records__ARLONGINT8, .-records__ARLONGINT8
	.align 4
	.type	records__BRLONGINT8, #function
	.proc	020
records__BRLONGINT8:
.LLFB1123:
	save	%sp, -160, %sp
.LLCFI1123:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1123:
	.size	records__BRLONGINT8, .-records__BRLONGINT8
	.align 4
	.type	records__CRLONGINT8, #function
	.proc	05
records__CRLONGINT8:
.LLFB1124:
	save	%sp, -96, %sp
.LLCFI1124:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1124:
	.size	records__CRLONGINT8, .-records__CRLONGINT8
	.align 4
	.type	records__DRLONGINT8, #function
	.proc	020
records__DRLONGINT8:
.LLFB1125:
	save	%sp, -160, %sp
.LLCFI1125:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1125:
	.size	records__DRLONGINT8, .-records__DRLONGINT8
	.align 4
	.type	records__ERLONGINT8, #function
	.proc	05
records__ERLONGINT8:
.LLFB1126:
	save	%sp, -96, %sp
.LLCFI1126:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1126:
	.size	records__ERLONGINT8, .-records__ERLONGINT8
	.align 4
	.type	records__FRLONGINT8, #function
	.proc	020
records__FRLONGINT8:
.LLFB1127:
	save	%sp, -160, %sp
.LLCFI1127:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1127:
	.size	records__FRLONGINT8, .-records__FRLONGINT8
	.align 4
	.type	records__ARLONGINT9, #function
	.proc	05
records__ARLONGINT9:
.LLFB1128:
	save	%sp, -96, %sp
.LLCFI1128:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1128:
	.size	records__ARLONGINT9, .-records__ARLONGINT9
	.align 4
	.type	records__BRLONGINT9, #function
	.proc	020
records__BRLONGINT9:
.LLFB1129:
	save	%sp, -168, %sp
.LLCFI1129:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1129:
	.size	records__BRLONGINT9, .-records__BRLONGINT9
	.align 4
	.type	records__CRLONGINT9, #function
	.proc	05
records__CRLONGINT9:
.LLFB1130:
	save	%sp, -96, %sp
.LLCFI1130:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1130:
	.size	records__CRLONGINT9, .-records__CRLONGINT9
	.align 4
	.type	records__DRLONGINT9, #function
	.proc	020
records__DRLONGINT9:
.LLFB1131:
	save	%sp, -168, %sp
.LLCFI1131:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1131:
	.size	records__DRLONGINT9, .-records__DRLONGINT9
	.align 4
	.type	records__ERLONGINT9, #function
	.proc	05
records__ERLONGINT9:
.LLFB1132:
	save	%sp, -96, %sp
.LLCFI1132:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1132:
	.size	records__ERLONGINT9, .-records__ERLONGINT9
	.align 4
	.type	records__FRLONGINT9, #function
	.proc	020
records__FRLONGINT9:
.LLFB1133:
	save	%sp, -168, %sp
.LLCFI1133:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1133:
	.size	records__FRLONGINT9, .-records__FRLONGINT9
	.align 4
	.type	records__ARLONGINT10, #function
	.proc	05
records__ARLONGINT10:
.LLFB1134:
	save	%sp, -96, %sp
.LLCFI1134:
	mov	%i0, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1134:
	.size	records__ARLONGINT10, .-records__ARLONGINT10
	.align 4
	.type	records__BRLONGINT10, #function
	.proc	020
records__BRLONGINT10:
.LLFB1135:
	save	%sp, -176, %sp
.LLCFI1135:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1135:
	.size	records__BRLONGINT10, .-records__BRLONGINT10
	.align 4
	.type	records__CRLONGINT10, #function
	.proc	05
records__CRLONGINT10:
.LLFB1136:
	save	%sp, -96, %sp
.LLCFI1136:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1136:
	.size	records__CRLONGINT10, .-records__CRLONGINT10
	.align 4
	.type	records__DRLONGINT10, #function
	.proc	020
records__DRLONGINT10:
.LLFB1137:
	save	%sp, -176, %sp
.LLCFI1137:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1137:
	.size	records__DRLONGINT10, .-records__DRLONGINT10
	.align 4
	.type	records__ERLONGINT10, #function
	.proc	05
records__ERLONGINT10:
.LLFB1138:
	save	%sp, -96, %sp
.LLCFI1138:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1138:
	.size	records__ERLONGINT10, .-records__ERLONGINT10
	.align 4
	.type	records__FRLONGINT10, #function
	.proc	020
records__FRLONGINT10:
.LLFB1139:
	save	%sp, -176, %sp
.LLCFI1139:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1139:
	.size	records__FRLONGINT10, .-records__FRLONGINT10
	.align 4
	.type	records__ARLONGCARD1, #function
	.proc	017
records__ARLONGCARD1:
.LLFB1140:
	save	%sp, -96, %sp
.LLCFI1140:
	mov	%i0, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1140:
	.size	records__ARLONGCARD1, .-records__ARLONGCARD1
	.align 4
	.type	records__BRLONGCARD1, #function
	.proc	020
records__BRLONGCARD1:
.LLFB1141:
	save	%sp, -104, %sp
.LLCFI1141:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1141:
	.size	records__BRLONGCARD1, .-records__BRLONGCARD1
	.align 4
	.type	records__CRLONGCARD1, #function
	.proc	017
records__CRLONGCARD1:
.LLFB1142:
	save	%sp, -96, %sp
.LLCFI1142:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1142:
	.size	records__CRLONGCARD1, .-records__CRLONGCARD1
	.align 4
	.type	records__DRLONGCARD1, #function
	.proc	020
records__DRLONGCARD1:
.LLFB1143:
	save	%sp, -104, %sp
.LLCFI1143:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1143:
	.size	records__DRLONGCARD1, .-records__DRLONGCARD1
	.align 4
	.type	records__ERLONGCARD1, #function
	.proc	017
records__ERLONGCARD1:
.LLFB1144:
	save	%sp, -96, %sp
.LLCFI1144:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1144:
	.size	records__ERLONGCARD1, .-records__ERLONGCARD1
	.align 4
	.type	records__FRLONGCARD1, #function
	.proc	020
records__FRLONGCARD1:
.LLFB1145:
	save	%sp, -104, %sp
.LLCFI1145:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1145:
	.size	records__FRLONGCARD1, .-records__FRLONGCARD1
	.align 4
	.type	records__ARLONGCARD2, #function
	.proc	017
records__ARLONGCARD2:
.LLFB1146:
	save	%sp, -96, %sp
.LLCFI1146:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1146:
	.size	records__ARLONGCARD2, .-records__ARLONGCARD2
	.align 4
	.type	records__BRLONGCARD2, #function
	.proc	020
records__BRLONGCARD2:
.LLFB1147:
	save	%sp, -112, %sp
.LLCFI1147:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1147:
	.size	records__BRLONGCARD2, .-records__BRLONGCARD2
	.align 4
	.type	records__CRLONGCARD2, #function
	.proc	017
records__CRLONGCARD2:
.LLFB1148:
	save	%sp, -96, %sp
.LLCFI1148:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1148:
	.size	records__CRLONGCARD2, .-records__CRLONGCARD2
	.align 4
	.type	records__DRLONGCARD2, #function
	.proc	020
records__DRLONGCARD2:
.LLFB1149:
	save	%sp, -112, %sp
.LLCFI1149:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1149:
	.size	records__DRLONGCARD2, .-records__DRLONGCARD2
	.align 4
	.type	records__ERLONGCARD2, #function
	.proc	017
records__ERLONGCARD2:
.LLFB1150:
	save	%sp, -96, %sp
.LLCFI1150:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1150:
	.size	records__ERLONGCARD2, .-records__ERLONGCARD2
	.align 4
	.type	records__FRLONGCARD2, #function
	.proc	020
records__FRLONGCARD2:
.LLFB1151:
	save	%sp, -112, %sp
.LLCFI1151:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1151:
	.size	records__FRLONGCARD2, .-records__FRLONGCARD2
	.align 4
	.type	records__ARLONGCARD3, #function
	.proc	017
records__ARLONGCARD3:
.LLFB1152:
	save	%sp, -96, %sp
.LLCFI1152:
	mov	%i0, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1152:
	.size	records__ARLONGCARD3, .-records__ARLONGCARD3
	.align 4
	.type	records__BRLONGCARD3, #function
	.proc	020
records__BRLONGCARD3:
.LLFB1153:
	save	%sp, -120, %sp
.LLCFI1153:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1153:
	.size	records__BRLONGCARD3, .-records__BRLONGCARD3
	.align 4
	.type	records__CRLONGCARD3, #function
	.proc	017
records__CRLONGCARD3:
.LLFB1154:
	save	%sp, -96, %sp
.LLCFI1154:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1154:
	.size	records__CRLONGCARD3, .-records__CRLONGCARD3
	.align 4
	.type	records__DRLONGCARD3, #function
	.proc	020
records__DRLONGCARD3:
.LLFB1155:
	save	%sp, -120, %sp
.LLCFI1155:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1155:
	.size	records__DRLONGCARD3, .-records__DRLONGCARD3
	.align 4
	.type	records__ERLONGCARD3, #function
	.proc	017
records__ERLONGCARD3:
.LLFB1156:
	save	%sp, -96, %sp
.LLCFI1156:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1156:
	.size	records__ERLONGCARD3, .-records__ERLONGCARD3
	.align 4
	.type	records__FRLONGCARD3, #function
	.proc	020
records__FRLONGCARD3:
.LLFB1157:
	save	%sp, -120, %sp
.LLCFI1157:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1157:
	.size	records__FRLONGCARD3, .-records__FRLONGCARD3
	.align 4
	.type	records__ARLONGCARD4, #function
	.proc	017
records__ARLONGCARD4:
.LLFB1158:
	save	%sp, -96, %sp
.LLCFI1158:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1158:
	.size	records__ARLONGCARD4, .-records__ARLONGCARD4
	.align 4
	.type	records__BRLONGCARD4, #function
	.proc	020
records__BRLONGCARD4:
.LLFB1159:
	save	%sp, -128, %sp
.LLCFI1159:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1159:
	.size	records__BRLONGCARD4, .-records__BRLONGCARD4
	.align 4
	.type	records__CRLONGCARD4, #function
	.proc	017
records__CRLONGCARD4:
.LLFB1160:
	save	%sp, -96, %sp
.LLCFI1160:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1160:
	.size	records__CRLONGCARD4, .-records__CRLONGCARD4
	.align 4
	.type	records__DRLONGCARD4, #function
	.proc	020
records__DRLONGCARD4:
.LLFB1161:
	save	%sp, -128, %sp
.LLCFI1161:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1161:
	.size	records__DRLONGCARD4, .-records__DRLONGCARD4
	.align 4
	.type	records__ERLONGCARD4, #function
	.proc	017
records__ERLONGCARD4:
.LLFB1162:
	save	%sp, -96, %sp
.LLCFI1162:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1162:
	.size	records__ERLONGCARD4, .-records__ERLONGCARD4
	.align 4
	.type	records__FRLONGCARD4, #function
	.proc	020
records__FRLONGCARD4:
.LLFB1163:
	save	%sp, -128, %sp
.LLCFI1163:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1163:
	.size	records__FRLONGCARD4, .-records__FRLONGCARD4
	.align 4
	.type	records__ARLONGCARD5, #function
	.proc	017
records__ARLONGCARD5:
.LLFB1164:
	save	%sp, -96, %sp
.LLCFI1164:
	mov	%i0, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1164:
	.size	records__ARLONGCARD5, .-records__ARLONGCARD5
	.align 4
	.type	records__BRLONGCARD5, #function
	.proc	020
records__BRLONGCARD5:
.LLFB1165:
	save	%sp, -136, %sp
.LLCFI1165:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1165:
	.size	records__BRLONGCARD5, .-records__BRLONGCARD5
	.align 4
	.type	records__CRLONGCARD5, #function
	.proc	017
records__CRLONGCARD5:
.LLFB1166:
	save	%sp, -96, %sp
.LLCFI1166:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1166:
	.size	records__CRLONGCARD5, .-records__CRLONGCARD5
	.align 4
	.type	records__DRLONGCARD5, #function
	.proc	020
records__DRLONGCARD5:
.LLFB1167:
	save	%sp, -136, %sp
.LLCFI1167:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1167:
	.size	records__DRLONGCARD5, .-records__DRLONGCARD5
	.align 4
	.type	records__ERLONGCARD5, #function
	.proc	017
records__ERLONGCARD5:
.LLFB1168:
	save	%sp, -96, %sp
.LLCFI1168:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+16], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1168:
	.size	records__ERLONGCARD5, .-records__ERLONGCARD5
	.align 4
	.type	records__FRLONGCARD5, #function
	.proc	020
records__FRLONGCARD5:
.LLFB1169:
	save	%sp, -136, %sp
.LLCFI1169:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1169:
	.size	records__FRLONGCARD5, .-records__FRLONGCARD5
	.align 4
	.type	records__ARLONGCARD6, #function
	.proc	017
records__ARLONGCARD6:
.LLFB1170:
	save	%sp, -96, %sp
.LLCFI1170:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1170:
	.size	records__ARLONGCARD6, .-records__ARLONGCARD6
	.align 4
	.type	records__BRLONGCARD6, #function
	.proc	020
records__BRLONGCARD6:
.LLFB1171:
	save	%sp, -144, %sp
.LLCFI1171:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1171:
	.size	records__BRLONGCARD6, .-records__BRLONGCARD6
	.align 4
	.type	records__CRLONGCARD6, #function
	.proc	017
records__CRLONGCARD6:
.LLFB1172:
	save	%sp, -96, %sp
.LLCFI1172:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1172:
	.size	records__CRLONGCARD6, .-records__CRLONGCARD6
	.align 4
	.type	records__DRLONGCARD6, #function
	.proc	020
records__DRLONGCARD6:
.LLFB1173:
	save	%sp, -144, %sp
.LLCFI1173:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1173:
	.size	records__DRLONGCARD6, .-records__DRLONGCARD6
	.align 4
	.type	records__ERLONGCARD6, #function
	.proc	017
records__ERLONGCARD6:
.LLFB1174:
	save	%sp, -96, %sp
.LLCFI1174:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1174:
	.size	records__ERLONGCARD6, .-records__ERLONGCARD6
	.align 4
	.type	records__FRLONGCARD6, #function
	.proc	020
records__FRLONGCARD6:
.LLFB1175:
	save	%sp, -144, %sp
.LLCFI1175:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1175:
	.size	records__FRLONGCARD6, .-records__FRLONGCARD6
	.align 4
	.type	records__ARLONGCARD7, #function
	.proc	017
records__ARLONGCARD7:
.LLFB1176:
	save	%sp, -96, %sp
.LLCFI1176:
	mov	%i0, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1176:
	.size	records__ARLONGCARD7, .-records__ARLONGCARD7
	.align 4
	.type	records__BRLONGCARD7, #function
	.proc	020
records__BRLONGCARD7:
.LLFB1177:
	save	%sp, -152, %sp
.LLCFI1177:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1177:
	.size	records__BRLONGCARD7, .-records__BRLONGCARD7
	.align 4
	.type	records__CRLONGCARD7, #function
	.proc	017
records__CRLONGCARD7:
.LLFB1178:
	save	%sp, -96, %sp
.LLCFI1178:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1178:
	.size	records__CRLONGCARD7, .-records__CRLONGCARD7
	.align 4
	.type	records__DRLONGCARD7, #function
	.proc	020
records__DRLONGCARD7:
.LLFB1179:
	save	%sp, -152, %sp
.LLCFI1179:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1179:
	.size	records__DRLONGCARD7, .-records__DRLONGCARD7
	.align 4
	.type	records__ERLONGCARD7, #function
	.proc	017
records__ERLONGCARD7:
.LLFB1180:
	save	%sp, -96, %sp
.LLCFI1180:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+24], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1180:
	.size	records__ERLONGCARD7, .-records__ERLONGCARD7
	.align 4
	.type	records__FRLONGCARD7, #function
	.proc	020
records__FRLONGCARD7:
.LLFB1181:
	save	%sp, -152, %sp
.LLCFI1181:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1181:
	.size	records__FRLONGCARD7, .-records__FRLONGCARD7
	.align 4
	.type	records__ARLONGCARD8, #function
	.proc	017
records__ARLONGCARD8:
.LLFB1182:
	save	%sp, -96, %sp
.LLCFI1182:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1182:
	.size	records__ARLONGCARD8, .-records__ARLONGCARD8
	.align 4
	.type	records__BRLONGCARD8, #function
	.proc	020
records__BRLONGCARD8:
.LLFB1183:
	save	%sp, -160, %sp
.LLCFI1183:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1183:
	.size	records__BRLONGCARD8, .-records__BRLONGCARD8
	.align 4
	.type	records__CRLONGCARD8, #function
	.proc	017
records__CRLONGCARD8:
.LLFB1184:
	save	%sp, -96, %sp
.LLCFI1184:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1184:
	.size	records__CRLONGCARD8, .-records__CRLONGCARD8
	.align 4
	.type	records__DRLONGCARD8, #function
	.proc	020
records__DRLONGCARD8:
.LLFB1185:
	save	%sp, -160, %sp
.LLCFI1185:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1185:
	.size	records__DRLONGCARD8, .-records__DRLONGCARD8
	.align 4
	.type	records__ERLONGCARD8, #function
	.proc	017
records__ERLONGCARD8:
.LLFB1186:
	save	%sp, -96, %sp
.LLCFI1186:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1186:
	.size	records__ERLONGCARD8, .-records__ERLONGCARD8
	.align 4
	.type	records__FRLONGCARD8, #function
	.proc	020
records__FRLONGCARD8:
.LLFB1187:
	save	%sp, -160, %sp
.LLCFI1187:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1187:
	.size	records__FRLONGCARD8, .-records__FRLONGCARD8
	.align 4
	.type	records__ARLONGCARD9, #function
	.proc	017
records__ARLONGCARD9:
.LLFB1188:
	save	%sp, -96, %sp
.LLCFI1188:
	mov	%i0, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1188:
	.size	records__ARLONGCARD9, .-records__ARLONGCARD9
	.align 4
	.type	records__BRLONGCARD9, #function
	.proc	020
records__BRLONGCARD9:
.LLFB1189:
	save	%sp, -168, %sp
.LLCFI1189:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1189:
	.size	records__BRLONGCARD9, .-records__BRLONGCARD9
	.align 4
	.type	records__CRLONGCARD9, #function
	.proc	017
records__CRLONGCARD9:
.LLFB1190:
	save	%sp, -96, %sp
.LLCFI1190:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1190:
	.size	records__CRLONGCARD9, .-records__CRLONGCARD9
	.align 4
	.type	records__DRLONGCARD9, #function
	.proc	020
records__DRLONGCARD9:
.LLFB1191:
	save	%sp, -168, %sp
.LLCFI1191:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1191:
	.size	records__DRLONGCARD9, .-records__DRLONGCARD9
	.align 4
	.type	records__ERLONGCARD9, #function
	.proc	017
records__ERLONGCARD9:
.LLFB1192:
	save	%sp, -96, %sp
.LLCFI1192:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+32], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1192:
	.size	records__ERLONGCARD9, .-records__ERLONGCARD9
	.align 4
	.type	records__FRLONGCARD9, #function
	.proc	020
records__FRLONGCARD9:
.LLFB1193:
	save	%sp, -168, %sp
.LLCFI1193:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1193:
	.size	records__FRLONGCARD9, .-records__FRLONGCARD9
	.align 4
	.type	records__ARLONGCARD10, #function
	.proc	017
records__ARLONGCARD10:
.LLFB1194:
	save	%sp, -96, %sp
.LLCFI1194:
	mov	%i0, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1194:
	.size	records__ARLONGCARD10, .-records__ARLONGCARD10
	.align 4
	.type	records__BRLONGCARD10, #function
	.proc	020
records__BRLONGCARD10:
.LLFB1195:
	save	%sp, -176, %sp
.LLCFI1195:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1195:
	.size	records__BRLONGCARD10, .-records__BRLONGCARD10
	.align 4
	.type	records__CRLONGCARD10, #function
	.proc	017
records__CRLONGCARD10:
.LLFB1196:
	save	%sp, -96, %sp
.LLCFI1196:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1196:
	.size	records__CRLONGCARD10, .-records__CRLONGCARD10
	.align 4
	.type	records__DRLONGCARD10, #function
	.proc	020
records__DRLONGCARD10:
.LLFB1197:
	save	%sp, -176, %sp
.LLCFI1197:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1197:
	.size	records__DRLONGCARD10, .-records__DRLONGCARD10
	.align 4
	.type	records__ERLONGCARD10, #function
	.proc	017
records__ERLONGCARD10:
.LLFB1198:
	save	%sp, -96, %sp
.LLCFI1198:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1+40], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1198:
	.size	records__ERLONGCARD10, .-records__ERLONGCARD10
	.align 4
	.type	records__FRLONGCARD10, #function
	.proc	020
records__FRLONGCARD10:
.LLFB1199:
	save	%sp, -176, %sp
.LLCFI1199:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1199:
	.size	records__FRLONGCARD10, .-records__FRLONGCARD10
	.align 4
	.type	records__ARFLOAT321, #function
	.proc	06
records__ARFLOAT321:
.LLFB1200:
	save	%sp, -96, %sp
.LLCFI1200:
	mov	%i0, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1200:
	.size	records__ARFLOAT321, .-records__ARFLOAT321
	.align 4
	.type	records__BRFLOAT321, #function
	.proc	020
records__BRFLOAT321:
.LLFB1201:
	save	%sp, -104, %sp
.LLCFI1201:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1201:
	.size	records__BRFLOAT321, .-records__BRFLOAT321
	.align 4
	.type	records__CRFLOAT321, #function
	.proc	06
records__CRFLOAT321:
.LLFB1202:
	save	%sp, -96, %sp
.LLCFI1202:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1202:
	.size	records__CRFLOAT321, .-records__CRFLOAT321
	.align 4
	.type	records__DRFLOAT321, #function
	.proc	020
records__DRFLOAT321:
.LLFB1203:
	save	%sp, -104, %sp
.LLCFI1203:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1203:
	.size	records__DRFLOAT321, .-records__DRFLOAT321
	.align 4
	.type	records__ERFLOAT321, #function
	.proc	06
records__ERFLOAT321:
.LLFB1204:
	save	%sp, -96, %sp
.LLCFI1204:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1204:
	.size	records__ERFLOAT321, .-records__ERFLOAT321
	.align 4
	.type	records__FRFLOAT321, #function
	.proc	020
records__FRFLOAT321:
.LLFB1205:
	save	%sp, -104, %sp
.LLCFI1205:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	4, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1205:
	.size	records__FRFLOAT321, .-records__FRFLOAT321
	.align 4
	.type	records__ARFLOAT322, #function
	.proc	06
records__ARFLOAT322:
.LLFB1206:
	save	%sp, -96, %sp
.LLCFI1206:
	mov	%i0, %g1
	add	%g1, 4, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1206:
	.size	records__ARFLOAT322, .-records__ARFLOAT322
	.align 4
	.type	records__BRFLOAT322, #function
	.proc	020
records__BRFLOAT322:
.LLFB1207:
	save	%sp, -104, %sp
.LLCFI1207:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1207:
	.size	records__BRFLOAT322, .-records__BRFLOAT322
	.align 4
	.type	records__CRFLOAT322, #function
	.proc	06
records__CRFLOAT322:
.LLFB1208:
	save	%sp, -96, %sp
.LLCFI1208:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 4, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1208:
	.size	records__CRFLOAT322, .-records__CRFLOAT322
	.align 4
	.type	records__DRFLOAT322, #function
	.proc	020
records__DRFLOAT322:
.LLFB1209:
	save	%sp, -104, %sp
.LLCFI1209:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1209:
	.size	records__DRFLOAT322, .-records__DRFLOAT322
	.align 4
	.type	records__ERFLOAT322, #function
	.proc	06
records__ERFLOAT322:
.LLFB1210:
	save	%sp, -96, %sp
.LLCFI1210:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 4, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1210:
	.size	records__ERFLOAT322, .-records__ERFLOAT322
	.align 4
	.type	records__FRFLOAT322, #function
	.proc	020
records__FRFLOAT322:
.LLFB1211:
	save	%sp, -104, %sp
.LLCFI1211:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1211:
	.size	records__FRFLOAT322, .-records__FRFLOAT322
	.align 4
	.type	records__ARFLOAT323, #function
	.proc	06
records__ARFLOAT323:
.LLFB1212:
	save	%sp, -96, %sp
.LLCFI1212:
	mov	%i0, %g1
	add	%g1, 4, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1212:
	.size	records__ARFLOAT323, .-records__ARFLOAT323
	.align 4
	.type	records__BRFLOAT323, #function
	.proc	020
records__BRFLOAT323:
.LLFB1213:
	save	%sp, -112, %sp
.LLCFI1213:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1213:
	.size	records__BRFLOAT323, .-records__BRFLOAT323
	.align 4
	.type	records__CRFLOAT323, #function
	.proc	06
records__CRFLOAT323:
.LLFB1214:
	save	%sp, -96, %sp
.LLCFI1214:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 4, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1214:
	.size	records__CRFLOAT323, .-records__CRFLOAT323
	.align 4
	.type	records__DRFLOAT323, #function
	.proc	020
records__DRFLOAT323:
.LLFB1215:
	save	%sp, -112, %sp
.LLCFI1215:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1215:
	.size	records__DRFLOAT323, .-records__DRFLOAT323
	.align 4
	.type	records__ERFLOAT323, #function
	.proc	06
records__ERFLOAT323:
.LLFB1216:
	save	%sp, -96, %sp
.LLCFI1216:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 4, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1216:
	.size	records__ERFLOAT323, .-records__ERFLOAT323
	.align 4
	.type	records__FRFLOAT323, #function
	.proc	020
records__FRFLOAT323:
.LLFB1217:
	save	%sp, -112, %sp
.LLCFI1217:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	12, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1217:
	.size	records__FRFLOAT323, .-records__FRFLOAT323
	.align 4
	.type	records__ARFLOAT324, #function
	.proc	06
records__ARFLOAT324:
.LLFB1218:
	save	%sp, -96, %sp
.LLCFI1218:
	mov	%i0, %g1
	add	%g1, 8, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1218:
	.size	records__ARFLOAT324, .-records__ARFLOAT324
	.align 4
	.type	records__BRFLOAT324, #function
	.proc	020
records__BRFLOAT324:
.LLFB1219:
	save	%sp, -112, %sp
.LLCFI1219:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1219:
	.size	records__BRFLOAT324, .-records__BRFLOAT324
	.align 4
	.type	records__CRFLOAT324, #function
	.proc	06
records__CRFLOAT324:
.LLFB1220:
	save	%sp, -96, %sp
.LLCFI1220:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 8, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1220:
	.size	records__CRFLOAT324, .-records__CRFLOAT324
	.align 4
	.type	records__DRFLOAT324, #function
	.proc	020
records__DRFLOAT324:
.LLFB1221:
	save	%sp, -112, %sp
.LLCFI1221:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1221:
	.size	records__DRFLOAT324, .-records__DRFLOAT324
	.align 4
	.type	records__ERFLOAT324, #function
	.proc	06
records__ERFLOAT324:
.LLFB1222:
	save	%sp, -96, %sp
.LLCFI1222:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 8, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1222:
	.size	records__ERFLOAT324, .-records__ERFLOAT324
	.align 4
	.type	records__FRFLOAT324, #function
	.proc	020
records__FRFLOAT324:
.LLFB1223:
	save	%sp, -112, %sp
.LLCFI1223:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1223:
	.size	records__FRFLOAT324, .-records__FRFLOAT324
	.align 4
	.type	records__ARFLOAT325, #function
	.proc	06
records__ARFLOAT325:
.LLFB1224:
	save	%sp, -96, %sp
.LLCFI1224:
	mov	%i0, %g1
	add	%g1, 8, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1224:
	.size	records__ARFLOAT325, .-records__ARFLOAT325
	.align 4
	.type	records__BRFLOAT325, #function
	.proc	020
records__BRFLOAT325:
.LLFB1225:
	save	%sp, -120, %sp
.LLCFI1225:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1225:
	.size	records__BRFLOAT325, .-records__BRFLOAT325
	.align 4
	.type	records__CRFLOAT325, #function
	.proc	06
records__CRFLOAT325:
.LLFB1226:
	save	%sp, -96, %sp
.LLCFI1226:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 8, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1226:
	.size	records__CRFLOAT325, .-records__CRFLOAT325
	.align 4
	.type	records__DRFLOAT325, #function
	.proc	020
records__DRFLOAT325:
.LLFB1227:
	save	%sp, -120, %sp
.LLCFI1227:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1227:
	.size	records__DRFLOAT325, .-records__DRFLOAT325
	.align 4
	.type	records__ERFLOAT325, #function
	.proc	06
records__ERFLOAT325:
.LLFB1228:
	save	%sp, -96, %sp
.LLCFI1228:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 8, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1228:
	.size	records__ERFLOAT325, .-records__ERFLOAT325
	.align 4
	.type	records__FRFLOAT325, #function
	.proc	020
records__FRFLOAT325:
.LLFB1229:
	save	%sp, -120, %sp
.LLCFI1229:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	20, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1229:
	.size	records__FRFLOAT325, .-records__FRFLOAT325
	.align 4
	.type	records__ARFLOAT326, #function
	.proc	06
records__ARFLOAT326:
.LLFB1230:
	save	%sp, -96, %sp
.LLCFI1230:
	mov	%i0, %g1
	add	%g1, 12, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1230:
	.size	records__ARFLOAT326, .-records__ARFLOAT326
	.align 4
	.type	records__BRFLOAT326, #function
	.proc	020
records__BRFLOAT326:
.LLFB1231:
	save	%sp, -120, %sp
.LLCFI1231:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1231:
	.size	records__BRFLOAT326, .-records__BRFLOAT326
	.align 4
	.type	records__CRFLOAT326, #function
	.proc	06
records__CRFLOAT326:
.LLFB1232:
	save	%sp, -96, %sp
.LLCFI1232:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 12, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1232:
	.size	records__CRFLOAT326, .-records__CRFLOAT326
	.align 4
	.type	records__DRFLOAT326, #function
	.proc	020
records__DRFLOAT326:
.LLFB1233:
	save	%sp, -120, %sp
.LLCFI1233:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1233:
	.size	records__DRFLOAT326, .-records__DRFLOAT326
	.align 4
	.type	records__ERFLOAT326, #function
	.proc	06
records__ERFLOAT326:
.LLFB1234:
	save	%sp, -96, %sp
.LLCFI1234:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 12, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1234:
	.size	records__ERFLOAT326, .-records__ERFLOAT326
	.align 4
	.type	records__FRFLOAT326, #function
	.proc	020
records__FRFLOAT326:
.LLFB1235:
	save	%sp, -120, %sp
.LLCFI1235:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1235:
	.size	records__FRFLOAT326, .-records__FRFLOAT326
	.align 4
	.type	records__ARFLOAT327, #function
	.proc	06
records__ARFLOAT327:
.LLFB1236:
	save	%sp, -96, %sp
.LLCFI1236:
	mov	%i0, %g1
	add	%g1, 12, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1236:
	.size	records__ARFLOAT327, .-records__ARFLOAT327
	.align 4
	.type	records__BRFLOAT327, #function
	.proc	020
records__BRFLOAT327:
.LLFB1237:
	save	%sp, -128, %sp
.LLCFI1237:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1237:
	.size	records__BRFLOAT327, .-records__BRFLOAT327
	.align 4
	.type	records__CRFLOAT327, #function
	.proc	06
records__CRFLOAT327:
.LLFB1238:
	save	%sp, -96, %sp
.LLCFI1238:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 12, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1238:
	.size	records__CRFLOAT327, .-records__CRFLOAT327
	.align 4
	.type	records__DRFLOAT327, #function
	.proc	020
records__DRFLOAT327:
.LLFB1239:
	save	%sp, -128, %sp
.LLCFI1239:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1239:
	.size	records__DRFLOAT327, .-records__DRFLOAT327
	.align 4
	.type	records__ERFLOAT327, #function
	.proc	06
records__ERFLOAT327:
.LLFB1240:
	save	%sp, -96, %sp
.LLCFI1240:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 12, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1240:
	.size	records__ERFLOAT327, .-records__ERFLOAT327
	.align 4
	.type	records__FRFLOAT327, #function
	.proc	020
records__FRFLOAT327:
.LLFB1241:
	save	%sp, -128, %sp
.LLCFI1241:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	28, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1241:
	.size	records__FRFLOAT327, .-records__FRFLOAT327
	.align 4
	.type	records__ARFLOAT328, #function
	.proc	06
records__ARFLOAT328:
.LLFB1242:
	save	%sp, -96, %sp
.LLCFI1242:
	mov	%i0, %g1
	add	%g1, 16, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1242:
	.size	records__ARFLOAT328, .-records__ARFLOAT328
	.align 4
	.type	records__BRFLOAT328, #function
	.proc	020
records__BRFLOAT328:
.LLFB1243:
	save	%sp, -128, %sp
.LLCFI1243:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1243:
	.size	records__BRFLOAT328, .-records__BRFLOAT328
	.align 4
	.type	records__CRFLOAT328, #function
	.proc	06
records__CRFLOAT328:
.LLFB1244:
	save	%sp, -96, %sp
.LLCFI1244:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 16, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1244:
	.size	records__CRFLOAT328, .-records__CRFLOAT328
	.align 4
	.type	records__DRFLOAT328, #function
	.proc	020
records__DRFLOAT328:
.LLFB1245:
	save	%sp, -128, %sp
.LLCFI1245:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1245:
	.size	records__DRFLOAT328, .-records__DRFLOAT328
	.align 4
	.type	records__ERFLOAT328, #function
	.proc	06
records__ERFLOAT328:
.LLFB1246:
	save	%sp, -96, %sp
.LLCFI1246:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 16, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1246:
	.size	records__ERFLOAT328, .-records__ERFLOAT328
	.align 4
	.type	records__FRFLOAT328, #function
	.proc	020
records__FRFLOAT328:
.LLFB1247:
	save	%sp, -128, %sp
.LLCFI1247:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1247:
	.size	records__FRFLOAT328, .-records__FRFLOAT328
	.align 4
	.type	records__ARFLOAT329, #function
	.proc	06
records__ARFLOAT329:
.LLFB1248:
	save	%sp, -96, %sp
.LLCFI1248:
	mov	%i0, %g1
	add	%g1, 16, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1248:
	.size	records__ARFLOAT329, .-records__ARFLOAT329
	.align 4
	.type	records__BRFLOAT329, #function
	.proc	020
records__BRFLOAT329:
.LLFB1249:
	save	%sp, -136, %sp
.LLCFI1249:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1249:
	.size	records__BRFLOAT329, .-records__BRFLOAT329
	.align 4
	.type	records__CRFLOAT329, #function
	.proc	06
records__CRFLOAT329:
.LLFB1250:
	save	%sp, -96, %sp
.LLCFI1250:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 16, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1250:
	.size	records__CRFLOAT329, .-records__CRFLOAT329
	.align 4
	.type	records__DRFLOAT329, #function
	.proc	020
records__DRFLOAT329:
.LLFB1251:
	save	%sp, -136, %sp
.LLCFI1251:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1251:
	.size	records__DRFLOAT329, .-records__DRFLOAT329
	.align 4
	.type	records__ERFLOAT329, #function
	.proc	06
records__ERFLOAT329:
.LLFB1252:
	save	%sp, -96, %sp
.LLCFI1252:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 16, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1252:
	.size	records__ERFLOAT329, .-records__ERFLOAT329
	.align 4
	.type	records__FRFLOAT329, #function
	.proc	020
records__FRFLOAT329:
.LLFB1253:
	save	%sp, -136, %sp
.LLCFI1253:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	36, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1253:
	.size	records__FRFLOAT329, .-records__FRFLOAT329
	.align 4
	.type	records__ARFLOAT3210, #function
	.proc	06
records__ARFLOAT3210:
.LLFB1254:
	save	%sp, -96, %sp
.LLCFI1254:
	mov	%i0, %g1
	add	%g1, 20, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1254:
	.size	records__ARFLOAT3210, .-records__ARFLOAT3210
	.align 4
	.type	records__BRFLOAT3210, #function
	.proc	020
records__BRFLOAT3210:
.LLFB1255:
	save	%sp, -136, %sp
.LLCFI1255:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1255:
	.size	records__BRFLOAT3210, .-records__BRFLOAT3210
	.align 4
	.type	records__CRFLOAT3210, #function
	.proc	06
records__CRFLOAT3210:
.LLFB1256:
	save	%sp, -96, %sp
.LLCFI1256:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 20, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1256:
	.size	records__CRFLOAT3210, .-records__CRFLOAT3210
	.align 4
	.type	records__DRFLOAT3210, #function
	.proc	020
records__DRFLOAT3210:
.LLFB1257:
	save	%sp, -136, %sp
.LLCFI1257:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1257:
	.size	records__DRFLOAT3210, .-records__DRFLOAT3210
	.align 4
	.type	records__ERFLOAT3210, #function
	.proc	06
records__ERFLOAT3210:
.LLFB1258:
	save	%sp, -96, %sp
.LLCFI1258:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 20, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE1258:
	.size	records__ERFLOAT3210, .-records__ERFLOAT3210
	.align 4
	.type	records__FRFLOAT3210, #function
	.proc	020
records__FRFLOAT3210:
.LLFB1259:
	save	%sp, -136, %sp
.LLCFI1259:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1259:
	.size	records__FRFLOAT3210, .-records__FRFLOAT3210
	.align 4
	.type	records__ARFLOAT641, #function
	.proc	07
records__ARFLOAT641:
.LLFB1260:
	save	%sp, -96, %sp
.LLCFI1260:
	mov	%i0, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1260:
	.size	records__ARFLOAT641, .-records__ARFLOAT641
	.align 4
	.type	records__BRFLOAT641, #function
	.proc	020
records__BRFLOAT641:
.LLFB1261:
	save	%sp, -104, %sp
.LLCFI1261:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1261:
	.size	records__BRFLOAT641, .-records__BRFLOAT641
	.align 4
	.type	records__CRFLOAT641, #function
	.proc	07
records__CRFLOAT641:
.LLFB1262:
	save	%sp, -96, %sp
.LLCFI1262:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1262:
	.size	records__CRFLOAT641, .-records__CRFLOAT641
	.align 4
	.type	records__DRFLOAT641, #function
	.proc	020
records__DRFLOAT641:
.LLFB1263:
	save	%sp, -104, %sp
.LLCFI1263:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1263:
	.size	records__DRFLOAT641, .-records__DRFLOAT641
	.align 4
	.type	records__ERFLOAT641, #function
	.proc	07
records__ERFLOAT641:
.LLFB1264:
	save	%sp, -96, %sp
.LLCFI1264:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1264:
	.size	records__ERFLOAT641, .-records__ERFLOAT641
	.align 4
	.type	records__FRFLOAT641, #function
	.proc	020
records__FRFLOAT641:
.LLFB1265:
	save	%sp, -104, %sp
.LLCFI1265:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	8, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1265:
	.size	records__FRFLOAT641, .-records__FRFLOAT641
	.align 4
	.type	records__ARFLOAT642, #function
	.proc	07
records__ARFLOAT642:
.LLFB1266:
	save	%sp, -96, %sp
.LLCFI1266:
	mov	%i0, %g1
	add	%g1, 8, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1266:
	.size	records__ARFLOAT642, .-records__ARFLOAT642
	.align 4
	.type	records__BRFLOAT642, #function
	.proc	020
records__BRFLOAT642:
.LLFB1267:
	save	%sp, -112, %sp
.LLCFI1267:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1267:
	.size	records__BRFLOAT642, .-records__BRFLOAT642
	.align 4
	.type	records__CRFLOAT642, #function
	.proc	07
records__CRFLOAT642:
.LLFB1268:
	save	%sp, -96, %sp
.LLCFI1268:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 8, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1268:
	.size	records__CRFLOAT642, .-records__CRFLOAT642
	.align 4
	.type	records__DRFLOAT642, #function
	.proc	020
records__DRFLOAT642:
.LLFB1269:
	save	%sp, -112, %sp
.LLCFI1269:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1269:
	.size	records__DRFLOAT642, .-records__DRFLOAT642
	.align 4
	.type	records__ERFLOAT642, #function
	.proc	07
records__ERFLOAT642:
.LLFB1270:
	save	%sp, -96, %sp
.LLCFI1270:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 8, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1270:
	.size	records__ERFLOAT642, .-records__ERFLOAT642
	.align 4
	.type	records__FRFLOAT642, #function
	.proc	020
records__FRFLOAT642:
.LLFB1271:
	save	%sp, -112, %sp
.LLCFI1271:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	16, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1271:
	.size	records__FRFLOAT642, .-records__FRFLOAT642
	.align 4
	.type	records__ARFLOAT643, #function
	.proc	07
records__ARFLOAT643:
.LLFB1272:
	save	%sp, -96, %sp
.LLCFI1272:
	mov	%i0, %g1
	add	%g1, 8, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1272:
	.size	records__ARFLOAT643, .-records__ARFLOAT643
	.align 4
	.type	records__BRFLOAT643, #function
	.proc	020
records__BRFLOAT643:
.LLFB1273:
	save	%sp, -120, %sp
.LLCFI1273:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1273:
	.size	records__BRFLOAT643, .-records__BRFLOAT643
	.align 4
	.type	records__CRFLOAT643, #function
	.proc	07
records__CRFLOAT643:
.LLFB1274:
	save	%sp, -96, %sp
.LLCFI1274:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 8, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1274:
	.size	records__CRFLOAT643, .-records__CRFLOAT643
	.align 4
	.type	records__DRFLOAT643, #function
	.proc	020
records__DRFLOAT643:
.LLFB1275:
	save	%sp, -120, %sp
.LLCFI1275:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1275:
	.size	records__DRFLOAT643, .-records__DRFLOAT643
	.align 4
	.type	records__ERFLOAT643, #function
	.proc	07
records__ERFLOAT643:
.LLFB1276:
	save	%sp, -96, %sp
.LLCFI1276:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 8, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1276:
	.size	records__ERFLOAT643, .-records__ERFLOAT643
	.align 4
	.type	records__FRFLOAT643, #function
	.proc	020
records__FRFLOAT643:
.LLFB1277:
	save	%sp, -120, %sp
.LLCFI1277:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	24, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1277:
	.size	records__FRFLOAT643, .-records__FRFLOAT643
	.align 4
	.type	records__ARFLOAT644, #function
	.proc	07
records__ARFLOAT644:
.LLFB1278:
	save	%sp, -96, %sp
.LLCFI1278:
	mov	%i0, %g1
	add	%g1, 16, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1278:
	.size	records__ARFLOAT644, .-records__ARFLOAT644
	.align 4
	.type	records__BRFLOAT644, #function
	.proc	020
records__BRFLOAT644:
.LLFB1279:
	save	%sp, -128, %sp
.LLCFI1279:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1279:
	.size	records__BRFLOAT644, .-records__BRFLOAT644
	.align 4
	.type	records__CRFLOAT644, #function
	.proc	07
records__CRFLOAT644:
.LLFB1280:
	save	%sp, -96, %sp
.LLCFI1280:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 16, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1280:
	.size	records__CRFLOAT644, .-records__CRFLOAT644
	.align 4
	.type	records__DRFLOAT644, #function
	.proc	020
records__DRFLOAT644:
.LLFB1281:
	save	%sp, -128, %sp
.LLCFI1281:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1281:
	.size	records__DRFLOAT644, .-records__DRFLOAT644
	.align 4
	.type	records__ERFLOAT644, #function
	.proc	07
records__ERFLOAT644:
.LLFB1282:
	save	%sp, -96, %sp
.LLCFI1282:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 16, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1282:
	.size	records__ERFLOAT644, .-records__ERFLOAT644
	.align 4
	.type	records__FRFLOAT644, #function
	.proc	020
records__FRFLOAT644:
.LLFB1283:
	save	%sp, -128, %sp
.LLCFI1283:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	32, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1283:
	.size	records__FRFLOAT644, .-records__FRFLOAT644
	.align 4
	.type	records__ARFLOAT645, #function
	.proc	07
records__ARFLOAT645:
.LLFB1284:
	save	%sp, -96, %sp
.LLCFI1284:
	mov	%i0, %g1
	add	%g1, 16, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1284:
	.size	records__ARFLOAT645, .-records__ARFLOAT645
	.align 4
	.type	records__BRFLOAT645, #function
	.proc	020
records__BRFLOAT645:
.LLFB1285:
	save	%sp, -136, %sp
.LLCFI1285:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1285:
	.size	records__BRFLOAT645, .-records__BRFLOAT645
	.align 4
	.type	records__CRFLOAT645, #function
	.proc	07
records__CRFLOAT645:
.LLFB1286:
	save	%sp, -96, %sp
.LLCFI1286:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 16, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1286:
	.size	records__CRFLOAT645, .-records__CRFLOAT645
	.align 4
	.type	records__DRFLOAT645, #function
	.proc	020
records__DRFLOAT645:
.LLFB1287:
	save	%sp, -136, %sp
.LLCFI1287:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1287:
	.size	records__DRFLOAT645, .-records__DRFLOAT645
	.align 4
	.type	records__ERFLOAT645, #function
	.proc	07
records__ERFLOAT645:
.LLFB1288:
	save	%sp, -96, %sp
.LLCFI1288:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 16, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1288:
	.size	records__ERFLOAT645, .-records__ERFLOAT645
	.align 4
	.type	records__FRFLOAT645, #function
	.proc	020
records__FRFLOAT645:
.LLFB1289:
	save	%sp, -136, %sp
.LLCFI1289:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	40, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1289:
	.size	records__FRFLOAT645, .-records__FRFLOAT645
	.align 4
	.type	records__ARFLOAT646, #function
	.proc	07
records__ARFLOAT646:
.LLFB1290:
	save	%sp, -96, %sp
.LLCFI1290:
	mov	%i0, %g1
	add	%g1, 24, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1290:
	.size	records__ARFLOAT646, .-records__ARFLOAT646
	.align 4
	.type	records__BRFLOAT646, #function
	.proc	020
records__BRFLOAT646:
.LLFB1291:
	save	%sp, -144, %sp
.LLCFI1291:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1291:
	.size	records__BRFLOAT646, .-records__BRFLOAT646
	.align 4
	.type	records__CRFLOAT646, #function
	.proc	07
records__CRFLOAT646:
.LLFB1292:
	save	%sp, -96, %sp
.LLCFI1292:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 24, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1292:
	.size	records__CRFLOAT646, .-records__CRFLOAT646
	.align 4
	.type	records__DRFLOAT646, #function
	.proc	020
records__DRFLOAT646:
.LLFB1293:
	save	%sp, -144, %sp
.LLCFI1293:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1293:
	.size	records__DRFLOAT646, .-records__DRFLOAT646
	.align 4
	.type	records__ERFLOAT646, #function
	.proc	07
records__ERFLOAT646:
.LLFB1294:
	save	%sp, -96, %sp
.LLCFI1294:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 24, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1294:
	.size	records__ERFLOAT646, .-records__ERFLOAT646
	.align 4
	.type	records__FRFLOAT646, #function
	.proc	020
records__FRFLOAT646:
.LLFB1295:
	save	%sp, -144, %sp
.LLCFI1295:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	48, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1295:
	.size	records__FRFLOAT646, .-records__FRFLOAT646
	.align 4
	.type	records__ARFLOAT647, #function
	.proc	07
records__ARFLOAT647:
.LLFB1296:
	save	%sp, -96, %sp
.LLCFI1296:
	mov	%i0, %g1
	add	%g1, 24, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1296:
	.size	records__ARFLOAT647, .-records__ARFLOAT647
	.align 4
	.type	records__BRFLOAT647, #function
	.proc	020
records__BRFLOAT647:
.LLFB1297:
	save	%sp, -152, %sp
.LLCFI1297:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1297:
	.size	records__BRFLOAT647, .-records__BRFLOAT647
	.align 4
	.type	records__CRFLOAT647, #function
	.proc	07
records__CRFLOAT647:
.LLFB1298:
	save	%sp, -96, %sp
.LLCFI1298:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 24, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1298:
	.size	records__CRFLOAT647, .-records__CRFLOAT647
	.align 4
	.type	records__DRFLOAT647, #function
	.proc	020
records__DRFLOAT647:
.LLFB1299:
	save	%sp, -152, %sp
.LLCFI1299:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1299:
	.size	records__DRFLOAT647, .-records__DRFLOAT647
	.align 4
	.type	records__ERFLOAT647, #function
	.proc	07
records__ERFLOAT647:
.LLFB1300:
	save	%sp, -96, %sp
.LLCFI1300:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 24, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1300:
	.size	records__ERFLOAT647, .-records__ERFLOAT647
	.align 4
	.type	records__FRFLOAT647, #function
	.proc	020
records__FRFLOAT647:
.LLFB1301:
	save	%sp, -152, %sp
.LLCFI1301:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	56, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1301:
	.size	records__FRFLOAT647, .-records__FRFLOAT647
	.align 4
	.type	records__ARFLOAT648, #function
	.proc	07
records__ARFLOAT648:
.LLFB1302:
	save	%sp, -96, %sp
.LLCFI1302:
	mov	%i0, %g1
	add	%g1, 32, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1302:
	.size	records__ARFLOAT648, .-records__ARFLOAT648
	.align 4
	.type	records__BRFLOAT648, #function
	.proc	020
records__BRFLOAT648:
.LLFB1303:
	save	%sp, -160, %sp
.LLCFI1303:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1303:
	.size	records__BRFLOAT648, .-records__BRFLOAT648
	.align 4
	.type	records__CRFLOAT648, #function
	.proc	07
records__CRFLOAT648:
.LLFB1304:
	save	%sp, -96, %sp
.LLCFI1304:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 32, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1304:
	.size	records__CRFLOAT648, .-records__CRFLOAT648
	.align 4
	.type	records__DRFLOAT648, #function
	.proc	020
records__DRFLOAT648:
.LLFB1305:
	save	%sp, -160, %sp
.LLCFI1305:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1305:
	.size	records__DRFLOAT648, .-records__DRFLOAT648
	.align 4
	.type	records__ERFLOAT648, #function
	.proc	07
records__ERFLOAT648:
.LLFB1306:
	save	%sp, -96, %sp
.LLCFI1306:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 32, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1306:
	.size	records__ERFLOAT648, .-records__ERFLOAT648
	.align 4
	.type	records__FRFLOAT648, #function
	.proc	020
records__FRFLOAT648:
.LLFB1307:
	save	%sp, -160, %sp
.LLCFI1307:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	64, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1307:
	.size	records__FRFLOAT648, .-records__FRFLOAT648
	.align 4
	.type	records__ARFLOAT649, #function
	.proc	07
records__ARFLOAT649:
.LLFB1308:
	save	%sp, -96, %sp
.LLCFI1308:
	mov	%i0, %g1
	add	%g1, 32, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1308:
	.size	records__ARFLOAT649, .-records__ARFLOAT649
	.align 4
	.type	records__BRFLOAT649, #function
	.proc	020
records__BRFLOAT649:
.LLFB1309:
	save	%sp, -168, %sp
.LLCFI1309:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1309:
	.size	records__BRFLOAT649, .-records__BRFLOAT649
	.align 4
	.type	records__CRFLOAT649, #function
	.proc	07
records__CRFLOAT649:
.LLFB1310:
	save	%sp, -96, %sp
.LLCFI1310:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 32, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1310:
	.size	records__CRFLOAT649, .-records__CRFLOAT649
	.align 4
	.type	records__DRFLOAT649, #function
	.proc	020
records__DRFLOAT649:
.LLFB1311:
	save	%sp, -168, %sp
.LLCFI1311:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1311:
	.size	records__DRFLOAT649, .-records__DRFLOAT649
	.align 4
	.type	records__ERFLOAT649, #function
	.proc	07
records__ERFLOAT649:
.LLFB1312:
	save	%sp, -96, %sp
.LLCFI1312:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 32, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1312:
	.size	records__ERFLOAT649, .-records__ERFLOAT649
	.align 4
	.type	records__FRFLOAT649, #function
	.proc	020
records__FRFLOAT649:
.LLFB1313:
	save	%sp, -168, %sp
.LLCFI1313:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	72, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1313:
	.size	records__FRFLOAT649, .-records__FRFLOAT649
	.align 4
	.type	records__ARFLOAT6410, #function
	.proc	07
records__ARFLOAT6410:
.LLFB1314:
	save	%sp, -96, %sp
.LLCFI1314:
	mov	%i0, %g1
	add	%g1, 40, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1314:
	.size	records__ARFLOAT6410, .-records__ARFLOAT6410
	.align 4
	.type	records__BRFLOAT6410, #function
	.proc	020
records__BRFLOAT6410:
.LLFB1315:
	save	%sp, -176, %sp
.LLCFI1315:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1315:
	.size	records__BRFLOAT6410, .-records__BRFLOAT6410
	.align 4
	.type	records__CRFLOAT6410, #function
	.proc	07
records__CRFLOAT6410:
.LLFB1316:
	save	%sp, -96, %sp
.LLCFI1316:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	add	%g1, 40, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1316:
	.size	records__CRFLOAT6410, .-records__CRFLOAT6410
	.align 4
	.type	records__DRFLOAT6410, #function
	.proc	020
records__DRFLOAT6410:
.LLFB1317:
	save	%sp, -176, %sp
.LLCFI1317:
	st	%i0, [%fp+68]
	mov	%i1, %g1
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1317:
	.size	records__DRFLOAT6410, .-records__DRFLOAT6410
	.align 4
	.type	records__ERFLOAT6410, #function
	.proc	07
records__ERFLOAT6410:
.LLFB1318:
	save	%sp, -96, %sp
.LLCFI1318:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	add	%g1, 40, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE1318:
	.size	records__ERFLOAT6410, .-records__ERFLOAT6410
	.align 4
	.type	records__FRFLOAT6410, #function
	.proc	020
records__FRFLOAT6410:
.LLFB1319:
	save	%sp, -176, %sp
.LLCFI1319:
	mov	%i0, %g1
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	mov	%o5, %o0
	mov	%g1, %o1
	mov	80, %o2
	call	memcpy, 0
	 nop
	return	%i7+8
	 nop
.LLFE1319:
	.size	records__FRFLOAT6410, .-records__FRFLOAT6410
	.align 4
	.global records_M3
	.type	records_M3, #function
	.proc	0120
records_M3:
.LLFB1320:
	save	%sp, -96, %sp
.LLCFI1320:
	st	%i0, [%fp+68]
	sethi	%hi(MM_records), %g1
	or	%g1, %lo(MM_records), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1320:
	.size	records_M3, .-records_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 23776
L_1:
	.ascii	"records_M3"
	.skip 1
	.ascii	"FRFLOAT6410"
	.skip 1
	.ascii	"ERFLOAT6410"
	.skip 1
	.ascii	"DRFLOAT6410"
	.skip 1
	.ascii	"CRFLOAT6410"
	.skip 1
	.ascii	"BRFLOAT6410"
	.skip 1
	.ascii	"ARFLOAT6410"
	.skip 1
	.ascii	"FRFLOAT649"
	.skip 1
	.ascii	"ERFLOAT649"
	.skip 1
	.ascii	"DRFLOAT649"
	.skip 1
	.ascii	"CRFLOAT649"
	.skip 1
	.ascii	"BRFLOAT649"
	.skip 1
	.ascii	"ARFLOAT649"
	.skip 1
	.ascii	"FRFLOAT648"
	.skip 1
	.ascii	"ERFLOAT648"
	.skip 1
	.ascii	"DRFLOAT648"
	.skip 1
	.ascii	"CRFLOAT648"
	.skip 1
	.ascii	"BRFLOAT648"
	.skip 1
	.ascii	"ARFLOAT648"
	.skip 1
	.ascii	"FRFLOAT647"
	.skip 1
	.ascii	"ERFLOAT647"
	.skip 1
	.ascii	"DRFLOAT647"
	.skip 1
	.ascii	"CRFLOAT647"
	.skip 1
	.ascii	"BRFLOAT647"
	.skip 1
	.ascii	"ARFLOAT647"
	.skip 1
	.ascii	"FRFLOAT646"
	.skip 1
	.ascii	"ERFLOAT646"
	.skip 1
	.ascii	"DRFLOAT646"
	.skip 1
	.ascii	"CRFLOAT646"
	.skip 1
	.ascii	"BRFLOAT646"
	.skip 1
	.ascii	"ARFLOAT646"
	.skip 1
	.ascii	"FRFLOAT645"
	.skip 1
	.ascii	"ERFLOAT645"
	.skip 1
	.ascii	"DRFLOAT645"
	.skip 1
	.ascii	"CRFLOAT645"
	.skip 1
	.ascii	"BRFLOAT645"
	.skip 1
	.ascii	"ARFLOAT645"
	.skip 1
	.ascii	"FRFLOAT644"
	.skip 1
	.ascii	"ERFLOAT644"
	.skip 1
	.ascii	"DRFLOAT644"
	.skip 1
	.ascii	"CRFLOAT644"
	.skip 1
	.ascii	"BRFLOAT644"
	.skip 1
	.ascii	"ARFLOAT644"
	.skip 1
	.ascii	"FRFLOAT643"
	.skip 1
	.ascii	"ERFLOAT643"
	.skip 1
	.ascii	"DRFLOAT643"
	.skip 1
	.ascii	"CRFLOAT643"
	.skip 1
	.ascii	"BRFLOAT643"
	.skip 1
	.ascii	"ARFLOAT643"
	.skip 1
	.ascii	"FRFLOAT642"
	.skip 1
	.ascii	"ERFLOAT642"
	.skip 1
	.ascii	"DRFLOAT642"
	.skip 1
	.ascii	"CRFLOAT642"
	.skip 1
	.ascii	"BRFLOAT642"
	.skip 1
	.ascii	"ARFLOAT642"
	.skip 1
	.ascii	"FRFLOAT641"
	.skip 1
	.ascii	"ERFLOAT641"
	.skip 1
	.ascii	"DRFLOAT641"
	.skip 1
	.ascii	"CRFLOAT641"
	.skip 1
	.ascii	"BRFLOAT641"
	.skip 1
	.ascii	"ARFLOAT641"
	.skip 1
	.ascii	"FRFLOAT3210"
	.skip 1
	.ascii	"ERFLOAT3210"
	.skip 1
	.ascii	"DRFLOAT3210"
	.skip 1
	.ascii	"CRFLOAT3210"
	.skip 1
	.ascii	"BRFLOAT3210"
	.skip 1
	.ascii	"ARFLOAT3210"
	.skip 1
	.ascii	"FRFLOAT329"
	.skip 1
	.ascii	"ERFLOAT329"
	.skip 1
	.ascii	"DRFLOAT329"
	.skip 1
	.ascii	"CRFLOAT329"
	.skip 1
	.ascii	"BRFLOAT329"
	.skip 1
	.ascii	"ARFLOAT329"
	.skip 1
	.ascii	"FRFLOAT328"
	.skip 1
	.ascii	"ERFLOAT328"
	.skip 1
	.ascii	"DRFLOAT328"
	.skip 1
	.ascii	"CRFLOAT328"
	.skip 1
	.ascii	"BRFLOAT328"
	.skip 1
	.ascii	"ARFLOAT328"
	.skip 1
	.ascii	"FRFLOAT327"
	.skip 1
	.ascii	"ERFLOAT327"
	.skip 1
	.ascii	"DRFLOAT327"
	.skip 1
	.ascii	"CRFLOAT327"
	.skip 1
	.ascii	"BRFLOAT327"
	.skip 1
	.ascii	"ARFLOAT327"
	.skip 1
	.ascii	"FRFLOAT326"
	.skip 1
	.ascii	"ERFLOAT326"
	.skip 1
	.ascii	"DRFLOAT326"
	.skip 1
	.ascii	"CRFLOAT326"
	.skip 1
	.ascii	"BRFLOAT326"
	.skip 1
	.ascii	"ARFLOAT326"
	.skip 1
	.ascii	"FRFLOAT325"
	.skip 1
	.ascii	"ERFLOAT325"
	.skip 1
	.ascii	"DRFLOAT325"
	.skip 1
	.ascii	"CRFLOAT325"
	.skip 1
	.ascii	"BRFLOAT325"
	.skip 1
	.ascii	"ARFLOAT325"
	.skip 1
	.ascii	"FRFLOAT324"
	.skip 1
	.ascii	"ERFLOAT324"
	.skip 1
	.ascii	"DRFLOAT324"
	.skip 1
	.ascii	"CRFLOAT324"
	.skip 1
	.ascii	"BRFLOAT324"
	.skip 1
	.ascii	"ARFLOAT324"
	.skip 1
	.ascii	"FRFLOAT323"
	.skip 1
	.ascii	"ERFLOAT323"
	.skip 1
	.ascii	"DRFLOAT323"
	.skip 1
	.ascii	"CRFLOAT323"
	.skip 1
	.ascii	"BRFLOAT323"
	.skip 1
	.ascii	"ARFLOAT323"
	.skip 1
	.ascii	"FRFLOAT322"
	.skip 1
	.ascii	"ERFLOAT322"
	.skip 1
	.ascii	"DRFLOAT322"
	.skip 1
	.ascii	"CRFLOAT322"
	.skip 1
	.ascii	"BRFLOAT322"
	.skip 1
	.ascii	"ARFLOAT322"
	.skip 1
	.ascii	"FRFLOAT321"
	.skip 1
	.ascii	"ERFLOAT321"
	.skip 1
	.ascii	"DRFLOAT321"
	.skip 1
	.ascii	"CRFLOAT321"
	.skip 1
	.ascii	"BRFLOAT321"
	.skip 1
	.ascii	"ARFLOAT321"
	.skip 1
	.ascii	"FRLONGCARD10"
	.skip 1
	.ascii	"ERLONGCARD10"
	.skip 1
	.ascii	"DRLONGCARD10"
	.skip 1
	.ascii	"CRLONGCARD10"
	.skip 1
	.ascii	"BRLONGCARD10"
	.skip 1
	.ascii	"ARLONGCARD10"
	.skip 1
	.ascii	"FRLONGCARD9"
	.skip 1
	.ascii	"ERLONGCARD9"
	.skip 1
	.ascii	"DRLONGCARD9"
	.skip 1
	.ascii	"CRLONGCARD9"
	.skip 1
	.ascii	"BRLONGCARD9"
	.skip 1
	.ascii	"ARLONGCARD9"
	.skip 1
	.ascii	"FRLONGCARD8"
	.skip 1
	.ascii	"ERLONGCARD8"
	.skip 1
	.ascii	"DRLONGCARD8"
	.skip 1
	.ascii	"CRLONGCARD8"
	.skip 1
	.ascii	"BRLONGCARD8"
	.skip 1
	.ascii	"ARLONGCARD8"
	.skip 1
	.ascii	"FRLONGCARD7"
	.skip 1
	.ascii	"ERLONGCARD7"
	.skip 1
	.ascii	"DRLONGCARD7"
	.skip 1
	.ascii	"CRLONGCARD7"
	.skip 1
	.ascii	"BRLONGCARD7"
	.skip 1
	.ascii	"ARLONGCARD7"
	.skip 1
	.ascii	"FRLONGCARD6"
	.skip 1
	.ascii	"ERLONGCARD6"
	.skip 1
	.ascii	"DRLONGCARD6"
	.skip 1
	.ascii	"CRLONGCARD6"
	.skip 1
	.ascii	"BRLONGCARD6"
	.skip 1
	.ascii	"ARLONGCARD6"
	.skip 1
	.ascii	"FRLONGCARD5"
	.skip 1
	.ascii	"ERLONGCARD5"
	.skip 1
	.ascii	"DRLONGCARD5"
	.skip 1
	.ascii	"CRLONGCARD5"
	.skip 1
	.ascii	"BRLONGCARD5"
	.skip 1
	.ascii	"ARLONGCARD5"
	.skip 1
	.ascii	"FRLONGCARD4"
	.skip 1
	.ascii	"ERLONGCARD4"
	.skip 1
	.ascii	"DRLONGCARD4"
	.skip 1
	.ascii	"CRLONGCARD4"
	.skip 1
	.ascii	"BRLONGCARD4"
	.skip 1
	.ascii	"ARLONGCARD4"
	.skip 1
	.ascii	"FRLONGCARD3"
	.skip 1
	.ascii	"ERLONGCARD3"
	.skip 1
	.ascii	"DRLONGCARD3"
	.skip 1
	.ascii	"CRLONGCARD3"
	.skip 1
	.ascii	"BRLONGCARD3"
	.skip 1
	.ascii	"ARLONGCARD3"
	.skip 1
	.ascii	"FRLONGCARD2"
	.skip 1
	.ascii	"ERLONGCARD2"
	.skip 1
	.ascii	"DRLONGCARD2"
	.skip 1
	.ascii	"CRLONGCARD2"
	.skip 1
	.ascii	"BRLONGCARD2"
	.skip 1
	.ascii	"ARLONGCARD2"
	.skip 1
	.ascii	"FRLONGCARD1"
	.skip 1
	.ascii	"ERLONGCARD1"
	.skip 1
	.ascii	"DRLONGCARD1"
	.skip 1
	.ascii	"CRLONGCARD1"
	.skip 1
	.ascii	"BRLONGCARD1"
	.skip 1
	.ascii	"ARLONGCARD1"
	.skip 1
	.ascii	"FRLONGINT10"
	.skip 1
	.ascii	"ERLONGINT10"
	.skip 1
	.ascii	"DRLONGINT10"
	.skip 1
	.ascii	"CRLONGINT10"
	.skip 1
	.ascii	"BRLONGINT10"
	.skip 1
	.ascii	"ARLONGINT10"
	.skip 1
	.ascii	"FRLONGINT9"
	.skip 1
	.ascii	"ERLONGINT9"
	.skip 1
	.ascii	"DRLONGINT9"
	.skip 1
	.ascii	"CRLONGINT9"
	.skip 1
	.ascii	"BRLONGINT9"
	.skip 1
	.ascii	"ARLONGINT9"
	.skip 1
	.ascii	"FRLONGINT8"
	.skip 1
	.ascii	"ERLONGINT8"
	.skip 1
	.ascii	"DRLONGINT8"
	.skip 1
	.ascii	"CRLONGINT8"
	.skip 1
	.ascii	"BRLONGINT8"
	.skip 1
	.ascii	"ARLONGINT8"
	.skip 1
	.ascii	"FRLONGINT7"
	.skip 1
	.ascii	"ERLONGINT7"
	.skip 1
	.ascii	"DRLONGINT7"
	.skip 1
	.ascii	"CRLONGINT7"
	.skip 1
	.ascii	"BRLONGINT7"
	.skip 1
	.ascii	"ARLONGINT7"
	.skip 1
	.ascii	"FRLONGINT6"
	.skip 1
	.ascii	"ERLONGINT6"
	.skip 1
	.ascii	"DRLONGINT6"
	.skip 1
	.ascii	"CRLONGINT6"
	.skip 1
	.ascii	"BRLONGINT6"
	.skip 1
	.ascii	"ARLONGINT6"
	.skip 1
	.ascii	"FRLONGINT5"
	.skip 1
	.ascii	"ERLONGINT5"
	.skip 1
	.ascii	"DRLONGINT5"
	.skip 1
	.ascii	"CRLONGINT5"
	.skip 1
	.ascii	"BRLONGINT5"
	.skip 1
	.ascii	"ARLONGINT5"
	.skip 1
	.ascii	"FRLONGINT4"
	.skip 1
	.ascii	"ERLONGINT4"
	.skip 1
	.ascii	"DRLONGINT4"
	.skip 1
	.ascii	"CRLONGINT4"
	.skip 1
	.ascii	"BRLONGINT4"
	.skip 1
	.ascii	"ARLONGINT4"
	.skip 1
	.ascii	"FRLONGINT3"
	.skip 1
	.ascii	"ERLONGINT3"
	.skip 1
	.ascii	"DRLONGINT3"
	.skip 1
	.ascii	"CRLONGINT3"
	.skip 1
	.ascii	"BRLONGINT3"
	.skip 1
	.ascii	"ARLONGINT3"
	.skip 1
	.ascii	"FRLONGINT2"
	.skip 1
	.ascii	"ERLONGINT2"
	.skip 1
	.ascii	"DRLONGINT2"
	.skip 1
	.ascii	"CRLONGINT2"
	.skip 1
	.ascii	"BRLONGINT2"
	.skip 1
	.ascii	"ARLONGINT2"
	.skip 1
	.ascii	"FRLONGINT1"
	.skip 1
	.ascii	"ERLONGINT1"
	.skip 1
	.ascii	"DRLONGINT1"
	.skip 1
	.ascii	"CRLONGINT1"
	.skip 1
	.ascii	"BRLONGINT1"
	.skip 1
	.ascii	"ARLONGINT1"
	.skip 1
	.ascii	"FRCARDINAL10"
	.skip 1
	.ascii	"ERCARDINAL10"
	.skip 1
	.ascii	"DRCARDINAL10"
	.skip 1
	.ascii	"CRCARDINAL10"
	.skip 1
	.ascii	"BRCARDINAL10"
	.skip 1
	.ascii	"ARCARDINAL10"
	.skip 1
	.ascii	"FRCARDINAL9"
	.skip 1
	.ascii	"ERCARDINAL9"
	.skip 1
	.ascii	"DRCARDINAL9"
	.skip 1
	.ascii	"CRCARDINAL9"
	.skip 1
	.ascii	"BRCARDINAL9"
	.skip 1
	.ascii	"ARCARDINAL9"
	.skip 1
	.ascii	"FRCARDINAL8"
	.skip 1
	.ascii	"ERCARDINAL8"
	.skip 1
	.ascii	"DRCARDINAL8"
	.skip 1
	.ascii	"CRCARDINAL8"
	.skip 1
	.ascii	"BRCARDINAL8"
	.skip 1
	.ascii	"ARCARDINAL8"
	.skip 1
	.ascii	"FRCARDINAL7"
	.skip 1
	.ascii	"ERCARDINAL7"
	.skip 1
	.ascii	"DRCARDINAL7"
	.skip 1
	.ascii	"CRCARDINAL7"
	.skip 1
	.ascii	"BRCARDINAL7"
	.skip 1
	.ascii	"ARCARDINAL7"
	.skip 1
	.ascii	"FRCARDINAL6"
	.skip 1
	.ascii	"ERCARDINAL6"
	.skip 1
	.ascii	"DRCARDINAL6"
	.skip 1
	.ascii	"CRCARDINAL6"
	.skip 1
	.ascii	"BRCARDINAL6"
	.skip 1
	.ascii	"ARCARDINAL6"
	.skip 1
	.ascii	"FRCARDINAL5"
	.skip 1
	.ascii	"ERCARDINAL5"
	.skip 1
	.ascii	"DRCARDINAL5"
	.skip 1
	.ascii	"CRCARDINAL5"
	.skip 1
	.ascii	"BRCARDINAL5"
	.skip 1
	.ascii	"ARCARDINAL5"
	.skip 1
	.ascii	"FRCARDINAL4"
	.skip 1
	.ascii	"ERCARDINAL4"
	.skip 1
	.ascii	"DRCARDINAL4"
	.skip 1
	.ascii	"CRCARDINAL4"
	.skip 1
	.ascii	"BRCARDINAL4"
	.skip 1
	.ascii	"ARCARDINAL4"
	.skip 1
	.ascii	"FRCARDINAL3"
	.skip 1
	.ascii	"ERCARDINAL3"
	.skip 1
	.ascii	"DRCARDINAL3"
	.skip 1
	.ascii	"CRCARDINAL3"
	.skip 1
	.ascii	"BRCARDINAL3"
	.skip 1
	.ascii	"ARCARDINAL3"
	.skip 1
	.ascii	"FRCARDINAL2"
	.skip 1
	.ascii	"ERCARDINAL2"
	.skip 1
	.ascii	"DRCARDINAL2"
	.skip 1
	.ascii	"CRCARDINAL2"
	.skip 1
	.ascii	"BRCARDINAL2"
	.skip 1
	.ascii	"ARCARDINAL2"
	.skip 1
	.ascii	"FRCARDINAL1"
	.skip 1
	.ascii	"ERCARDINAL1"
	.skip 1
	.ascii	"DRCARDINAL1"
	.skip 1
	.ascii	"CRCARDINAL1"
	.skip 1
	.ascii	"BRCARDINAL1"
	.skip 1
	.ascii	"ARCARDINAL1"
	.skip 1
	.ascii	"FRINTEGER10"
	.skip 1
	.ascii	"ERINTEGER10"
	.skip 1
	.ascii	"DRINTEGER10"
	.skip 1
	.ascii	"CRINTEGER10"
	.skip 1
	.ascii	"BRINTEGER10"
	.skip 1
	.ascii	"ARINTEGER10"
	.skip 1
	.ascii	"FRINTEGER9"
	.skip 1
	.ascii	"ERINTEGER9"
	.skip 1
	.ascii	"DRINTEGER9"
	.skip 1
	.ascii	"CRINTEGER9"
	.skip 1
	.ascii	"BRINTEGER9"
	.skip 1
	.ascii	"ARINTEGER9"
	.skip 1
	.ascii	"FRINTEGER8"
	.skip 1
	.ascii	"ERINTEGER8"
	.skip 1
	.ascii	"DRINTEGER8"
	.skip 1
	.ascii	"CRINTEGER8"
	.skip 1
	.ascii	"BRINTEGER8"
	.skip 1
	.ascii	"ARINTEGER8"
	.skip 1
	.ascii	"FRINTEGER7"
	.skip 1
	.ascii	"ERINTEGER7"
	.skip 1
	.ascii	"DRINTEGER7"
	.skip 1
	.ascii	"CRINTEGER7"
	.skip 1
	.ascii	"BRINTEGER7"
	.skip 1
	.ascii	"ARINTEGER7"
	.skip 1
	.ascii	"FRINTEGER6"
	.skip 1
	.ascii	"ERINTEGER6"
	.skip 1
	.ascii	"DRINTEGER6"
	.skip 1
	.ascii	"CRINTEGER6"
	.skip 1
	.ascii	"BRINTEGER6"
	.skip 1
	.ascii	"ARINTEGER6"
	.skip 1
	.ascii	"FRINTEGER5"
	.skip 1
	.ascii	"ERINTEGER5"
	.skip 1
	.ascii	"DRINTEGER5"
	.skip 1
	.ascii	"CRINTEGER5"
	.skip 1
	.ascii	"BRINTEGER5"
	.skip 1
	.ascii	"ARINTEGER5"
	.skip 1
	.ascii	"FRINTEGER4"
	.skip 1
	.ascii	"ERINTEGER4"
	.skip 1
	.ascii	"DRINTEGER4"
	.skip 1
	.ascii	"CRINTEGER4"
	.skip 1
	.ascii	"BRINTEGER4"
	.skip 1
	.ascii	"ARINTEGER4"
	.skip 1
	.ascii	"FRINTEGER3"
	.skip 1
	.ascii	"ERINTEGER3"
	.skip 1
	.ascii	"DRINTEGER3"
	.skip 1
	.ascii	"CRINTEGER3"
	.skip 1
	.ascii	"BRINTEGER3"
	.skip 1
	.ascii	"ARINTEGER3"
	.skip 1
	.ascii	"FRINTEGER2"
	.skip 1
	.ascii	"ERINTEGER2"
	.skip 1
	.ascii	"DRINTEGER2"
	.skip 1
	.ascii	"CRINTEGER2"
	.skip 1
	.ascii	"BRINTEGER2"
	.skip 1
	.ascii	"ARINTEGER2"
	.skip 1
	.ascii	"FRINTEGER1"
	.skip 1
	.ascii	"ERINTEGER1"
	.skip 1
	.ascii	"DRINTEGER1"
	.skip 1
	.ascii	"CRINTEGER1"
	.skip 1
	.ascii	"BRINTEGER1"
	.skip 1
	.ascii	"ARINTEGER1"
	.skip 1
	.ascii	"FRUINT6410"
	.skip 1
	.ascii	"ERUINT6410"
	.skip 1
	.ascii	"DRUINT6410"
	.skip 1
	.ascii	"CRUINT6410"
	.skip 1
	.ascii	"BRUINT6410"
	.skip 1
	.ascii	"ARUINT6410"
	.skip 1
	.ascii	"FRUINT649"
	.skip 1
	.ascii	"ERUINT649"
	.skip 1
	.ascii	"DRUINT649"
	.skip 1
	.ascii	"CRUINT649"
	.skip 1
	.ascii	"BRUINT649"
	.skip 1
	.ascii	"ARUINT649"
	.skip 1
	.ascii	"FRUINT648"
	.skip 1
	.ascii	"ERUINT648"
	.skip 1
	.ascii	"DRUINT648"
	.skip 1
	.ascii	"CRUINT648"
	.skip 1
	.ascii	"BRUINT648"
	.skip 1
	.ascii	"ARUINT648"
	.skip 1
	.ascii	"FRUINT647"
	.skip 1
	.ascii	"ERUINT647"
	.skip 1
	.ascii	"DRUINT647"
	.skip 1
	.ascii	"CRUINT647"
	.skip 1
	.ascii	"BRUINT647"
	.skip 1
	.ascii	"ARUINT647"
	.skip 1
	.ascii	"FRUINT646"
	.skip 1
	.ascii	"ERUINT646"
	.skip 1
	.ascii	"DRUINT646"
	.skip 1
	.ascii	"CRUINT646"
	.skip 1
	.ascii	"BRUINT646"
	.skip 1
	.ascii	"ARUINT646"
	.skip 1
	.ascii	"FRUINT645"
	.skip 1
	.ascii	"ERUINT645"
	.skip 1
	.ascii	"DRUINT645"
	.skip 1
	.ascii	"CRUINT645"
	.skip 1
	.ascii	"BRUINT645"
	.skip 1
	.ascii	"ARUINT645"
	.skip 1
	.ascii	"FRUINT644"
	.skip 1
	.ascii	"ERUINT644"
	.skip 1
	.ascii	"DRUINT644"
	.skip 1
	.ascii	"CRUINT644"
	.skip 1
	.ascii	"BRUINT644"
	.skip 1
	.ascii	"ARUINT644"
	.skip 1
	.ascii	"FRUINT643"
	.skip 1
	.ascii	"ERUINT643"
	.skip 1
	.ascii	"DRUINT643"
	.skip 1
	.ascii	"CRUINT643"
	.skip 1
	.ascii	"BRUINT643"
	.skip 1
	.ascii	"ARUINT643"
	.skip 1
	.ascii	"FRUINT642"
	.skip 1
	.ascii	"ERUINT642"
	.skip 1
	.ascii	"DRUINT642"
	.skip 1
	.ascii	"CRUINT642"
	.skip 1
	.ascii	"BRUINT642"
	.skip 1
	.ascii	"ARUINT642"
	.skip 1
	.ascii	"FRUINT641"
	.skip 1
	.ascii	"ERUINT641"
	.skip 1
	.ascii	"DRUINT641"
	.skip 1
	.ascii	"CRUINT641"
	.skip 1
	.ascii	"BRUINT641"
	.skip 1
	.ascii	"ARUINT641"
	.skip 1
	.ascii	"FRINT6410"
	.skip 1
	.ascii	"ERINT6410"
	.skip 1
	.ascii	"DRINT6410"
	.skip 1
	.ascii	"CRINT6410"
	.skip 1
	.ascii	"BRINT6410"
	.skip 1
	.ascii	"ARINT6410"
	.skip 1
	.ascii	"FRINT649"
	.skip 1
	.ascii	"ERINT649"
	.skip 1
	.ascii	"DRINT649"
	.skip 1
	.ascii	"CRINT649"
	.skip 1
	.ascii	"BRINT649"
	.skip 1
	.ascii	"ARINT649"
	.skip 1
	.ascii	"FRINT648"
	.skip 1
	.ascii	"ERINT648"
	.skip 1
	.ascii	"DRINT648"
	.skip 1
	.ascii	"CRINT648"
	.skip 1
	.ascii	"BRINT648"
	.skip 1
	.ascii	"ARINT648"
	.skip 1
	.ascii	"FRINT647"
	.skip 1
	.ascii	"ERINT647"
	.skip 1
	.ascii	"DRINT647"
	.skip 1
	.ascii	"CRINT647"
	.skip 1
	.ascii	"BRINT647"
	.skip 1
	.ascii	"ARINT647"
	.skip 1
	.ascii	"FRINT646"
	.skip 1
	.ascii	"ERINT646"
	.skip 1
	.ascii	"DRINT646"
	.skip 1
	.ascii	"CRINT646"
	.skip 1
	.ascii	"BRINT646"
	.skip 1
	.ascii	"ARINT646"
	.skip 1
	.ascii	"FRINT645"
	.skip 1
	.ascii	"ERINT645"
	.skip 1
	.ascii	"DRINT645"
	.skip 1
	.ascii	"CRINT645"
	.skip 1
	.ascii	"BRINT645"
	.skip 1
	.ascii	"ARINT645"
	.skip 1
	.ascii	"FRINT644"
	.skip 1
	.ascii	"ERINT644"
	.skip 1
	.ascii	"DRINT644"
	.skip 1
	.ascii	"CRINT644"
	.skip 1
	.ascii	"BRINT644"
	.skip 1
	.ascii	"ARINT644"
	.skip 1
	.ascii	"FRINT643"
	.skip 1
	.ascii	"ERINT643"
	.skip 1
	.ascii	"DRINT643"
	.skip 1
	.ascii	"CRINT643"
	.skip 1
	.ascii	"BRINT643"
	.skip 1
	.ascii	"ARINT643"
	.skip 1
	.ascii	"FRINT642"
	.skip 1
	.ascii	"ERINT642"
	.skip 1
	.ascii	"DRINT642"
	.skip 1
	.ascii	"CRINT642"
	.skip 1
	.ascii	"BRINT642"
	.skip 1
	.ascii	"ARINT642"
	.skip 1
	.ascii	"FRINT641"
	.skip 1
	.ascii	"ERINT641"
	.skip 1
	.ascii	"DRINT641"
	.skip 1
	.ascii	"CRINT641"
	.skip 1
	.ascii	"BRINT641"
	.skip 1
	.ascii	"ARINT641"
	.skip 1
	.ascii	"FRUINT3210"
	.skip 1
	.ascii	"ERUINT3210"
	.skip 1
	.ascii	"DRUINT3210"
	.skip 1
	.ascii	"CRUINT3210"
	.skip 1
	.ascii	"BRUINT3210"
	.skip 1
	.ascii	"ARUINT3210"
	.skip 1
	.ascii	"FRUINT329"
	.skip 1
	.ascii	"ERUINT329"
	.skip 1
	.ascii	"DRUINT329"
	.skip 1
	.ascii	"CRUINT329"
	.skip 1
	.ascii	"BRUINT329"
	.skip 1
	.ascii	"ARUINT329"
	.skip 1
	.ascii	"FRUINT328"
	.skip 1
	.ascii	"ERUINT328"
	.skip 1
	.ascii	"DRUINT328"
	.skip 1
	.ascii	"CRUINT328"
	.skip 1
	.ascii	"BRUINT328"
	.skip 1
	.ascii	"ARUINT328"
	.skip 1
	.ascii	"FRUINT327"
	.skip 1
	.ascii	"ERUINT327"
	.skip 1
	.ascii	"DRUINT327"
	.skip 1
	.ascii	"CRUINT327"
	.skip 1
	.ascii	"BRUINT327"
	.skip 1
	.ascii	"ARUINT327"
	.skip 1
	.ascii	"FRUINT326"
	.skip 1
	.ascii	"ERUINT326"
	.skip 1
	.ascii	"DRUINT326"
	.skip 1
	.ascii	"CRUINT326"
	.skip 1
	.ascii	"BRUINT326"
	.skip 1
	.ascii	"ARUINT326"
	.skip 1
	.ascii	"FRUINT325"
	.skip 1
	.ascii	"ERUINT325"
	.skip 1
	.ascii	"DRUINT325"
	.skip 1
	.ascii	"CRUINT325"
	.skip 1
	.ascii	"BRUINT325"
	.skip 1
	.ascii	"ARUINT325"
	.skip 1
	.ascii	"FRUINT324"
	.skip 1
	.ascii	"ERUINT324"
	.skip 1
	.ascii	"DRUINT324"
	.skip 1
	.ascii	"CRUINT324"
	.skip 1
	.ascii	"BRUINT324"
	.skip 1
	.ascii	"ARUINT324"
	.skip 1
	.ascii	"FRUINT323"
	.skip 1
	.ascii	"ERUINT323"
	.skip 1
	.ascii	"DRUINT323"
	.skip 1
	.ascii	"CRUINT323"
	.skip 1
	.ascii	"BRUINT323"
	.skip 1
	.ascii	"ARUINT323"
	.skip 1
	.ascii	"FRUINT322"
	.skip 1
	.ascii	"ERUINT322"
	.skip 1
	.ascii	"DRUINT322"
	.skip 1
	.ascii	"CRUINT322"
	.skip 1
	.ascii	"BRUINT322"
	.skip 1
	.ascii	"ARUINT322"
	.skip 1
	.ascii	"FRUINT321"
	.skip 1
	.ascii	"ERUINT321"
	.skip 1
	.ascii	"DRUINT321"
	.skip 1
	.ascii	"CRUINT321"
	.skip 1
	.ascii	"BRUINT321"
	.skip 1
	.ascii	"ARUINT321"
	.skip 1
	.ascii	"FRINT3210"
	.skip 1
	.ascii	"ERINT3210"
	.skip 1
	.ascii	"DRINT3210"
	.skip 1
	.ascii	"CRINT3210"
	.skip 1
	.ascii	"BRINT3210"
	.skip 1
	.ascii	"ARINT3210"
	.skip 1
	.ascii	"FRINT329"
	.skip 1
	.ascii	"ERINT329"
	.skip 1
	.ascii	"DRINT329"
	.skip 1
	.ascii	"CRINT329"
	.skip 1
	.ascii	"BRINT329"
	.skip 1
	.ascii	"ARINT329"
	.skip 1
	.ascii	"FRINT328"
	.skip 1
	.ascii	"ERINT328"
	.skip 1
	.ascii	"DRINT328"
	.skip 1
	.ascii	"CRINT328"
	.skip 1
	.ascii	"BRINT328"
	.skip 1
	.ascii	"ARINT328"
	.skip 1
	.ascii	"FRINT327"
	.skip 1
	.ascii	"ERINT327"
	.skip 1
	.ascii	"DRINT327"
	.skip 1
	.ascii	"CRINT327"
	.skip 1
	.ascii	"BRINT327"
	.skip 1
	.ascii	"ARINT327"
	.skip 1
	.ascii	"FRINT326"
	.skip 1
	.ascii	"ERINT326"
	.skip 1
	.ascii	"DRINT326"
	.skip 1
	.ascii	"CRINT326"
	.skip 1
	.ascii	"BRINT326"
	.skip 1
	.ascii	"ARINT326"
	.skip 1
	.ascii	"FRINT325"
	.skip 1
	.ascii	"ERINT325"
	.skip 1
	.ascii	"DRINT325"
	.skip 1
	.ascii	"CRINT325"
	.skip 1
	.ascii	"BRINT325"
	.skip 1
	.ascii	"ARINT325"
	.skip 1
	.ascii	"FRINT324"
	.skip 1
	.ascii	"ERINT324"
	.skip 1
	.ascii	"DRINT324"
	.skip 1
	.ascii	"CRINT324"
	.skip 1
	.ascii	"BRINT324"
	.skip 1
	.ascii	"ARINT324"
	.skip 1
	.ascii	"FRINT323"
	.skip 1
	.ascii	"ERINT323"
	.skip 1
	.ascii	"DRINT323"
	.skip 1
	.ascii	"CRINT323"
	.skip 1
	.ascii	"BRINT323"
	.skip 1
	.ascii	"ARINT323"
	.skip 1
	.ascii	"FRINT322"
	.skip 1
	.ascii	"ERINT322"
	.skip 1
	.ascii	"DRINT322"
	.skip 1
	.ascii	"CRINT322"
	.skip 1
	.ascii	"BRINT322"
	.skip 1
	.ascii	"ARINT322"
	.skip 1
	.ascii	"FRINT321"
	.skip 1
	.ascii	"ERINT321"
	.skip 1
	.ascii	"DRINT321"
	.skip 1
	.ascii	"CRINT321"
	.skip 1
	.ascii	"BRINT321"
	.skip 1
	.ascii	"ARINT321"
	.skip 1
	.ascii	"FRWIDECHAR10"
	.skip 1
	.ascii	"ERWIDECHAR10"
	.skip 1
	.ascii	"DRWIDECHAR10"
	.skip 1
	.ascii	"CRWIDECHAR10"
	.skip 1
	.ascii	"BRWIDECHAR10"
	.skip 1
	.ascii	"ARWIDECHAR10"
	.skip 1
	.ascii	"FRWIDECHAR9"
	.skip 1
	.ascii	"ERWIDECHAR9"
	.skip 1
	.ascii	"DRWIDECHAR9"
	.skip 1
	.ascii	"CRWIDECHAR9"
	.skip 1
	.ascii	"BRWIDECHAR9"
	.skip 1
	.ascii	"ARWIDECHAR9"
	.skip 1
	.ascii	"FRWIDECHAR8"
	.skip 1
	.ascii	"ERWIDECHAR8"
	.skip 1
	.ascii	"DRWIDECHAR8"
	.skip 1
	.ascii	"CRWIDECHAR8"
	.skip 1
	.ascii	"BRWIDECHAR8"
	.skip 1
	.ascii	"ARWIDECHAR8"
	.skip 1
	.ascii	"FRWIDECHAR7"
	.skip 1
	.ascii	"ERWIDECHAR7"
	.skip 1
	.ascii	"DRWIDECHAR7"
	.skip 1
	.ascii	"CRWIDECHAR7"
	.skip 1
	.ascii	"BRWIDECHAR7"
	.skip 1
	.ascii	"ARWIDECHAR7"
	.skip 1
	.ascii	"FRWIDECHAR6"
	.skip 1
	.ascii	"ERWIDECHAR6"
	.skip 1
	.ascii	"DRWIDECHAR6"
	.skip 1
	.ascii	"CRWIDECHAR6"
	.skip 1
	.ascii	"BRWIDECHAR6"
	.skip 1
	.ascii	"ARWIDECHAR6"
	.skip 1
	.ascii	"FRWIDECHAR5"
	.skip 1
	.ascii	"ERWIDECHAR5"
	.skip 1
	.ascii	"DRWIDECHAR5"
	.skip 1
	.ascii	"CRWIDECHAR5"
	.skip 1
	.ascii	"BRWIDECHAR5"
	.skip 1
	.ascii	"ARWIDECHAR5"
	.skip 1
	.ascii	"FRWIDECHAR4"
	.skip 1
	.ascii	"ERWIDECHAR4"
	.skip 1
	.ascii	"DRWIDECHAR4"
	.skip 1
	.ascii	"CRWIDECHAR4"
	.skip 1
	.ascii	"BRWIDECHAR4"
	.skip 1
	.ascii	"ARWIDECHAR4"
	.skip 1
	.ascii	"FRWIDECHAR3"
	.skip 1
	.ascii	"ERWIDECHAR3"
	.skip 1
	.ascii	"DRWIDECHAR3"
	.skip 1
	.ascii	"CRWIDECHAR3"
	.skip 1
	.ascii	"BRWIDECHAR3"
	.skip 1
	.ascii	"ARWIDECHAR3"
	.skip 1
	.ascii	"FRWIDECHAR2"
	.skip 1
	.ascii	"ERWIDECHAR2"
	.skip 1
	.ascii	"DRWIDECHAR2"
	.skip 1
	.ascii	"CRWIDECHAR2"
	.skip 1
	.ascii	"BRWIDECHAR2"
	.skip 1
	.ascii	"ARWIDECHAR2"
	.skip 1
	.ascii	"FRWIDECHAR1"
	.skip 1
	.ascii	"ERWIDECHAR1"
	.skip 1
	.ascii	"DRWIDECHAR1"
	.skip 1
	.ascii	"CRWIDECHAR1"
	.skip 1
	.ascii	"BRWIDECHAR1"
	.skip 1
	.ascii	"ARWIDECHAR1"
	.skip 1
	.ascii	"FRBOOLEAN10"
	.skip 1
	.ascii	"ERBOOLEAN10"
	.skip 1
	.ascii	"DRBOOLEAN10"
	.skip 1
	.ascii	"CRBOOLEAN10"
	.skip 1
	.ascii	"BRBOOLEAN10"
	.skip 1
	.ascii	"ARBOOLEAN10"
	.skip 1
	.ascii	"FRBOOLEAN9"
	.skip 1
	.ascii	"ERBOOLEAN9"
	.skip 1
	.ascii	"DRBOOLEAN9"
	.skip 1
	.ascii	"CRBOOLEAN9"
	.skip 1
	.ascii	"BRBOOLEAN9"
	.skip 1
	.ascii	"ARBOOLEAN9"
	.skip 1
	.ascii	"FRBOOLEAN8"
	.skip 1
	.ascii	"ERBOOLEAN8"
	.skip 1
	.ascii	"DRBOOLEAN8"
	.skip 1
	.ascii	"CRBOOLEAN8"
	.skip 1
	.ascii	"BRBOOLEAN8"
	.skip 1
	.ascii	"ARBOOLEAN8"
	.skip 1
	.ascii	"FRBOOLEAN7"
	.skip 1
	.ascii	"ERBOOLEAN7"
	.skip 1
	.ascii	"DRBOOLEAN7"
	.skip 1
	.ascii	"CRBOOLEAN7"
	.skip 1
	.ascii	"BRBOOLEAN7"
	.skip 1
	.ascii	"ARBOOLEAN7"
	.skip 1
	.ascii	"FRBOOLEAN6"
	.skip 1
	.ascii	"ERBOOLEAN6"
	.skip 1
	.ascii	"DRBOOLEAN6"
	.skip 1
	.ascii	"CRBOOLEAN6"
	.skip 1
	.ascii	"BRBOOLEAN6"
	.skip 1
	.ascii	"ARBOOLEAN6"
	.skip 1
	.ascii	"FRBOOLEAN5"
	.skip 1
	.ascii	"ERBOOLEAN5"
	.skip 1
	.ascii	"DRBOOLEAN5"
	.skip 1
	.ascii	"CRBOOLEAN5"
	.skip 1
	.ascii	"BRBOOLEAN5"
	.skip 1
	.ascii	"ARBOOLEAN5"
	.skip 1
	.ascii	"FRBOOLEAN4"
	.skip 1
	.ascii	"ERBOOLEAN4"
	.skip 1
	.ascii	"DRBOOLEAN4"
	.skip 1
	.ascii	"CRBOOLEAN4"
	.skip 1
	.ascii	"BRBOOLEAN4"
	.skip 1
	.ascii	"ARBOOLEAN4"
	.skip 1
	.ascii	"FRBOOLEAN3"
	.skip 1
	.ascii	"ERBOOLEAN3"
	.skip 1
	.ascii	"DRBOOLEAN3"
	.skip 1
	.ascii	"CRBOOLEAN3"
	.skip 1
	.ascii	"BRBOOLEAN3"
	.skip 1
	.ascii	"ARBOOLEAN3"
	.skip 1
	.ascii	"FRBOOLEAN2"
	.skip 1
	.ascii	"ERBOOLEAN2"
	.skip 1
	.ascii	"DRBOOLEAN2"
	.skip 1
	.ascii	"CRBOOLEAN2"
	.skip 1
	.ascii	"BRBOOLEAN2"
	.skip 1
	.ascii	"ARBOOLEAN2"
	.skip 1
	.ascii	"FRBOOLEAN1"
	.skip 1
	.ascii	"ERBOOLEAN1"
	.skip 1
	.ascii	"DRBOOLEAN1"
	.skip 1
	.ascii	"CRBOOLEAN1"
	.skip 1
	.ascii	"BRBOOLEAN1"
	.skip 1
	.ascii	"ARBOOLEAN1"
	.skip 1
	.ascii	"FRCHAR100"
	.skip 1
	.ascii	"ERCHAR100"
	.skip 1
	.ascii	"DRCHAR100"
	.skip 1
	.ascii	"CRCHAR100"
	.skip 1
	.ascii	"BRCHAR100"
	.skip 1
	.ascii	"ARCHAR100"
	.skip 1
	.ascii	"FRCHAR99"
	.skip 1
	.ascii	"ERCHAR99"
	.skip 1
	.ascii	"DRCHAR99"
	.skip 1
	.ascii	"CRCHAR99"
	.skip 1
	.ascii	"BRCHAR99"
	.skip 1
	.ascii	"ARCHAR99"
	.skip 1
	.ascii	"FRCHAR98"
	.skip 1
	.ascii	"ERCHAR98"
	.skip 1
	.ascii	"DRCHAR98"
	.skip 1
	.ascii	"CRCHAR98"
	.skip 1
	.ascii	"BRCHAR98"
	.skip 1
	.ascii	"ARCHAR98"
	.skip 1
	.ascii	"FRCHAR97"
	.skip 1
	.ascii	"ERCHAR97"
	.skip 1
	.ascii	"DRCHAR97"
	.skip 1
	.ascii	"CRCHAR97"
	.skip 1
	.ascii	"BRCHAR97"
	.skip 1
	.ascii	"ARCHAR97"
	.skip 1
	.ascii	"FRCHAR96"
	.skip 1
	.ascii	"ERCHAR96"
	.skip 1
	.ascii	"DRCHAR96"
	.skip 1
	.ascii	"CRCHAR96"
	.skip 1
	.ascii	"BRCHAR96"
	.skip 1
	.ascii	"ARCHAR96"
	.skip 1
	.ascii	"FRCHAR95"
	.skip 1
	.ascii	"ERCHAR95"
	.skip 1
	.ascii	"DRCHAR95"
	.skip 1
	.ascii	"CRCHAR95"
	.skip 1
	.ascii	"BRCHAR95"
	.skip 1
	.ascii	"ARCHAR95"
	.skip 1
	.ascii	"FRCHAR94"
	.skip 1
	.ascii	"ERCHAR94"
	.skip 1
	.ascii	"DRCHAR94"
	.skip 1
	.ascii	"CRCHAR94"
	.skip 1
	.ascii	"BRCHAR94"
	.skip 1
	.ascii	"ARCHAR94"
	.skip 1
	.ascii	"FRCHAR93"
	.skip 1
	.ascii	"ERCHAR93"
	.skip 1
	.ascii	"DRCHAR93"
	.skip 1
	.ascii	"CRCHAR93"
	.skip 1
	.ascii	"BRCHAR93"
	.skip 1
	.ascii	"ARCHAR93"
	.skip 1
	.ascii	"FRCHAR92"
	.skip 1
	.ascii	"ERCHAR92"
	.skip 1
	.ascii	"DRCHAR92"
	.skip 1
	.ascii	"CRCHAR92"
	.skip 1
	.ascii	"BRCHAR92"
	.skip 1
	.ascii	"ARCHAR92"
	.skip 1
	.ascii	"FRCHAR91"
	.skip 1
	.ascii	"ERCHAR91"
	.skip 1
	.ascii	"DRCHAR91"
	.skip 1
	.ascii	"CRCHAR91"
	.skip 1
	.ascii	"BRCHAR91"
	.skip 1
	.ascii	"ARCHAR91"
	.skip 1
	.ascii	"FRCHAR90"
	.skip 1
	.ascii	"ERCHAR90"
	.skip 1
	.ascii	"DRCHAR90"
	.skip 1
	.ascii	"CRCHAR90"
	.skip 1
	.ascii	"BRCHAR90"
	.skip 1
	.ascii	"ARCHAR90"
	.skip 1
	.ascii	"FRCHAR89"
	.skip 1
	.ascii	"ERCHAR89"
	.skip 1
	.ascii	"DRCHAR89"
	.skip 1
	.ascii	"CRCHAR89"
	.skip 1
	.ascii	"BRCHAR89"
	.skip 1
	.ascii	"ARCHAR89"
	.skip 1
	.ascii	"FRCHAR88"
	.skip 1
	.ascii	"ERCHAR88"
	.skip 1
	.ascii	"DRCHAR88"
	.skip 1
	.ascii	"CRCHAR88"
	.skip 1
	.ascii	"BRCHAR88"
	.skip 1
	.ascii	"ARCHAR88"
	.skip 1
	.ascii	"FRCHAR87"
	.skip 1
	.ascii	"ERCHAR87"
	.skip 1
	.ascii	"DRCHAR87"
	.skip 1
	.ascii	"CRCHAR87"
	.skip 1
	.ascii	"BRCHAR87"
	.skip 1
	.ascii	"ARCHAR87"
	.skip 1
	.ascii	"FRCHAR86"
	.skip 1
	.ascii	"ERCHAR86"
	.skip 1
	.ascii	"DRCHAR86"
	.skip 1
	.ascii	"CRCHAR86"
	.skip 1
	.ascii	"BRCHAR86"
	.skip 1
	.ascii	"ARCHAR86"
	.skip 1
	.ascii	"FRCHAR85"
	.skip 1
	.ascii	"ERCHAR85"
	.skip 1
	.ascii	"DRCHAR85"
	.skip 1
	.ascii	"CRCHAR85"
	.skip 1
	.ascii	"BRCHAR85"
	.skip 1
	.ascii	"ARCHAR85"
	.skip 1
	.ascii	"FRCHAR84"
	.skip 1
	.ascii	"ERCHAR84"
	.skip 1
	.ascii	"DRCHAR84"
	.skip 1
	.ascii	"CRCHAR84"
	.skip 1
	.ascii	"BRCHAR84"
	.skip 1
	.ascii	"ARCHAR84"
	.skip 1
	.ascii	"FRCHAR83"
	.skip 1
	.ascii	"ERCHAR83"
	.skip 1
	.ascii	"DRCHAR83"
	.skip 1
	.ascii	"CRCHAR83"
	.skip 1
	.ascii	"BRCHAR83"
	.skip 1
	.ascii	"ARCHAR83"
	.skip 1
	.ascii	"FRCHAR82"
	.skip 1
	.ascii	"ERCHAR82"
	.skip 1
	.ascii	"DRCHAR82"
	.skip 1
	.ascii	"CRCHAR82"
	.skip 1
	.ascii	"BRCHAR82"
	.skip 1
	.ascii	"ARCHAR82"
	.skip 1
	.ascii	"FRCHAR81"
	.skip 1
	.ascii	"ERCHAR81"
	.skip 1
	.ascii	"DRCHAR81"
	.skip 1
	.ascii	"CRCHAR81"
	.skip 1
	.ascii	"BRCHAR81"
	.skip 1
	.ascii	"ARCHAR81"
	.skip 1
	.ascii	"FRCHAR80"
	.skip 1
	.ascii	"ERCHAR80"
	.skip 1
	.ascii	"DRCHAR80"
	.skip 1
	.ascii	"CRCHAR80"
	.skip 1
	.ascii	"BRCHAR80"
	.skip 1
	.ascii	"ARCHAR80"
	.skip 1
	.ascii	"FRCHAR79"
	.skip 1
	.ascii	"ERCHAR79"
	.skip 1
	.ascii	"DRCHAR79"
	.skip 1
	.ascii	"CRCHAR79"
	.skip 1
	.ascii	"BRCHAR79"
	.skip 1
	.ascii	"ARCHAR79"
	.skip 1
	.ascii	"FRCHAR78"
	.skip 1
	.ascii	"ERCHAR78"
	.skip 1
	.ascii	"DRCHAR78"
	.skip 1
	.ascii	"CRCHAR78"
	.skip 1
	.ascii	"BRCHAR78"
	.skip 1
	.ascii	"ARCHAR78"
	.skip 1
	.ascii	"FRCHAR77"
	.skip 1
	.ascii	"ERCHAR77"
	.skip 1
	.ascii	"DRCHAR77"
	.skip 1
	.ascii	"CRCHAR77"
	.skip 1
	.ascii	"BRCHAR77"
	.skip 1
	.ascii	"ARCHAR77"
	.skip 1
	.ascii	"FRCHAR76"
	.skip 1
	.ascii	"ERCHAR76"
	.skip 1
	.ascii	"DRCHAR76"
	.skip 1
	.ascii	"CRCHAR76"
	.skip 1
	.ascii	"BRCHAR76"
	.skip 1
	.ascii	"ARCHAR76"
	.skip 1
	.ascii	"FRCHAR75"
	.skip 1
	.ascii	"ERCHAR75"
	.skip 1
	.ascii	"DRCHAR75"
	.skip 1
	.ascii	"CRCHAR75"
	.skip 1
	.ascii	"BRCHAR75"
	.skip 1
	.ascii	"ARCHAR75"
	.skip 1
	.ascii	"FRCHAR74"
	.skip 1
	.ascii	"ERCHAR74"
	.skip 1
	.ascii	"DRCHAR74"
	.skip 1
	.ascii	"CRCHAR74"
	.skip 1
	.ascii	"BRCHAR74"
	.skip 1
	.ascii	"ARCHAR74"
	.skip 1
	.ascii	"FRCHAR73"
	.skip 1
	.ascii	"ERCHAR73"
	.skip 1
	.ascii	"DRCHAR73"
	.skip 1
	.ascii	"CRCHAR73"
	.skip 1
	.ascii	"BRCHAR73"
	.skip 1
	.ascii	"ARCHAR73"
	.skip 1
	.ascii	"FRCHAR72"
	.skip 1
	.ascii	"ERCHAR72"
	.skip 1
	.ascii	"DRCHAR72"
	.skip 1
	.ascii	"CRCHAR72"
	.skip 1
	.ascii	"BRCHAR72"
	.skip 1
	.ascii	"ARCHAR72"
	.skip 1
	.ascii	"FRCHAR71"
	.skip 1
	.ascii	"ERCHAR71"
	.skip 1
	.ascii	"DRCHAR71"
	.skip 1
	.ascii	"CRCHAR71"
	.skip 1
	.ascii	"BRCHAR71"
	.skip 1
	.ascii	"ARCHAR71"
	.skip 1
	.ascii	"FRCHAR70"
	.skip 1
	.ascii	"ERCHAR70"
	.skip 1
	.ascii	"DRCHAR70"
	.skip 1
	.ascii	"CRCHAR70"
	.skip 1
	.ascii	"BRCHAR70"
	.skip 1
	.ascii	"ARCHAR70"
	.skip 1
	.ascii	"FRCHAR69"
	.skip 1
	.ascii	"ERCHAR69"
	.skip 1
	.ascii	"DRCHAR69"
	.skip 1
	.ascii	"CRCHAR69"
	.skip 1
	.ascii	"BRCHAR69"
	.skip 1
	.ascii	"ARCHAR69"
	.skip 1
	.ascii	"FRCHAR68"
	.skip 1
	.ascii	"ERCHAR68"
	.skip 1
	.ascii	"DRCHAR68"
	.skip 1
	.ascii	"CRCHAR68"
	.skip 1
	.ascii	"BRCHAR68"
	.skip 1
	.ascii	"ARCHAR68"
	.skip 1
	.ascii	"FRCHAR67"
	.skip 1
	.ascii	"ERCHAR67"
	.skip 1
	.ascii	"DRCHAR67"
	.skip 1
	.ascii	"CRCHAR67"
	.skip 1
	.ascii	"BRCHAR67"
	.skip 1
	.ascii	"ARCHAR67"
	.skip 1
	.ascii	"FRCHAR66"
	.skip 1
	.ascii	"ERCHAR66"
	.skip 1
	.ascii	"DRCHAR66"
	.skip 1
	.ascii	"CRCHAR66"
	.skip 1
	.ascii	"BRCHAR66"
	.skip 1
	.ascii	"ARCHAR66"
	.skip 1
	.ascii	"FRCHAR65"
	.skip 1
	.ascii	"ERCHAR65"
	.skip 1
	.ascii	"DRCHAR65"
	.skip 1
	.ascii	"CRCHAR65"
	.skip 1
	.ascii	"BRCHAR65"
	.skip 1
	.ascii	"ARCHAR65"
	.skip 1
	.ascii	"FRCHAR64"
	.skip 1
	.ascii	"ERCHAR64"
	.skip 1
	.ascii	"DRCHAR64"
	.skip 1
	.ascii	"CRCHAR64"
	.skip 1
	.ascii	"BRCHAR64"
	.skip 1
	.ascii	"ARCHAR64"
	.skip 1
	.ascii	"FRCHAR63"
	.skip 1
	.ascii	"ERCHAR63"
	.skip 1
	.ascii	"DRCHAR63"
	.skip 1
	.ascii	"CRCHAR63"
	.skip 1
	.ascii	"BRCHAR63"
	.skip 1
	.ascii	"ARCHAR63"
	.skip 1
	.ascii	"FRCHAR62"
	.skip 1
	.ascii	"ERCHAR62"
	.skip 1
	.ascii	"DRCHAR62"
	.skip 1
	.ascii	"CRCHAR62"
	.skip 1
	.ascii	"BRCHAR62"
	.skip 1
	.ascii	"ARCHAR62"
	.skip 1
	.ascii	"FRCHAR61"
	.skip 1
	.ascii	"ERCHAR61"
	.skip 1
	.ascii	"DRCHAR61"
	.skip 1
	.ascii	"CRCHAR61"
	.skip 1
	.ascii	"BRCHAR61"
	.skip 1
	.ascii	"ARCHAR61"
	.skip 1
	.ascii	"FRCHAR60"
	.skip 1
	.ascii	"ERCHAR60"
	.skip 1
	.ascii	"DRCHAR60"
	.skip 1
	.ascii	"CRCHAR60"
	.skip 1
	.ascii	"BRCHAR60"
	.skip 1
	.ascii	"ARCHAR60"
	.skip 1
	.ascii	"FRCHAR59"
	.skip 1
	.ascii	"ERCHAR59"
	.skip 1
	.ascii	"DRCHAR59"
	.skip 1
	.ascii	"CRCHAR59"
	.skip 1
	.ascii	"BRCHAR59"
	.skip 1
	.ascii	"ARCHAR59"
	.skip 1
	.ascii	"FRCHAR58"
	.skip 1
	.ascii	"ERCHAR58"
	.skip 1
	.ascii	"DRCHAR58"
	.skip 1
	.ascii	"CRCHAR58"
	.skip 1
	.ascii	"BRCHAR58"
	.skip 1
	.ascii	"ARCHAR58"
	.skip 1
	.ascii	"FRCHAR57"
	.skip 1
	.ascii	"ERCHAR57"
	.skip 1
	.ascii	"DRCHAR57"
	.skip 1
	.ascii	"CRCHAR57"
	.skip 1
	.ascii	"BRCHAR57"
	.skip 1
	.ascii	"ARCHAR57"
	.skip 1
	.ascii	"FRCHAR56"
	.skip 1
	.ascii	"ERCHAR56"
	.skip 1
	.ascii	"DRCHAR56"
	.skip 1
	.ascii	"CRCHAR56"
	.skip 1
	.ascii	"BRCHAR56"
	.skip 1
	.ascii	"ARCHAR56"
	.skip 1
	.ascii	"FRCHAR55"
	.skip 1
	.ascii	"ERCHAR55"
	.skip 1
	.ascii	"DRCHAR55"
	.skip 1
	.ascii	"CRCHAR55"
	.skip 1
	.ascii	"BRCHAR55"
	.skip 1
	.ascii	"ARCHAR55"
	.skip 1
	.ascii	"FRCHAR54"
	.skip 1
	.ascii	"ERCHAR54"
	.skip 1
	.ascii	"DRCHAR54"
	.skip 1
	.ascii	"CRCHAR54"
	.skip 1
	.ascii	"BRCHAR54"
	.skip 1
	.ascii	"ARCHAR54"
	.skip 1
	.ascii	"FRCHAR53"
	.skip 1
	.ascii	"ERCHAR53"
	.skip 1
	.ascii	"DRCHAR53"
	.skip 1
	.ascii	"CRCHAR53"
	.skip 1
	.ascii	"BRCHAR53"
	.skip 1
	.ascii	"ARCHAR53"
	.skip 1
	.ascii	"FRCHAR52"
	.skip 1
	.ascii	"ERCHAR52"
	.skip 1
	.ascii	"DRCHAR52"
	.skip 1
	.ascii	"CRCHAR52"
	.skip 1
	.ascii	"BRCHAR52"
	.skip 1
	.ascii	"ARCHAR52"
	.skip 1
	.ascii	"FRCHAR51"
	.skip 1
	.ascii	"ERCHAR51"
	.skip 1
	.ascii	"DRCHAR51"
	.skip 1
	.ascii	"CRCHAR51"
	.skip 1
	.ascii	"BRCHAR51"
	.skip 1
	.ascii	"ARCHAR51"
	.skip 1
	.ascii	"FRCHAR50"
	.skip 1
	.ascii	"ERCHAR50"
	.skip 1
	.ascii	"DRCHAR50"
	.skip 1
	.ascii	"CRCHAR50"
	.skip 1
	.ascii	"BRCHAR50"
	.skip 1
	.ascii	"ARCHAR50"
	.skip 1
	.ascii	"FRCHAR49"
	.skip 1
	.ascii	"ERCHAR49"
	.skip 1
	.ascii	"DRCHAR49"
	.skip 1
	.ascii	"CRCHAR49"
	.skip 1
	.ascii	"BRCHAR49"
	.skip 1
	.ascii	"ARCHAR49"
	.skip 1
	.ascii	"FRCHAR48"
	.skip 1
	.ascii	"ERCHAR48"
	.skip 1
	.ascii	"DRCHAR48"
	.skip 1
	.ascii	"CRCHAR48"
	.skip 1
	.ascii	"BRCHAR48"
	.skip 1
	.ascii	"ARCHAR48"
	.skip 1
	.ascii	"FRCHAR47"
	.skip 1
	.ascii	"ERCHAR47"
	.skip 1
	.ascii	"DRCHAR47"
	.skip 1
	.ascii	"CRCHAR47"
	.skip 1
	.ascii	"BRCHAR47"
	.skip 1
	.ascii	"ARCHAR47"
	.skip 1
	.ascii	"FRCHAR46"
	.skip 1
	.ascii	"ERCHAR46"
	.skip 1
	.ascii	"DRCHAR46"
	.skip 1
	.ascii	"CRCHAR46"
	.skip 1
	.ascii	"BRCHAR46"
	.skip 1
	.ascii	"ARCHAR46"
	.skip 1
	.ascii	"FRCHAR45"
	.skip 1
	.ascii	"ERCHAR45"
	.skip 1
	.ascii	"DRCHAR45"
	.skip 1
	.ascii	"CRCHAR45"
	.skip 1
	.ascii	"BRCHAR45"
	.skip 1
	.ascii	"ARCHAR45"
	.skip 1
	.ascii	"FRCHAR44"
	.skip 1
	.ascii	"ERCHAR44"
	.skip 1
	.ascii	"DRCHAR44"
	.skip 1
	.ascii	"CRCHAR44"
	.skip 1
	.ascii	"BRCHAR44"
	.skip 1
	.ascii	"ARCHAR44"
	.skip 1
	.ascii	"FRCHAR43"
	.skip 1
	.ascii	"ERCHAR43"
	.skip 1
	.ascii	"DRCHAR43"
	.skip 1
	.ascii	"CRCHAR43"
	.skip 1
	.ascii	"BRCHAR43"
	.skip 1
	.ascii	"ARCHAR43"
	.skip 1
	.ascii	"FRCHAR42"
	.skip 1
	.ascii	"ERCHAR42"
	.skip 1
	.ascii	"DRCHAR42"
	.skip 1
	.ascii	"CRCHAR42"
	.skip 1
	.ascii	"BRCHAR42"
	.skip 1
	.ascii	"ARCHAR42"
	.skip 1
	.ascii	"FRCHAR41"
	.skip 1
	.ascii	"ERCHAR41"
	.skip 1
	.ascii	"DRCHAR41"
	.skip 1
	.ascii	"CRCHAR41"
	.skip 1
	.ascii	"BRCHAR41"
	.skip 1
	.ascii	"ARCHAR41"
	.skip 1
	.ascii	"FRCHAR40"
	.skip 1
	.ascii	"ERCHAR40"
	.skip 1
	.ascii	"DRCHAR40"
	.skip 1
	.ascii	"CRCHAR40"
	.skip 1
	.ascii	"BRCHAR40"
	.skip 1
	.ascii	"ARCHAR40"
	.skip 1
	.ascii	"FRCHAR39"
	.skip 1
	.ascii	"ERCHAR39"
	.skip 1
	.ascii	"DRCHAR39"
	.skip 1
	.ascii	"CRCHAR39"
	.skip 1
	.ascii	"BRCHAR39"
	.skip 1
	.ascii	"ARCHAR39"
	.skip 1
	.ascii	"FRCHAR38"
	.skip 1
	.ascii	"ERCHAR38"
	.skip 1
	.ascii	"DRCHAR38"
	.skip 1
	.ascii	"CRCHAR38"
	.skip 1
	.ascii	"BRCHAR38"
	.skip 1
	.ascii	"ARCHAR38"
	.skip 1
	.ascii	"FRCHAR37"
	.skip 1
	.ascii	"ERCHAR37"
	.skip 1
	.ascii	"DRCHAR37"
	.skip 1
	.ascii	"CRCHAR37"
	.skip 1
	.ascii	"BRCHAR37"
	.skip 1
	.ascii	"ARCHAR37"
	.skip 1
	.ascii	"FRCHAR36"
	.skip 1
	.ascii	"ERCHAR36"
	.skip 1
	.ascii	"DRCHAR36"
	.skip 1
	.ascii	"CRCHAR36"
	.skip 1
	.ascii	"BRCHAR36"
	.skip 1
	.ascii	"ARCHAR36"
	.skip 1
	.ascii	"FRCHAR35"
	.skip 1
	.ascii	"ERCHAR35"
	.skip 1
	.ascii	"DRCHAR35"
	.skip 1
	.ascii	"CRCHAR35"
	.skip 1
	.ascii	"BRCHAR35"
	.skip 1
	.ascii	"ARCHAR35"
	.skip 1
	.ascii	"FRCHAR34"
	.skip 1
	.ascii	"ERCHAR34"
	.skip 1
	.ascii	"DRCHAR34"
	.skip 1
	.ascii	"CRCHAR34"
	.skip 1
	.ascii	"BRCHAR34"
	.skip 1
	.ascii	"ARCHAR34"
	.skip 1
	.ascii	"FRCHAR33"
	.skip 1
	.ascii	"ERCHAR33"
	.skip 1
	.ascii	"DRCHAR33"
	.skip 1
	.ascii	"CRCHAR33"
	.skip 1
	.ascii	"BRCHAR33"
	.skip 1
	.ascii	"ARCHAR33"
	.skip 1
	.ascii	"FRCHAR32"
	.skip 1
	.ascii	"ERCHAR32"
	.skip 1
	.ascii	"DRCHAR32"
	.skip 1
	.ascii	"CRCHAR32"
	.skip 1
	.ascii	"BRCHAR32"
	.skip 1
	.ascii	"ARCHAR32"
	.skip 1
	.ascii	"FRCHAR31"
	.skip 1
	.ascii	"ERCHAR31"
	.skip 1
	.ascii	"DRCHAR31"
	.skip 1
	.ascii	"CRCHAR31"
	.skip 1
	.ascii	"BRCHAR31"
	.skip 1
	.ascii	"ARCHAR31"
	.skip 1
	.ascii	"FRCHAR30"
	.skip 1
	.ascii	"ERCHAR30"
	.skip 1
	.ascii	"DRCHAR30"
	.skip 1
	.ascii	"CRCHAR30"
	.skip 1
	.ascii	"BRCHAR30"
	.skip 1
	.ascii	"ARCHAR30"
	.skip 1
	.ascii	"FRCHAR29"
	.skip 1
	.ascii	"ERCHAR29"
	.skip 1
	.ascii	"DRCHAR29"
	.skip 1
	.ascii	"CRCHAR29"
	.skip 1
	.ascii	"BRCHAR29"
	.skip 1
	.ascii	"ARCHAR29"
	.skip 1
	.ascii	"FRCHAR28"
	.skip 1
	.ascii	"ERCHAR28"
	.skip 1
	.ascii	"DRCHAR28"
	.skip 1
	.ascii	"CRCHAR28"
	.skip 1
	.ascii	"BRCHAR28"
	.skip 1
	.ascii	"ARCHAR28"
	.skip 1
	.ascii	"FRCHAR27"
	.skip 1
	.ascii	"ERCHAR27"
	.skip 1
	.ascii	"DRCHAR27"
	.skip 1
	.ascii	"CRCHAR27"
	.skip 1
	.ascii	"BRCHAR27"
	.skip 1
	.ascii	"ARCHAR27"
	.skip 1
	.ascii	"FRCHAR26"
	.skip 1
	.ascii	"ERCHAR26"
	.skip 1
	.ascii	"DRCHAR26"
	.skip 1
	.ascii	"CRCHAR26"
	.skip 1
	.ascii	"BRCHAR26"
	.skip 1
	.ascii	"ARCHAR26"
	.skip 1
	.ascii	"FRCHAR25"
	.skip 1
	.ascii	"ERCHAR25"
	.skip 1
	.ascii	"DRCHAR25"
	.skip 1
	.ascii	"CRCHAR25"
	.skip 1
	.ascii	"BRCHAR25"
	.skip 1
	.ascii	"ARCHAR25"
	.skip 1
	.ascii	"FRCHAR24"
	.skip 1
	.ascii	"ERCHAR24"
	.skip 1
	.ascii	"DRCHAR24"
	.skip 1
	.ascii	"CRCHAR24"
	.skip 1
	.ascii	"BRCHAR24"
	.skip 1
	.ascii	"ARCHAR24"
	.skip 1
	.ascii	"FRCHAR23"
	.skip 1
	.ascii	"ERCHAR23"
	.skip 1
	.ascii	"DRCHAR23"
	.skip 1
	.ascii	"CRCHAR23"
	.skip 1
	.ascii	"BRCHAR23"
	.skip 1
	.ascii	"ARCHAR23"
	.skip 1
	.ascii	"FRCHAR22"
	.skip 1
	.ascii	"ERCHAR22"
	.skip 1
	.ascii	"DRCHAR22"
	.skip 1
	.ascii	"CRCHAR22"
	.skip 1
	.ascii	"BRCHAR22"
	.skip 1
	.ascii	"ARCHAR22"
	.skip 1
	.ascii	"FRCHAR21"
	.skip 1
	.ascii	"ERCHAR21"
	.skip 1
	.ascii	"DRCHAR21"
	.skip 1
	.ascii	"CRCHAR21"
	.skip 1
	.ascii	"BRCHAR21"
	.skip 1
	.ascii	"ARCHAR21"
	.skip 1
	.ascii	"FRCHAR20"
	.skip 1
	.ascii	"ERCHAR20"
	.skip 1
	.ascii	"DRCHAR20"
	.skip 1
	.ascii	"CRCHAR20"
	.skip 1
	.ascii	"BRCHAR20"
	.skip 1
	.ascii	"ARCHAR20"
	.skip 1
	.ascii	"FRCHAR19"
	.skip 1
	.ascii	"ERCHAR19"
	.skip 1
	.ascii	"DRCHAR19"
	.skip 1
	.ascii	"CRCHAR19"
	.skip 1
	.ascii	"BRCHAR19"
	.skip 1
	.ascii	"ARCHAR19"
	.skip 1
	.ascii	"FRCHAR18"
	.skip 1
	.ascii	"ERCHAR18"
	.skip 1
	.ascii	"DRCHAR18"
	.skip 1
	.ascii	"CRCHAR18"
	.skip 1
	.ascii	"BRCHAR18"
	.skip 1
	.ascii	"ARCHAR18"
	.skip 1
	.ascii	"FRCHAR17"
	.skip 1
	.ascii	"ERCHAR17"
	.skip 1
	.ascii	"DRCHAR17"
	.skip 1
	.ascii	"CRCHAR17"
	.skip 1
	.ascii	"BRCHAR17"
	.skip 1
	.ascii	"ARCHAR17"
	.skip 1
	.ascii	"FRCHAR16"
	.skip 1
	.ascii	"ERCHAR16"
	.skip 1
	.ascii	"DRCHAR16"
	.skip 1
	.ascii	"CRCHAR16"
	.skip 1
	.ascii	"BRCHAR16"
	.skip 1
	.ascii	"ARCHAR16"
	.skip 1
	.ascii	"FRCHAR15"
	.skip 1
	.ascii	"ERCHAR15"
	.skip 1
	.ascii	"DRCHAR15"
	.skip 1
	.ascii	"CRCHAR15"
	.skip 1
	.ascii	"BRCHAR15"
	.skip 1
	.ascii	"ARCHAR15"
	.skip 1
	.ascii	"FRCHAR14"
	.skip 1
	.ascii	"ERCHAR14"
	.skip 1
	.ascii	"DRCHAR14"
	.skip 1
	.ascii	"CRCHAR14"
	.skip 1
	.ascii	"BRCHAR14"
	.skip 1
	.ascii	"ARCHAR14"
	.skip 1
	.ascii	"FRCHAR13"
	.skip 1
	.ascii	"ERCHAR13"
	.skip 1
	.ascii	"DRCHAR13"
	.skip 1
	.ascii	"CRCHAR13"
	.skip 1
	.ascii	"BRCHAR13"
	.skip 1
	.ascii	"ARCHAR13"
	.skip 1
	.ascii	"FRCHAR12"
	.skip 1
	.ascii	"ERCHAR12"
	.skip 1
	.ascii	"DRCHAR12"
	.skip 1
	.ascii	"CRCHAR12"
	.skip 1
	.ascii	"BRCHAR12"
	.skip 1
	.ascii	"ARCHAR12"
	.skip 1
	.ascii	"FRCHAR11"
	.skip 1
	.ascii	"ERCHAR11"
	.skip 1
	.ascii	"DRCHAR11"
	.skip 1
	.ascii	"CRCHAR11"
	.skip 1
	.ascii	"BRCHAR11"
	.skip 1
	.ascii	"ARCHAR11"
	.skip 1
	.ascii	"FRCHAR10"
	.skip 1
	.ascii	"ERCHAR10"
	.skip 1
	.ascii	"DRCHAR10"
	.skip 1
	.ascii	"CRCHAR10"
	.skip 1
	.ascii	"BRCHAR10"
	.skip 1
	.ascii	"ARCHAR10"
	.skip 1
	.ascii	"FRCHAR9"
	.skip 1
	.ascii	"ERCHAR9"
	.skip 1
	.ascii	"DRCHAR9"
	.skip 1
	.ascii	"CRCHAR9"
	.skip 1
	.ascii	"BRCHAR9"
	.skip 1
	.ascii	"ARCHAR9"
	.skip 1
	.ascii	"FRCHAR8"
	.skip 1
	.ascii	"ERCHAR8"
	.skip 1
	.ascii	"DRCHAR8"
	.skip 1
	.ascii	"CRCHAR8"
	.skip 1
	.ascii	"BRCHAR8"
	.skip 1
	.ascii	"ARCHAR8"
	.skip 1
	.ascii	"FRCHAR7"
	.skip 1
	.ascii	"ERCHAR7"
	.skip 1
	.ascii	"DRCHAR7"
	.skip 1
	.ascii	"CRCHAR7"
	.skip 1
	.ascii	"BRCHAR7"
	.skip 1
	.ascii	"ARCHAR7"
	.skip 1
	.ascii	"FRCHAR6"
	.skip 1
	.ascii	"ERCHAR6"
	.skip 1
	.ascii	"DRCHAR6"
	.skip 1
	.ascii	"CRCHAR6"
	.skip 1
	.ascii	"BRCHAR6"
	.skip 1
	.ascii	"ARCHAR6"
	.skip 1
	.ascii	"FRCHAR5"
	.skip 1
	.ascii	"ERCHAR5"
	.skip 1
	.ascii	"DRCHAR5"
	.skip 1
	.ascii	"CRCHAR5"
	.skip 1
	.ascii	"BRCHAR5"
	.skip 1
	.ascii	"ARCHAR5"
	.skip 1
	.ascii	"FRCHAR4"
	.skip 1
	.ascii	"ERCHAR4"
	.skip 1
	.ascii	"DRCHAR4"
	.skip 1
	.ascii	"CRCHAR4"
	.skip 1
	.ascii	"BRCHAR4"
	.skip 1
	.ascii	"ARCHAR4"
	.skip 1
	.ascii	"FRCHAR3"
	.skip 1
	.ascii	"ERCHAR3"
	.skip 1
	.ascii	"DRCHAR3"
	.skip 1
	.ascii	"CRCHAR3"
	.skip 1
	.ascii	"BRCHAR3"
	.skip 1
	.ascii	"ARCHAR3"
	.skip 1
	.ascii	"FRCHAR2"
	.skip 1
	.ascii	"ERCHAR2"
	.skip 1
	.ascii	"DRCHAR2"
	.skip 1
	.ascii	"CRCHAR2"
	.skip 1
	.ascii	"BRCHAR2"
	.skip 1
	.ascii	"ARCHAR2"
	.skip 1
	.ascii	"FRCHAR1"
	.skip 1
	.ascii	"ERCHAR1"
	.skip 1
	.ascii	"DRCHAR1"
	.skip 1
	.ascii	"CRCHAR1"
	.skip 1
	.ascii	"BRCHAR1"
	.skip 1
	.ascii	"ARCHAR1"
	.skip 2
	.long	records_M3
	.long	L_1
	.long	records__FRFLOAT6410
	.long	L_1+11
	.long	records__ERFLOAT6410
	.long	L_1+23
	.long	records__DRFLOAT6410
	.long	L_1+35
	.long	records__CRFLOAT6410
	.long	L_1+47
	.long	records__BRFLOAT6410
	.long	L_1+59
	.long	records__ARFLOAT6410
	.long	L_1+71
	.long	records__FRFLOAT649
	.long	L_1+83
	.long	records__ERFLOAT649
	.long	L_1+94
	.long	records__DRFLOAT649
	.long	L_1+105
	.long	records__CRFLOAT649
	.long	L_1+116
	.long	records__BRFLOAT649
	.long	L_1+127
	.long	records__ARFLOAT649
	.long	L_1+138
	.long	records__FRFLOAT648
	.long	L_1+149
	.long	records__ERFLOAT648
	.long	L_1+160
	.long	records__DRFLOAT648
	.long	L_1+171
	.long	records__CRFLOAT648
	.long	L_1+182
	.long	records__BRFLOAT648
	.long	L_1+193
	.long	records__ARFLOAT648
	.long	L_1+204
	.long	records__FRFLOAT647
	.long	L_1+215
	.long	records__ERFLOAT647
	.long	L_1+226
	.long	records__DRFLOAT647
	.long	L_1+237
	.long	records__CRFLOAT647
	.long	L_1+248
	.long	records__BRFLOAT647
	.long	L_1+259
	.long	records__ARFLOAT647
	.long	L_1+270
	.long	records__FRFLOAT646
	.long	L_1+281
	.long	records__ERFLOAT646
	.long	L_1+292
	.long	records__DRFLOAT646
	.long	L_1+303
	.long	records__CRFLOAT646
	.long	L_1+314
	.long	records__BRFLOAT646
	.long	L_1+325
	.long	records__ARFLOAT646
	.long	L_1+336
	.long	records__FRFLOAT645
	.long	L_1+347
	.long	records__ERFLOAT645
	.long	L_1+358
	.long	records__DRFLOAT645
	.long	L_1+369
	.long	records__CRFLOAT645
	.long	L_1+380
	.long	records__BRFLOAT645
	.long	L_1+391
	.long	records__ARFLOAT645
	.long	L_1+402
	.long	records__FRFLOAT644
	.long	L_1+413
	.long	records__ERFLOAT644
	.long	L_1+424
	.long	records__DRFLOAT644
	.long	L_1+435
	.long	records__CRFLOAT644
	.long	L_1+446
	.long	records__BRFLOAT644
	.long	L_1+457
	.long	records__ARFLOAT644
	.long	L_1+468
	.long	records__FRFLOAT643
	.long	L_1+479
	.long	records__ERFLOAT643
	.long	L_1+490
	.long	records__DRFLOAT643
	.long	L_1+501
	.long	records__CRFLOAT643
	.long	L_1+512
	.long	records__BRFLOAT643
	.long	L_1+523
	.long	records__ARFLOAT643
	.long	L_1+534
	.long	records__FRFLOAT642
	.long	L_1+545
	.long	records__ERFLOAT642
	.long	L_1+556
	.long	records__DRFLOAT642
	.long	L_1+567
	.long	records__CRFLOAT642
	.long	L_1+578
	.long	records__BRFLOAT642
	.long	L_1+589
	.long	records__ARFLOAT642
	.long	L_1+600
	.long	records__FRFLOAT641
	.long	L_1+611
	.long	records__ERFLOAT641
	.long	L_1+622
	.long	records__DRFLOAT641
	.long	L_1+633
	.long	records__CRFLOAT641
	.long	L_1+644
	.long	records__BRFLOAT641
	.long	L_1+655
	.long	records__ARFLOAT641
	.long	L_1+666
	.long	records__FRFLOAT3210
	.long	L_1+677
	.long	records__ERFLOAT3210
	.long	L_1+689
	.long	records__DRFLOAT3210
	.long	L_1+701
	.long	records__CRFLOAT3210
	.long	L_1+713
	.long	records__BRFLOAT3210
	.long	L_1+725
	.long	records__ARFLOAT3210
	.long	L_1+737
	.long	records__FRFLOAT329
	.long	L_1+749
	.long	records__ERFLOAT329
	.long	L_1+760
	.long	records__DRFLOAT329
	.long	L_1+771
	.long	records__CRFLOAT329
	.long	L_1+782
	.long	records__BRFLOAT329
	.long	L_1+793
	.long	records__ARFLOAT329
	.long	L_1+804
	.long	records__FRFLOAT328
	.long	L_1+815
	.long	records__ERFLOAT328
	.long	L_1+826
	.long	records__DRFLOAT328
	.long	L_1+837
	.long	records__CRFLOAT328
	.long	L_1+848
	.long	records__BRFLOAT328
	.long	L_1+859
	.long	records__ARFLOAT328
	.long	L_1+870
	.long	records__FRFLOAT327
	.long	L_1+881
	.long	records__ERFLOAT327
	.long	L_1+892
	.long	records__DRFLOAT327
	.long	L_1+903
	.long	records__CRFLOAT327
	.long	L_1+914
	.long	records__BRFLOAT327
	.long	L_1+925
	.long	records__ARFLOAT327
	.long	L_1+936
	.long	records__FRFLOAT326
	.long	L_1+947
	.long	records__ERFLOAT326
	.long	L_1+958
	.long	records__DRFLOAT326
	.long	L_1+969
	.long	records__CRFLOAT326
	.long	L_1+980
	.long	records__BRFLOAT326
	.long	L_1+991
	.long	records__ARFLOAT326
	.long	L_1+1002
	.long	records__FRFLOAT325
	.long	L_1+1013
	.long	records__ERFLOAT325
	.long	L_1+1024
	.long	records__DRFLOAT325
	.long	L_1+1035
	.long	records__CRFLOAT325
	.long	L_1+1046
	.long	records__BRFLOAT325
	.long	L_1+1057
	.long	records__ARFLOAT325
	.long	L_1+1068
	.long	records__FRFLOAT324
	.long	L_1+1079
	.long	records__ERFLOAT324
	.long	L_1+1090
	.long	records__DRFLOAT324
	.long	L_1+1101
	.long	records__CRFLOAT324
	.long	L_1+1112
	.long	records__BRFLOAT324
	.long	L_1+1123
	.long	records__ARFLOAT324
	.long	L_1+1134
	.long	records__FRFLOAT323
	.long	L_1+1145
	.long	records__ERFLOAT323
	.long	L_1+1156
	.long	records__DRFLOAT323
	.long	L_1+1167
	.long	records__CRFLOAT323
	.long	L_1+1178
	.long	records__BRFLOAT323
	.long	L_1+1189
	.long	records__ARFLOAT323
	.long	L_1+1200
	.long	records__FRFLOAT322
	.long	L_1+1211
	.long	records__ERFLOAT322
	.long	L_1+1222
	.long	records__DRFLOAT322
	.long	L_1+1233
	.long	records__CRFLOAT322
	.long	L_1+1244
	.long	records__BRFLOAT322
	.long	L_1+1255
	.long	records__ARFLOAT322
	.long	L_1+1266
	.long	records__FRFLOAT321
	.long	L_1+1277
	.long	records__ERFLOAT321
	.long	L_1+1288
	.long	records__DRFLOAT321
	.long	L_1+1299
	.long	records__CRFLOAT321
	.long	L_1+1310
	.long	records__BRFLOAT321
	.long	L_1+1321
	.long	records__ARFLOAT321
	.long	L_1+1332
	.long	records__FRLONGCARD10
	.long	L_1+1343
	.long	records__ERLONGCARD10
	.long	L_1+1356
	.long	records__DRLONGCARD10
	.long	L_1+1369
	.long	records__CRLONGCARD10
	.long	L_1+1382
	.long	records__BRLONGCARD10
	.long	L_1+1395
	.long	records__ARLONGCARD10
	.long	L_1+1408
	.long	records__FRLONGCARD9
	.long	L_1+1421
	.long	records__ERLONGCARD9
	.long	L_1+1433
	.long	records__DRLONGCARD9
	.long	L_1+1445
	.long	records__CRLONGCARD9
	.long	L_1+1457
	.long	records__BRLONGCARD9
	.long	L_1+1469
	.long	records__ARLONGCARD9
	.long	L_1+1481
	.long	records__FRLONGCARD8
	.long	L_1+1493
	.long	records__ERLONGCARD8
	.long	L_1+1505
	.long	records__DRLONGCARD8
	.long	L_1+1517
	.long	records__CRLONGCARD8
	.long	L_1+1529
	.long	records__BRLONGCARD8
	.long	L_1+1541
	.long	records__ARLONGCARD8
	.long	L_1+1553
	.long	records__FRLONGCARD7
	.long	L_1+1565
	.long	records__ERLONGCARD7
	.long	L_1+1577
	.long	records__DRLONGCARD7
	.long	L_1+1589
	.long	records__CRLONGCARD7
	.long	L_1+1601
	.long	records__BRLONGCARD7
	.long	L_1+1613
	.long	records__ARLONGCARD7
	.long	L_1+1625
	.long	records__FRLONGCARD6
	.long	L_1+1637
	.long	records__ERLONGCARD6
	.long	L_1+1649
	.long	records__DRLONGCARD6
	.long	L_1+1661
	.long	records__CRLONGCARD6
	.long	L_1+1673
	.long	records__BRLONGCARD6
	.long	L_1+1685
	.long	records__ARLONGCARD6
	.long	L_1+1697
	.long	records__FRLONGCARD5
	.long	L_1+1709
	.long	records__ERLONGCARD5
	.long	L_1+1721
	.long	records__DRLONGCARD5
	.long	L_1+1733
	.long	records__CRLONGCARD5
	.long	L_1+1745
	.long	records__BRLONGCARD5
	.long	L_1+1757
	.long	records__ARLONGCARD5
	.long	L_1+1769
	.long	records__FRLONGCARD4
	.long	L_1+1781
	.long	records__ERLONGCARD4
	.long	L_1+1793
	.long	records__DRLONGCARD4
	.long	L_1+1805
	.long	records__CRLONGCARD4
	.long	L_1+1817
	.long	records__BRLONGCARD4
	.long	L_1+1829
	.long	records__ARLONGCARD4
	.long	L_1+1841
	.long	records__FRLONGCARD3
	.long	L_1+1853
	.long	records__ERLONGCARD3
	.long	L_1+1865
	.long	records__DRLONGCARD3
	.long	L_1+1877
	.long	records__CRLONGCARD3
	.long	L_1+1889
	.long	records__BRLONGCARD3
	.long	L_1+1901
	.long	records__ARLONGCARD3
	.long	L_1+1913
	.long	records__FRLONGCARD2
	.long	L_1+1925
	.long	records__ERLONGCARD2
	.long	L_1+1937
	.long	records__DRLONGCARD2
	.long	L_1+1949
	.long	records__CRLONGCARD2
	.long	L_1+1961
	.long	records__BRLONGCARD2
	.long	L_1+1973
	.long	records__ARLONGCARD2
	.long	L_1+1985
	.long	records__FRLONGCARD1
	.long	L_1+1997
	.long	records__ERLONGCARD1
	.long	L_1+2009
	.long	records__DRLONGCARD1
	.long	L_1+2021
	.long	records__CRLONGCARD1
	.long	L_1+2033
	.long	records__BRLONGCARD1
	.long	L_1+2045
	.long	records__ARLONGCARD1
	.long	L_1+2057
	.long	records__FRLONGINT10
	.long	L_1+2069
	.long	records__ERLONGINT10
	.long	L_1+2081
	.long	records__DRLONGINT10
	.long	L_1+2093
	.long	records__CRLONGINT10
	.long	L_1+2105
	.long	records__BRLONGINT10
	.long	L_1+2117
	.long	records__ARLONGINT10
	.long	L_1+2129
	.long	records__FRLONGINT9
	.long	L_1+2141
	.long	records__ERLONGINT9
	.long	L_1+2152
	.long	records__DRLONGINT9
	.long	L_1+2163
	.long	records__CRLONGINT9
	.long	L_1+2174
	.long	records__BRLONGINT9
	.long	L_1+2185
	.long	records__ARLONGINT9
	.long	L_1+2196
	.long	records__FRLONGINT8
	.long	L_1+2207
	.long	records__ERLONGINT8
	.long	L_1+2218
	.long	records__DRLONGINT8
	.long	L_1+2229
	.long	records__CRLONGINT8
	.long	L_1+2240
	.long	records__BRLONGINT8
	.long	L_1+2251
	.long	records__ARLONGINT8
	.long	L_1+2262
	.long	records__FRLONGINT7
	.long	L_1+2273
	.long	records__ERLONGINT7
	.long	L_1+2284
	.long	records__DRLONGINT7
	.long	L_1+2295
	.long	records__CRLONGINT7
	.long	L_1+2306
	.long	records__BRLONGINT7
	.long	L_1+2317
	.long	records__ARLONGINT7
	.long	L_1+2328
	.long	records__FRLONGINT6
	.long	L_1+2339
	.long	records__ERLONGINT6
	.long	L_1+2350
	.long	records__DRLONGINT6
	.long	L_1+2361
	.long	records__CRLONGINT6
	.long	L_1+2372
	.long	records__BRLONGINT6
	.long	L_1+2383
	.long	records__ARLONGINT6
	.long	L_1+2394
	.long	records__FRLONGINT5
	.long	L_1+2405
	.long	records__ERLONGINT5
	.long	L_1+2416
	.long	records__DRLONGINT5
	.long	L_1+2427
	.long	records__CRLONGINT5
	.long	L_1+2438
	.long	records__BRLONGINT5
	.long	L_1+2449
	.long	records__ARLONGINT5
	.long	L_1+2460
	.long	records__FRLONGINT4
	.long	L_1+2471
	.long	records__ERLONGINT4
	.long	L_1+2482
	.long	records__DRLONGINT4
	.long	L_1+2493
	.long	records__CRLONGINT4
	.long	L_1+2504
	.long	records__BRLONGINT4
	.long	L_1+2515
	.long	records__ARLONGINT4
	.long	L_1+2526
	.long	records__FRLONGINT3
	.long	L_1+2537
	.long	records__ERLONGINT3
	.long	L_1+2548
	.long	records__DRLONGINT3
	.long	L_1+2559
	.long	records__CRLONGINT3
	.long	L_1+2570
	.long	records__BRLONGINT3
	.long	L_1+2581
	.long	records__ARLONGINT3
	.long	L_1+2592
	.long	records__FRLONGINT2
	.long	L_1+2603
	.long	records__ERLONGINT2
	.long	L_1+2614
	.long	records__DRLONGINT2
	.long	L_1+2625
	.long	records__CRLONGINT2
	.long	L_1+2636
	.long	records__BRLONGINT2
	.long	L_1+2647
	.long	records__ARLONGINT2
	.long	L_1+2658
	.long	records__FRLONGINT1
	.long	L_1+2669
	.long	records__ERLONGINT1
	.long	L_1+2680
	.long	records__DRLONGINT1
	.long	L_1+2691
	.long	records__CRLONGINT1
	.long	L_1+2702
	.long	records__BRLONGINT1
	.long	L_1+2713
	.long	records__ARLONGINT1
	.long	L_1+2724
	.long	records__FRCARDINAL10
	.long	L_1+2735
	.long	records__ERCARDINAL10
	.long	L_1+2748
	.long	records__DRCARDINAL10
	.long	L_1+2761
	.long	records__CRCARDINAL10
	.long	L_1+2774
	.long	records__BRCARDINAL10
	.long	L_1+2787
	.long	records__ARCARDINAL10
	.long	L_1+2800
	.long	records__FRCARDINAL9
	.long	L_1+2813
	.long	records__ERCARDINAL9
	.long	L_1+2825
	.long	records__DRCARDINAL9
	.long	L_1+2837
	.long	records__CRCARDINAL9
	.long	L_1+2849
	.long	records__BRCARDINAL9
	.long	L_1+2861
	.long	records__ARCARDINAL9
	.long	L_1+2873
	.long	records__FRCARDINAL8
	.long	L_1+2885
	.long	records__ERCARDINAL8
	.long	L_1+2897
	.long	records__DRCARDINAL8
	.long	L_1+2909
	.long	records__CRCARDINAL8
	.long	L_1+2921
	.long	records__BRCARDINAL8
	.long	L_1+2933
	.long	records__ARCARDINAL8
	.long	L_1+2945
	.long	records__FRCARDINAL7
	.long	L_1+2957
	.long	records__ERCARDINAL7
	.long	L_1+2969
	.long	records__DRCARDINAL7
	.long	L_1+2981
	.long	records__CRCARDINAL7
	.long	L_1+2993
	.long	records__BRCARDINAL7
	.long	L_1+3005
	.long	records__ARCARDINAL7
	.long	L_1+3017
	.long	records__FRCARDINAL6
	.long	L_1+3029
	.long	records__ERCARDINAL6
	.long	L_1+3041
	.long	records__DRCARDINAL6
	.long	L_1+3053
	.long	records__CRCARDINAL6
	.long	L_1+3065
	.long	records__BRCARDINAL6
	.long	L_1+3077
	.long	records__ARCARDINAL6
	.long	L_1+3089
	.long	records__FRCARDINAL5
	.long	L_1+3101
	.long	records__ERCARDINAL5
	.long	L_1+3113
	.long	records__DRCARDINAL5
	.long	L_1+3125
	.long	records__CRCARDINAL5
	.long	L_1+3137
	.long	records__BRCARDINAL5
	.long	L_1+3149
	.long	records__ARCARDINAL5
	.long	L_1+3161
	.long	records__FRCARDINAL4
	.long	L_1+3173
	.long	records__ERCARDINAL4
	.long	L_1+3185
	.long	records__DRCARDINAL4
	.long	L_1+3197
	.long	records__CRCARDINAL4
	.long	L_1+3209
	.long	records__BRCARDINAL4
	.long	L_1+3221
	.long	records__ARCARDINAL4
	.long	L_1+3233
	.long	records__FRCARDINAL3
	.long	L_1+3245
	.long	records__ERCARDINAL3
	.long	L_1+3257
	.long	records__DRCARDINAL3
	.long	L_1+3269
	.long	records__CRCARDINAL3
	.long	L_1+3281
	.long	records__BRCARDINAL3
	.long	L_1+3293
	.long	records__ARCARDINAL3
	.long	L_1+3305
	.long	records__FRCARDINAL2
	.long	L_1+3317
	.long	records__ERCARDINAL2
	.long	L_1+3329
	.long	records__DRCARDINAL2
	.long	L_1+3341
	.long	records__CRCARDINAL2
	.long	L_1+3353
	.long	records__BRCARDINAL2
	.long	L_1+3365
	.long	records__ARCARDINAL2
	.long	L_1+3377
	.long	records__FRCARDINAL1
	.long	L_1+3389
	.long	records__ERCARDINAL1
	.long	L_1+3401
	.long	records__DRCARDINAL1
	.long	L_1+3413
	.long	records__CRCARDINAL1
	.long	L_1+3425
	.long	records__BRCARDINAL1
	.long	L_1+3437
	.long	records__ARCARDINAL1
	.long	L_1+3449
	.long	records__FRINTEGER10
	.long	L_1+3461
	.long	records__ERINTEGER10
	.long	L_1+3473
	.long	records__DRINTEGER10
	.long	L_1+3485
	.long	records__CRINTEGER10
	.long	L_1+3497
	.long	records__BRINTEGER10
	.long	L_1+3509
	.long	records__ARINTEGER10
	.long	L_1+3521
	.long	records__FRINTEGER9
	.long	L_1+3533
	.long	records__ERINTEGER9
	.long	L_1+3544
	.long	records__DRINTEGER9
	.long	L_1+3555
	.long	records__CRINTEGER9
	.long	L_1+3566
	.long	records__BRINTEGER9
	.long	L_1+3577
	.long	records__ARINTEGER9
	.long	L_1+3588
	.long	records__FRINTEGER8
	.long	L_1+3599
	.long	records__ERINTEGER8
	.long	L_1+3610
	.long	records__DRINTEGER8
	.long	L_1+3621
	.long	records__CRINTEGER8
	.long	L_1+3632
	.long	records__BRINTEGER8
	.long	L_1+3643
	.long	records__ARINTEGER8
	.long	L_1+3654
	.long	records__FRINTEGER7
	.long	L_1+3665
	.long	records__ERINTEGER7
	.long	L_1+3676
	.long	records__DRINTEGER7
	.long	L_1+3687
	.long	records__CRINTEGER7
	.long	L_1+3698
	.long	records__BRINTEGER7
	.long	L_1+3709
	.long	records__ARINTEGER7
	.long	L_1+3720
	.long	records__FRINTEGER6
	.long	L_1+3731
	.long	records__ERINTEGER6
	.long	L_1+3742
	.long	records__DRINTEGER6
	.long	L_1+3753
	.long	records__CRINTEGER6
	.long	L_1+3764
	.long	records__BRINTEGER6
	.long	L_1+3775
	.long	records__ARINTEGER6
	.long	L_1+3786
	.long	records__FRINTEGER5
	.long	L_1+3797
	.long	records__ERINTEGER5
	.long	L_1+3808
	.long	records__DRINTEGER5
	.long	L_1+3819
	.long	records__CRINTEGER5
	.long	L_1+3830
	.long	records__BRINTEGER5
	.long	L_1+3841
	.long	records__ARINTEGER5
	.long	L_1+3852
	.long	records__FRINTEGER4
	.long	L_1+3863
	.long	records__ERINTEGER4
	.long	L_1+3874
	.long	records__DRINTEGER4
	.long	L_1+3885
	.long	records__CRINTEGER4
	.long	L_1+3896
	.long	records__BRINTEGER4
	.long	L_1+3907
	.long	records__ARINTEGER4
	.long	L_1+3918
	.long	records__FRINTEGER3
	.long	L_1+3929
	.long	records__ERINTEGER3
	.long	L_1+3940
	.long	records__DRINTEGER3
	.long	L_1+3951
	.long	records__CRINTEGER3
	.long	L_1+3962
	.long	records__BRINTEGER3
	.long	L_1+3973
	.long	records__ARINTEGER3
	.long	L_1+3984
	.long	records__FRINTEGER2
	.long	L_1+3995
	.long	records__ERINTEGER2
	.long	L_1+4006
	.long	records__DRINTEGER2
	.long	L_1+4017
	.long	records__CRINTEGER2
	.long	L_1+4028
	.long	records__BRINTEGER2
	.long	L_1+4039
	.long	records__ARINTEGER2
	.long	L_1+4050
	.long	records__FRINTEGER1
	.long	L_1+4061
	.long	records__ERINTEGER1
	.long	L_1+4072
	.long	records__DRINTEGER1
	.long	L_1+4083
	.long	records__CRINTEGER1
	.long	L_1+4094
	.long	records__BRINTEGER1
	.long	L_1+4105
	.long	records__ARINTEGER1
	.long	L_1+4116
	.long	records__FRUINT6410
	.long	L_1+4127
	.long	records__ERUINT6410
	.long	L_1+4138
	.long	records__DRUINT6410
	.long	L_1+4149
	.long	records__CRUINT6410
	.long	L_1+4160
	.long	records__BRUINT6410
	.long	L_1+4171
	.long	records__ARUINT6410
	.long	L_1+4182
	.long	records__FRUINT649
	.long	L_1+4193
	.long	records__ERUINT649
	.long	L_1+4203
	.long	records__DRUINT649
	.long	L_1+4213
	.long	records__CRUINT649
	.long	L_1+4223
	.long	records__BRUINT649
	.long	L_1+4233
	.long	records__ARUINT649
	.long	L_1+4243
	.long	records__FRUINT648
	.long	L_1+4253
	.long	records__ERUINT648
	.long	L_1+4263
	.long	records__DRUINT648
	.long	L_1+4273
	.long	records__CRUINT648
	.long	L_1+4283
	.long	records__BRUINT648
	.long	L_1+4293
	.long	records__ARUINT648
	.long	L_1+4303
	.long	records__FRUINT647
	.long	L_1+4313
	.long	records__ERUINT647
	.long	L_1+4323
	.long	records__DRUINT647
	.long	L_1+4333
	.long	records__CRUINT647
	.long	L_1+4343
	.long	records__BRUINT647
	.long	L_1+4353
	.long	records__ARUINT647
	.long	L_1+4363
	.long	records__FRUINT646
	.long	L_1+4373
	.long	records__ERUINT646
	.long	L_1+4383
	.long	records__DRUINT646
	.long	L_1+4393
	.long	records__CRUINT646
	.long	L_1+4403
	.long	records__BRUINT646
	.long	L_1+4413
	.long	records__ARUINT646
	.long	L_1+4423
	.long	records__FRUINT645
	.long	L_1+4433
	.long	records__ERUINT645
	.long	L_1+4443
	.long	records__DRUINT645
	.long	L_1+4453
	.long	records__CRUINT645
	.long	L_1+4463
	.long	records__BRUINT645
	.long	L_1+4473
	.long	records__ARUINT645
	.long	L_1+4483
	.long	records__FRUINT644
	.long	L_1+4493
	.long	records__ERUINT644
	.long	L_1+4503
	.long	records__DRUINT644
	.long	L_1+4513
	.long	records__CRUINT644
	.long	L_1+4523
	.long	records__BRUINT644
	.long	L_1+4533
	.long	records__ARUINT644
	.long	L_1+4543
	.long	records__FRUINT643
	.long	L_1+4553
	.long	records__ERUINT643
	.long	L_1+4563
	.long	records__DRUINT643
	.long	L_1+4573
	.long	records__CRUINT643
	.long	L_1+4583
	.long	records__BRUINT643
	.long	L_1+4593
	.long	records__ARUINT643
	.long	L_1+4603
	.long	records__FRUINT642
	.long	L_1+4613
	.long	records__ERUINT642
	.long	L_1+4623
	.long	records__DRUINT642
	.long	L_1+4633
	.long	records__CRUINT642
	.long	L_1+4643
	.long	records__BRUINT642
	.long	L_1+4653
	.long	records__ARUINT642
	.long	L_1+4663
	.long	records__FRUINT641
	.long	L_1+4673
	.long	records__ERUINT641
	.long	L_1+4683
	.long	records__DRUINT641
	.long	L_1+4693
	.long	records__CRUINT641
	.long	L_1+4703
	.long	records__BRUINT641
	.long	L_1+4713
	.long	records__ARUINT641
	.long	L_1+4723
	.long	records__FRINT6410
	.long	L_1+4733
	.long	records__ERINT6410
	.long	L_1+4743
	.long	records__DRINT6410
	.long	L_1+4753
	.long	records__CRINT6410
	.long	L_1+4763
	.long	records__BRINT6410
	.long	L_1+4773
	.long	records__ARINT6410
	.long	L_1+4783
	.long	records__FRINT649
	.long	L_1+4793
	.long	records__ERINT649
	.long	L_1+4802
	.long	records__DRINT649
	.long	L_1+4811
	.long	records__CRINT649
	.long	L_1+4820
	.long	records__BRINT649
	.long	L_1+4829
	.long	records__ARINT649
	.long	L_1+4838
	.long	records__FRINT648
	.long	L_1+4847
	.long	records__ERINT648
	.long	L_1+4856
	.long	records__DRINT648
	.long	L_1+4865
	.long	records__CRINT648
	.long	L_1+4874
	.long	records__BRINT648
	.long	L_1+4883
	.long	records__ARINT648
	.long	L_1+4892
	.long	records__FRINT647
	.long	L_1+4901
	.long	records__ERINT647
	.long	L_1+4910
	.long	records__DRINT647
	.long	L_1+4919
	.long	records__CRINT647
	.long	L_1+4928
	.long	records__BRINT647
	.long	L_1+4937
	.long	records__ARINT647
	.long	L_1+4946
	.long	records__FRINT646
	.long	L_1+4955
	.long	records__ERINT646
	.long	L_1+4964
	.long	records__DRINT646
	.long	L_1+4973
	.long	records__CRINT646
	.long	L_1+4982
	.long	records__BRINT646
	.long	L_1+4991
	.long	records__ARINT646
	.long	L_1+5000
	.long	records__FRINT645
	.long	L_1+5009
	.long	records__ERINT645
	.long	L_1+5018
	.long	records__DRINT645
	.long	L_1+5027
	.long	records__CRINT645
	.long	L_1+5036
	.long	records__BRINT645
	.long	L_1+5045
	.long	records__ARINT645
	.long	L_1+5054
	.long	records__FRINT644
	.long	L_1+5063
	.long	records__ERINT644
	.long	L_1+5072
	.long	records__DRINT644
	.long	L_1+5081
	.long	records__CRINT644
	.long	L_1+5090
	.long	records__BRINT644
	.long	L_1+5099
	.long	records__ARINT644
	.long	L_1+5108
	.long	records__FRINT643
	.long	L_1+5117
	.long	records__ERINT643
	.long	L_1+5126
	.long	records__DRINT643
	.long	L_1+5135
	.long	records__CRINT643
	.long	L_1+5144
	.long	records__BRINT643
	.long	L_1+5153
	.long	records__ARINT643
	.long	L_1+5162
	.long	records__FRINT642
	.long	L_1+5171
	.long	records__ERINT642
	.long	L_1+5180
	.long	records__DRINT642
	.long	L_1+5189
	.long	records__CRINT642
	.long	L_1+5198
	.long	records__BRINT642
	.long	L_1+5207
	.long	records__ARINT642
	.long	L_1+5216
	.long	records__FRINT641
	.long	L_1+5225
	.long	records__ERINT641
	.long	L_1+5234
	.long	records__DRINT641
	.long	L_1+5243
	.long	records__CRINT641
	.long	L_1+5252
	.long	records__BRINT641
	.long	L_1+5261
	.long	records__ARINT641
	.long	L_1+5270
	.long	records__FRUINT3210
	.long	L_1+5279
	.long	records__ERUINT3210
	.long	L_1+5290
	.long	records__DRUINT3210
	.long	L_1+5301
	.long	records__CRUINT3210
	.long	L_1+5312
	.long	records__BRUINT3210
	.long	L_1+5323
	.long	records__ARUINT3210
	.long	L_1+5334
	.long	records__FRUINT329
	.long	L_1+5345
	.long	records__ERUINT329
	.long	L_1+5355
	.long	records__DRUINT329
	.long	L_1+5365
	.long	records__CRUINT329
	.long	L_1+5375
	.long	records__BRUINT329
	.long	L_1+5385
	.long	records__ARUINT329
	.long	L_1+5395
	.long	records__FRUINT328
	.long	L_1+5405
	.long	records__ERUINT328
	.long	L_1+5415
	.long	records__DRUINT328
	.long	L_1+5425
	.long	records__CRUINT328
	.long	L_1+5435
	.long	records__BRUINT328
	.long	L_1+5445
	.long	records__ARUINT328
	.long	L_1+5455
	.long	records__FRUINT327
	.long	L_1+5465
	.long	records__ERUINT327
	.long	L_1+5475
	.long	records__DRUINT327
	.long	L_1+5485
	.long	records__CRUINT327
	.long	L_1+5495
	.long	records__BRUINT327
	.long	L_1+5505
	.long	records__ARUINT327
	.long	L_1+5515
	.long	records__FRUINT326
	.long	L_1+5525
	.long	records__ERUINT326
	.long	L_1+5535
	.long	records__DRUINT326
	.long	L_1+5545
	.long	records__CRUINT326
	.long	L_1+5555
	.long	records__BRUINT326
	.long	L_1+5565
	.long	records__ARUINT326
	.long	L_1+5575
	.long	records__FRUINT325
	.long	L_1+5585
	.long	records__ERUINT325
	.long	L_1+5595
	.long	records__DRUINT325
	.long	L_1+5605
	.long	records__CRUINT325
	.long	L_1+5615
	.long	records__BRUINT325
	.long	L_1+5625
	.long	records__ARUINT325
	.long	L_1+5635
	.long	records__FRUINT324
	.long	L_1+5645
	.long	records__ERUINT324
	.long	L_1+5655
	.long	records__DRUINT324
	.long	L_1+5665
	.long	records__CRUINT324
	.long	L_1+5675
	.long	records__BRUINT324
	.long	L_1+5685
	.long	records__ARUINT324
	.long	L_1+5695
	.long	records__FRUINT323
	.long	L_1+5705
	.long	records__ERUINT323
	.long	L_1+5715
	.long	records__DRUINT323
	.long	L_1+5725
	.long	records__CRUINT323
	.long	L_1+5735
	.long	records__BRUINT323
	.long	L_1+5745
	.long	records__ARUINT323
	.long	L_1+5755
	.long	records__FRUINT322
	.long	L_1+5765
	.long	records__ERUINT322
	.long	L_1+5775
	.long	records__DRUINT322
	.long	L_1+5785
	.long	records__CRUINT322
	.long	L_1+5795
	.long	records__BRUINT322
	.long	L_1+5805
	.long	records__ARUINT322
	.long	L_1+5815
	.long	records__FRUINT321
	.long	L_1+5825
	.long	records__ERUINT321
	.long	L_1+5835
	.long	records__DRUINT321
	.long	L_1+5845
	.long	records__CRUINT321
	.long	L_1+5855
	.long	records__BRUINT321
	.long	L_1+5865
	.long	records__ARUINT321
	.long	L_1+5875
	.long	records__FRINT3210
	.long	L_1+5885
	.long	records__ERINT3210
	.long	L_1+5895
	.long	records__DRINT3210
	.long	L_1+5905
	.long	records__CRINT3210
	.long	L_1+5915
	.long	records__BRINT3210
	.long	L_1+5925
	.long	records__ARINT3210
	.long	L_1+5935
	.long	records__FRINT329
	.long	L_1+5945
	.long	records__ERINT329
	.long	L_1+5954
	.long	records__DRINT329
	.long	L_1+5963
	.long	records__CRINT329
	.long	L_1+5972
	.long	records__BRINT329
	.long	L_1+5981
	.long	records__ARINT329
	.long	L_1+5990
	.long	records__FRINT328
	.long	L_1+5999
	.long	records__ERINT328
	.long	L_1+6008
	.long	records__DRINT328
	.long	L_1+6017
	.long	records__CRINT328
	.long	L_1+6026
	.long	records__BRINT328
	.long	L_1+6035
	.long	records__ARINT328
	.long	L_1+6044
	.long	records__FRINT327
	.long	L_1+6053
	.long	records__ERINT327
	.long	L_1+6062
	.long	records__DRINT327
	.long	L_1+6071
	.long	records__CRINT327
	.long	L_1+6080
	.long	records__BRINT327
	.long	L_1+6089
	.long	records__ARINT327
	.long	L_1+6098
	.long	records__FRINT326
	.long	L_1+6107
	.long	records__ERINT326
	.long	L_1+6116
	.long	records__DRINT326
	.long	L_1+6125
	.long	records__CRINT326
	.long	L_1+6134
	.long	records__BRINT326
	.long	L_1+6143
	.long	records__ARINT326
	.long	L_1+6152
	.long	records__FRINT325
	.long	L_1+6161
	.long	records__ERINT325
	.long	L_1+6170
	.long	records__DRINT325
	.long	L_1+6179
	.long	records__CRINT325
	.long	L_1+6188
	.long	records__BRINT325
	.long	L_1+6197
	.long	records__ARINT325
	.long	L_1+6206
	.long	records__FRINT324
	.long	L_1+6215
	.long	records__ERINT324
	.long	L_1+6224
	.long	records__DRINT324
	.long	L_1+6233
	.long	records__CRINT324
	.long	L_1+6242
	.long	records__BRINT324
	.long	L_1+6251
	.long	records__ARINT324
	.long	L_1+6260
	.long	records__FRINT323
	.long	L_1+6269
	.long	records__ERINT323
	.long	L_1+6278
	.long	records__DRINT323
	.long	L_1+6287
	.long	records__CRINT323
	.long	L_1+6296
	.long	records__BRINT323
	.long	L_1+6305
	.long	records__ARINT323
	.long	L_1+6314
	.long	records__FRINT322
	.long	L_1+6323
	.long	records__ERINT322
	.long	L_1+6332
	.long	records__DRINT322
	.long	L_1+6341
	.long	records__CRINT322
	.long	L_1+6350
	.long	records__BRINT322
	.long	L_1+6359
	.long	records__ARINT322
	.long	L_1+6368
	.long	records__FRINT321
	.long	L_1+6377
	.long	records__ERINT321
	.long	L_1+6386
	.long	records__DRINT321
	.long	L_1+6395
	.long	records__CRINT321
	.long	L_1+6404
	.long	records__BRINT321
	.long	L_1+6413
	.long	records__ARINT321
	.long	L_1+6422
	.long	records__FRWIDECHAR10
	.long	L_1+6431
	.long	records__ERWIDECHAR10
	.long	L_1+6444
	.long	records__DRWIDECHAR10
	.long	L_1+6457
	.long	records__CRWIDECHAR10
	.long	L_1+6470
	.long	records__BRWIDECHAR10
	.long	L_1+6483
	.long	records__ARWIDECHAR10
	.long	L_1+6496
	.long	records__FRWIDECHAR9
	.long	L_1+6509
	.long	records__ERWIDECHAR9
	.long	L_1+6521
	.long	records__DRWIDECHAR9
	.long	L_1+6533
	.long	records__CRWIDECHAR9
	.long	L_1+6545
	.long	records__BRWIDECHAR9
	.long	L_1+6557
	.long	records__ARWIDECHAR9
	.long	L_1+6569
	.long	records__FRWIDECHAR8
	.long	L_1+6581
	.long	records__ERWIDECHAR8
	.long	L_1+6593
	.long	records__DRWIDECHAR8
	.long	L_1+6605
	.long	records__CRWIDECHAR8
	.long	L_1+6617
	.long	records__BRWIDECHAR8
	.long	L_1+6629
	.long	records__ARWIDECHAR8
	.long	L_1+6641
	.long	records__FRWIDECHAR7
	.long	L_1+6653
	.long	records__ERWIDECHAR7
	.long	L_1+6665
	.long	records__DRWIDECHAR7
	.long	L_1+6677
	.long	records__CRWIDECHAR7
	.long	L_1+6689
	.long	records__BRWIDECHAR7
	.long	L_1+6701
	.long	records__ARWIDECHAR7
	.long	L_1+6713
	.long	records__FRWIDECHAR6
	.long	L_1+6725
	.long	records__ERWIDECHAR6
	.long	L_1+6737
	.long	records__DRWIDECHAR6
	.long	L_1+6749
	.long	records__CRWIDECHAR6
	.long	L_1+6761
	.long	records__BRWIDECHAR6
	.long	L_1+6773
	.long	records__ARWIDECHAR6
	.long	L_1+6785
	.long	records__FRWIDECHAR5
	.long	L_1+6797
	.long	records__ERWIDECHAR5
	.long	L_1+6809
	.long	records__DRWIDECHAR5
	.long	L_1+6821
	.long	records__CRWIDECHAR5
	.long	L_1+6833
	.long	records__BRWIDECHAR5
	.long	L_1+6845
	.long	records__ARWIDECHAR5
	.long	L_1+6857
	.long	records__FRWIDECHAR4
	.long	L_1+6869
	.long	records__ERWIDECHAR4
	.long	L_1+6881
	.long	records__DRWIDECHAR4
	.long	L_1+6893
	.long	records__CRWIDECHAR4
	.long	L_1+6905
	.long	records__BRWIDECHAR4
	.long	L_1+6917
	.long	records__ARWIDECHAR4
	.long	L_1+6929
	.long	records__FRWIDECHAR3
	.long	L_1+6941
	.long	records__ERWIDECHAR3
	.long	L_1+6953
	.long	records__DRWIDECHAR3
	.long	L_1+6965
	.long	records__CRWIDECHAR3
	.long	L_1+6977
	.long	records__BRWIDECHAR3
	.long	L_1+6989
	.long	records__ARWIDECHAR3
	.long	L_1+7001
	.long	records__FRWIDECHAR2
	.long	L_1+7013
	.long	records__ERWIDECHAR2
	.long	L_1+7025
	.long	records__DRWIDECHAR2
	.long	L_1+7037
	.long	records__CRWIDECHAR2
	.long	L_1+7049
	.long	records__BRWIDECHAR2
	.long	L_1+7061
	.long	records__ARWIDECHAR2
	.long	L_1+7073
	.long	records__FRWIDECHAR1
	.long	L_1+7085
	.long	records__ERWIDECHAR1
	.long	L_1+7097
	.long	records__DRWIDECHAR1
	.long	L_1+7109
	.long	records__CRWIDECHAR1
	.long	L_1+7121
	.long	records__BRWIDECHAR1
	.long	L_1+7133
	.long	records__ARWIDECHAR1
	.long	L_1+7145
	.long	records__FRBOOLEAN10
	.long	L_1+7157
	.long	records__ERBOOLEAN10
	.long	L_1+7169
	.long	records__DRBOOLEAN10
	.long	L_1+7181
	.long	records__CRBOOLEAN10
	.long	L_1+7193
	.long	records__BRBOOLEAN10
	.long	L_1+7205
	.long	records__ARBOOLEAN10
	.long	L_1+7217
	.long	records__FRBOOLEAN9
	.long	L_1+7229
	.long	records__ERBOOLEAN9
	.long	L_1+7240
	.long	records__DRBOOLEAN9
	.long	L_1+7251
	.long	records__CRBOOLEAN9
	.long	L_1+7262
	.long	records__BRBOOLEAN9
	.long	L_1+7273
	.long	records__ARBOOLEAN9
	.long	L_1+7284
	.long	records__FRBOOLEAN8
	.long	L_1+7295
	.long	records__ERBOOLEAN8
	.long	L_1+7306
	.long	records__DRBOOLEAN8
	.long	L_1+7317
	.long	records__CRBOOLEAN8
	.long	L_1+7328
	.long	records__BRBOOLEAN8
	.long	L_1+7339
	.long	records__ARBOOLEAN8
	.long	L_1+7350
	.long	records__FRBOOLEAN7
	.long	L_1+7361
	.long	records__ERBOOLEAN7
	.long	L_1+7372
	.long	records__DRBOOLEAN7
	.long	L_1+7383
	.long	records__CRBOOLEAN7
	.long	L_1+7394
	.long	records__BRBOOLEAN7
	.long	L_1+7405
	.long	records__ARBOOLEAN7
	.long	L_1+7416
	.long	records__FRBOOLEAN6
	.long	L_1+7427
	.long	records__ERBOOLEAN6
	.long	L_1+7438
	.long	records__DRBOOLEAN6
	.long	L_1+7449
	.long	records__CRBOOLEAN6
	.long	L_1+7460
	.long	records__BRBOOLEAN6
	.long	L_1+7471
	.long	records__ARBOOLEAN6
	.long	L_1+7482
	.long	records__FRBOOLEAN5
	.long	L_1+7493
	.long	records__ERBOOLEAN5
	.long	L_1+7504
	.long	records__DRBOOLEAN5
	.long	L_1+7515
	.long	records__CRBOOLEAN5
	.long	L_1+7526
	.long	records__BRBOOLEAN5
	.long	L_1+7537
	.long	records__ARBOOLEAN5
	.long	L_1+7548
	.long	records__FRBOOLEAN4
	.long	L_1+7559
	.long	records__ERBOOLEAN4
	.long	L_1+7570
	.long	records__DRBOOLEAN4
	.long	L_1+7581
	.long	records__CRBOOLEAN4
	.long	L_1+7592
	.long	records__BRBOOLEAN4
	.long	L_1+7603
	.long	records__ARBOOLEAN4
	.long	L_1+7614
	.long	records__FRBOOLEAN3
	.long	L_1+7625
	.long	records__ERBOOLEAN3
	.long	L_1+7636
	.long	records__DRBOOLEAN3
	.long	L_1+7647
	.long	records__CRBOOLEAN3
	.long	L_1+7658
	.long	records__BRBOOLEAN3
	.long	L_1+7669
	.long	records__ARBOOLEAN3
	.long	L_1+7680
	.long	records__FRBOOLEAN2
	.long	L_1+7691
	.long	records__ERBOOLEAN2
	.long	L_1+7702
	.long	records__DRBOOLEAN2
	.long	L_1+7713
	.long	records__CRBOOLEAN2
	.long	L_1+7724
	.long	records__BRBOOLEAN2
	.long	L_1+7735
	.long	records__ARBOOLEAN2
	.long	L_1+7746
	.long	records__FRBOOLEAN1
	.long	L_1+7757
	.long	records__ERBOOLEAN1
	.long	L_1+7768
	.long	records__DRBOOLEAN1
	.long	L_1+7779
	.long	records__CRBOOLEAN1
	.long	L_1+7790
	.long	records__BRBOOLEAN1
	.long	L_1+7801
	.long	records__ARBOOLEAN1
	.long	L_1+7812
	.long	records__FRCHAR100
	.long	L_1+7823
	.long	records__ERCHAR100
	.long	L_1+7833
	.long	records__DRCHAR100
	.long	L_1+7843
	.long	records__CRCHAR100
	.long	L_1+7853
	.long	records__BRCHAR100
	.long	L_1+7863
	.long	records__ARCHAR100
	.long	L_1+7873
	.long	records__FRCHAR99
	.long	L_1+7883
	.long	records__ERCHAR99
	.long	L_1+7892
	.long	records__DRCHAR99
	.long	L_1+7901
	.long	records__CRCHAR99
	.long	L_1+7910
	.long	records__BRCHAR99
	.long	L_1+7919
	.long	records__ARCHAR99
	.long	L_1+7928
	.long	records__FRCHAR98
	.long	L_1+7937
	.long	records__ERCHAR98
	.long	L_1+7946
	.long	records__DRCHAR98
	.long	L_1+7955
	.long	records__CRCHAR98
	.long	L_1+7964
	.long	records__BRCHAR98
	.long	L_1+7973
	.long	records__ARCHAR98
	.long	L_1+7982
	.long	records__FRCHAR97
	.long	L_1+7991
	.long	records__ERCHAR97
	.long	L_1+8000
	.long	records__DRCHAR97
	.long	L_1+8009
	.long	records__CRCHAR97
	.long	L_1+8018
	.long	records__BRCHAR97
	.long	L_1+8027
	.long	records__ARCHAR97
	.long	L_1+8036
	.long	records__FRCHAR96
	.long	L_1+8045
	.long	records__ERCHAR96
	.long	L_1+8054
	.long	records__DRCHAR96
	.long	L_1+8063
	.long	records__CRCHAR96
	.long	L_1+8072
	.long	records__BRCHAR96
	.long	L_1+8081
	.long	records__ARCHAR96
	.long	L_1+8090
	.long	records__FRCHAR95
	.long	L_1+8099
	.long	records__ERCHAR95
	.long	L_1+8108
	.long	records__DRCHAR95
	.long	L_1+8117
	.long	records__CRCHAR95
	.long	L_1+8126
	.long	records__BRCHAR95
	.long	L_1+8135
	.long	records__ARCHAR95
	.long	L_1+8144
	.long	records__FRCHAR94
	.long	L_1+8153
	.long	records__ERCHAR94
	.long	L_1+8162
	.long	records__DRCHAR94
	.long	L_1+8171
	.long	records__CRCHAR94
	.long	L_1+8180
	.long	records__BRCHAR94
	.long	L_1+8189
	.long	records__ARCHAR94
	.long	L_1+8198
	.long	records__FRCHAR93
	.long	L_1+8207
	.long	records__ERCHAR93
	.long	L_1+8216
	.long	records__DRCHAR93
	.long	L_1+8225
	.long	records__CRCHAR93
	.long	L_1+8234
	.long	records__BRCHAR93
	.long	L_1+8243
	.long	records__ARCHAR93
	.long	L_1+8252
	.long	records__FRCHAR92
	.long	L_1+8261
	.long	records__ERCHAR92
	.long	L_1+8270
	.long	records__DRCHAR92
	.long	L_1+8279
	.long	records__CRCHAR92
	.long	L_1+8288
	.long	records__BRCHAR92
	.long	L_1+8297
	.long	records__ARCHAR92
	.long	L_1+8306
	.long	records__FRCHAR91
	.long	L_1+8315
	.long	records__ERCHAR91
	.long	L_1+8324
	.long	records__DRCHAR91
	.long	L_1+8333
	.long	records__CRCHAR91
	.long	L_1+8342
	.long	records__BRCHAR91
	.long	L_1+8351
	.long	records__ARCHAR91
	.long	L_1+8360
	.long	records__FRCHAR90
	.long	L_1+8369
	.long	records__ERCHAR90
	.long	L_1+8378
	.long	records__DRCHAR90
	.long	L_1+8387
	.long	records__CRCHAR90
	.long	L_1+8396
	.long	records__BRCHAR90
	.long	L_1+8405
	.long	records__ARCHAR90
	.long	L_1+8414
	.long	records__FRCHAR89
	.long	L_1+8423
	.long	records__ERCHAR89
	.long	L_1+8432
	.long	records__DRCHAR89
	.long	L_1+8441
	.long	records__CRCHAR89
	.long	L_1+8450
	.long	records__BRCHAR89
	.long	L_1+8459
	.long	records__ARCHAR89
	.long	L_1+8468
	.long	records__FRCHAR88
	.long	L_1+8477
	.long	records__ERCHAR88
	.long	L_1+8486
	.long	records__DRCHAR88
	.long	L_1+8495
	.long	records__CRCHAR88
	.long	L_1+8504
	.long	records__BRCHAR88
	.long	L_1+8513
	.long	records__ARCHAR88
	.long	L_1+8522
	.long	records__FRCHAR87
	.long	L_1+8531
	.long	records__ERCHAR87
	.long	L_1+8540
	.long	records__DRCHAR87
	.long	L_1+8549
	.long	records__CRCHAR87
	.long	L_1+8558
	.long	records__BRCHAR87
	.long	L_1+8567
	.long	records__ARCHAR87
	.long	L_1+8576
	.long	records__FRCHAR86
	.long	L_1+8585
	.long	records__ERCHAR86
	.long	L_1+8594
	.long	records__DRCHAR86
	.long	L_1+8603
	.long	records__CRCHAR86
	.long	L_1+8612
	.long	records__BRCHAR86
	.long	L_1+8621
	.long	records__ARCHAR86
	.long	L_1+8630
	.long	records__FRCHAR85
	.long	L_1+8639
	.long	records__ERCHAR85
	.long	L_1+8648
	.long	records__DRCHAR85
	.long	L_1+8657
	.long	records__CRCHAR85
	.long	L_1+8666
	.long	records__BRCHAR85
	.long	L_1+8675
	.long	records__ARCHAR85
	.long	L_1+8684
	.long	records__FRCHAR84
	.long	L_1+8693
	.long	records__ERCHAR84
	.long	L_1+8702
	.long	records__DRCHAR84
	.long	L_1+8711
	.long	records__CRCHAR84
	.long	L_1+8720
	.long	records__BRCHAR84
	.long	L_1+8729
	.long	records__ARCHAR84
	.long	L_1+8738
	.long	records__FRCHAR83
	.long	L_1+8747
	.long	records__ERCHAR83
	.long	L_1+8756
	.long	records__DRCHAR83
	.long	L_1+8765
	.long	records__CRCHAR83
	.long	L_1+8774
	.long	records__BRCHAR83
	.long	L_1+8783
	.long	records__ARCHAR83
	.long	L_1+8792
	.long	records__FRCHAR82
	.long	L_1+8801
	.long	records__ERCHAR82
	.long	L_1+8810
	.long	records__DRCHAR82
	.long	L_1+8819
	.long	records__CRCHAR82
	.long	L_1+8828
	.long	records__BRCHAR82
	.long	L_1+8837
	.long	records__ARCHAR82
	.long	L_1+8846
	.long	records__FRCHAR81
	.long	L_1+8855
	.long	records__ERCHAR81
	.long	L_1+8864
	.long	records__DRCHAR81
	.long	L_1+8873
	.long	records__CRCHAR81
	.long	L_1+8882
	.long	records__BRCHAR81
	.long	L_1+8891
	.long	records__ARCHAR81
	.long	L_1+8900
	.long	records__FRCHAR80
	.long	L_1+8909
	.long	records__ERCHAR80
	.long	L_1+8918
	.long	records__DRCHAR80
	.long	L_1+8927
	.long	records__CRCHAR80
	.long	L_1+8936
	.long	records__BRCHAR80
	.long	L_1+8945
	.long	records__ARCHAR80
	.long	L_1+8954
	.long	records__FRCHAR79
	.long	L_1+8963
	.long	records__ERCHAR79
	.long	L_1+8972
	.long	records__DRCHAR79
	.long	L_1+8981
	.long	records__CRCHAR79
	.long	L_1+8990
	.long	records__BRCHAR79
	.long	L_1+8999
	.long	records__ARCHAR79
	.long	L_1+9008
	.long	records__FRCHAR78
	.long	L_1+9017
	.long	records__ERCHAR78
	.long	L_1+9026
	.long	records__DRCHAR78
	.long	L_1+9035
	.long	records__CRCHAR78
	.long	L_1+9044
	.long	records__BRCHAR78
	.long	L_1+9053
	.long	records__ARCHAR78
	.long	L_1+9062
	.long	records__FRCHAR77
	.long	L_1+9071
	.long	records__ERCHAR77
	.long	L_1+9080
	.long	records__DRCHAR77
	.long	L_1+9089
	.long	records__CRCHAR77
	.long	L_1+9098
	.long	records__BRCHAR77
	.long	L_1+9107
	.long	records__ARCHAR77
	.long	L_1+9116
	.long	records__FRCHAR76
	.long	L_1+9125
	.long	records__ERCHAR76
	.long	L_1+9134
	.long	records__DRCHAR76
	.long	L_1+9143
	.long	records__CRCHAR76
	.long	L_1+9152
	.long	records__BRCHAR76
	.long	L_1+9161
	.long	records__ARCHAR76
	.long	L_1+9170
	.long	records__FRCHAR75
	.long	L_1+9179
	.long	records__ERCHAR75
	.long	L_1+9188
	.long	records__DRCHAR75
	.long	L_1+9197
	.long	records__CRCHAR75
	.long	L_1+9206
	.long	records__BRCHAR75
	.long	L_1+9215
	.long	records__ARCHAR75
	.long	L_1+9224
	.long	records__FRCHAR74
	.long	L_1+9233
	.long	records__ERCHAR74
	.long	L_1+9242
	.long	records__DRCHAR74
	.long	L_1+9251
	.long	records__CRCHAR74
	.long	L_1+9260
	.long	records__BRCHAR74
	.long	L_1+9269
	.long	records__ARCHAR74
	.long	L_1+9278
	.long	records__FRCHAR73
	.long	L_1+9287
	.long	records__ERCHAR73
	.long	L_1+9296
	.long	records__DRCHAR73
	.long	L_1+9305
	.long	records__CRCHAR73
	.long	L_1+9314
	.long	records__BRCHAR73
	.long	L_1+9323
	.long	records__ARCHAR73
	.long	L_1+9332
	.long	records__FRCHAR72
	.long	L_1+9341
	.long	records__ERCHAR72
	.long	L_1+9350
	.long	records__DRCHAR72
	.long	L_1+9359
	.long	records__CRCHAR72
	.long	L_1+9368
	.long	records__BRCHAR72
	.long	L_1+9377
	.long	records__ARCHAR72
	.long	L_1+9386
	.long	records__FRCHAR71
	.long	L_1+9395
	.long	records__ERCHAR71
	.long	L_1+9404
	.long	records__DRCHAR71
	.long	L_1+9413
	.long	records__CRCHAR71
	.long	L_1+9422
	.long	records__BRCHAR71
	.long	L_1+9431
	.long	records__ARCHAR71
	.long	L_1+9440
	.long	records__FRCHAR70
	.long	L_1+9449
	.long	records__ERCHAR70
	.long	L_1+9458
	.long	records__DRCHAR70
	.long	L_1+9467
	.long	records__CRCHAR70
	.long	L_1+9476
	.long	records__BRCHAR70
	.long	L_1+9485
	.long	records__ARCHAR70
	.long	L_1+9494
	.long	records__FRCHAR69
	.long	L_1+9503
	.long	records__ERCHAR69
	.long	L_1+9512
	.long	records__DRCHAR69
	.long	L_1+9521
	.long	records__CRCHAR69
	.long	L_1+9530
	.long	records__BRCHAR69
	.long	L_1+9539
	.long	records__ARCHAR69
	.long	L_1+9548
	.long	records__FRCHAR68
	.long	L_1+9557
	.long	records__ERCHAR68
	.long	L_1+9566
	.long	records__DRCHAR68
	.long	L_1+9575
	.long	records__CRCHAR68
	.long	L_1+9584
	.long	records__BRCHAR68
	.long	L_1+9593
	.long	records__ARCHAR68
	.long	L_1+9602
	.long	records__FRCHAR67
	.long	L_1+9611
	.long	records__ERCHAR67
	.long	L_1+9620
	.long	records__DRCHAR67
	.long	L_1+9629
	.long	records__CRCHAR67
	.long	L_1+9638
	.long	records__BRCHAR67
	.long	L_1+9647
	.long	records__ARCHAR67
	.long	L_1+9656
	.long	records__FRCHAR66
	.long	L_1+9665
	.long	records__ERCHAR66
	.long	L_1+9674
	.long	records__DRCHAR66
	.long	L_1+9683
	.long	records__CRCHAR66
	.long	L_1+9692
	.long	records__BRCHAR66
	.long	L_1+9701
	.long	records__ARCHAR66
	.long	L_1+9710
	.long	records__FRCHAR65
	.long	L_1+9719
	.long	records__ERCHAR65
	.long	L_1+9728
	.long	records__DRCHAR65
	.long	L_1+9737
	.long	records__CRCHAR65
	.long	L_1+9746
	.long	records__BRCHAR65
	.long	L_1+9755
	.long	records__ARCHAR65
	.long	L_1+9764
	.long	records__FRCHAR64
	.long	L_1+9773
	.long	records__ERCHAR64
	.long	L_1+9782
	.long	records__DRCHAR64
	.long	L_1+9791
	.long	records__CRCHAR64
	.long	L_1+9800
	.long	records__BRCHAR64
	.long	L_1+9809
	.long	records__ARCHAR64
	.long	L_1+9818
	.long	records__FRCHAR63
	.long	L_1+9827
	.long	records__ERCHAR63
	.long	L_1+9836
	.long	records__DRCHAR63
	.long	L_1+9845
	.long	records__CRCHAR63
	.long	L_1+9854
	.long	records__BRCHAR63
	.long	L_1+9863
	.long	records__ARCHAR63
	.long	L_1+9872
	.long	records__FRCHAR62
	.long	L_1+9881
	.long	records__ERCHAR62
	.long	L_1+9890
	.long	records__DRCHAR62
	.long	L_1+9899
	.long	records__CRCHAR62
	.long	L_1+9908
	.long	records__BRCHAR62
	.long	L_1+9917
	.long	records__ARCHAR62
	.long	L_1+9926
	.long	records__FRCHAR61
	.long	L_1+9935
	.long	records__ERCHAR61
	.long	L_1+9944
	.long	records__DRCHAR61
	.long	L_1+9953
	.long	records__CRCHAR61
	.long	L_1+9962
	.long	records__BRCHAR61
	.long	L_1+9971
	.long	records__ARCHAR61
	.long	L_1+9980
	.long	records__FRCHAR60
	.long	L_1+9989
	.long	records__ERCHAR60
	.long	L_1+9998
	.long	records__DRCHAR60
	.long	L_1+10007
	.long	records__CRCHAR60
	.long	L_1+10016
	.long	records__BRCHAR60
	.long	L_1+10025
	.long	records__ARCHAR60
	.long	L_1+10034
	.long	records__FRCHAR59
	.long	L_1+10043
	.long	records__ERCHAR59
	.long	L_1+10052
	.long	records__DRCHAR59
	.long	L_1+10061
	.long	records__CRCHAR59
	.long	L_1+10070
	.long	records__BRCHAR59
	.long	L_1+10079
	.long	records__ARCHAR59
	.long	L_1+10088
	.long	records__FRCHAR58
	.long	L_1+10097
	.long	records__ERCHAR58
	.long	L_1+10106
	.long	records__DRCHAR58
	.long	L_1+10115
	.long	records__CRCHAR58
	.long	L_1+10124
	.long	records__BRCHAR58
	.long	L_1+10133
	.long	records__ARCHAR58
	.long	L_1+10142
	.long	records__FRCHAR57
	.long	L_1+10151
	.long	records__ERCHAR57
	.long	L_1+10160
	.long	records__DRCHAR57
	.long	L_1+10169
	.long	records__CRCHAR57
	.long	L_1+10178
	.long	records__BRCHAR57
	.long	L_1+10187
	.long	records__ARCHAR57
	.long	L_1+10196
	.long	records__FRCHAR56
	.long	L_1+10205
	.long	records__ERCHAR56
	.long	L_1+10214
	.long	records__DRCHAR56
	.long	L_1+10223
	.long	records__CRCHAR56
	.long	L_1+10232
	.long	records__BRCHAR56
	.long	L_1+10241
	.long	records__ARCHAR56
	.long	L_1+10250
	.long	records__FRCHAR55
	.long	L_1+10259
	.long	records__ERCHAR55
	.long	L_1+10268
	.long	records__DRCHAR55
	.long	L_1+10277
	.long	records__CRCHAR55
	.long	L_1+10286
	.long	records__BRCHAR55
	.long	L_1+10295
	.long	records__ARCHAR55
	.long	L_1+10304
	.long	records__FRCHAR54
	.long	L_1+10313
	.long	records__ERCHAR54
	.long	L_1+10322
	.long	records__DRCHAR54
	.long	L_1+10331
	.long	records__CRCHAR54
	.long	L_1+10340
	.long	records__BRCHAR54
	.long	L_1+10349
	.long	records__ARCHAR54
	.long	L_1+10358
	.long	records__FRCHAR53
	.long	L_1+10367
	.long	records__ERCHAR53
	.long	L_1+10376
	.long	records__DRCHAR53
	.long	L_1+10385
	.long	records__CRCHAR53
	.long	L_1+10394
	.long	records__BRCHAR53
	.long	L_1+10403
	.long	records__ARCHAR53
	.long	L_1+10412
	.long	records__FRCHAR52
	.long	L_1+10421
	.long	records__ERCHAR52
	.long	L_1+10430
	.long	records__DRCHAR52
	.long	L_1+10439
	.long	records__CRCHAR52
	.long	L_1+10448
	.long	records__BRCHAR52
	.long	L_1+10457
	.long	records__ARCHAR52
	.long	L_1+10466
	.long	records__FRCHAR51
	.long	L_1+10475
	.long	records__ERCHAR51
	.long	L_1+10484
	.long	records__DRCHAR51
	.long	L_1+10493
	.long	records__CRCHAR51
	.long	L_1+10502
	.long	records__BRCHAR51
	.long	L_1+10511
	.long	records__ARCHAR51
	.long	L_1+10520
	.long	records__FRCHAR50
	.long	L_1+10529
	.long	records__ERCHAR50
	.long	L_1+10538
	.long	records__DRCHAR50
	.long	L_1+10547
	.long	records__CRCHAR50
	.long	L_1+10556
	.long	records__BRCHAR50
	.long	L_1+10565
	.long	records__ARCHAR50
	.long	L_1+10574
	.long	records__FRCHAR49
	.long	L_1+10583
	.long	records__ERCHAR49
	.long	L_1+10592
	.long	records__DRCHAR49
	.long	L_1+10601
	.long	records__CRCHAR49
	.long	L_1+10610
	.long	records__BRCHAR49
	.long	L_1+10619
	.long	records__ARCHAR49
	.long	L_1+10628
	.long	records__FRCHAR48
	.long	L_1+10637
	.long	records__ERCHAR48
	.long	L_1+10646
	.long	records__DRCHAR48
	.long	L_1+10655
	.long	records__CRCHAR48
	.long	L_1+10664
	.long	records__BRCHAR48
	.long	L_1+10673
	.long	records__ARCHAR48
	.long	L_1+10682
	.long	records__FRCHAR47
	.long	L_1+10691
	.long	records__ERCHAR47
	.long	L_1+10700
	.long	records__DRCHAR47
	.long	L_1+10709
	.long	records__CRCHAR47
	.long	L_1+10718
	.long	records__BRCHAR47
	.long	L_1+10727
	.long	records__ARCHAR47
	.long	L_1+10736
	.long	records__FRCHAR46
	.long	L_1+10745
	.long	records__ERCHAR46
	.long	L_1+10754
	.long	records__DRCHAR46
	.long	L_1+10763
	.long	records__CRCHAR46
	.long	L_1+10772
	.long	records__BRCHAR46
	.long	L_1+10781
	.long	records__ARCHAR46
	.long	L_1+10790
	.long	records__FRCHAR45
	.long	L_1+10799
	.long	records__ERCHAR45
	.long	L_1+10808
	.long	records__DRCHAR45
	.long	L_1+10817
	.long	records__CRCHAR45
	.long	L_1+10826
	.long	records__BRCHAR45
	.long	L_1+10835
	.long	records__ARCHAR45
	.long	L_1+10844
	.long	records__FRCHAR44
	.long	L_1+10853
	.long	records__ERCHAR44
	.long	L_1+10862
	.long	records__DRCHAR44
	.long	L_1+10871
	.long	records__CRCHAR44
	.long	L_1+10880
	.long	records__BRCHAR44
	.long	L_1+10889
	.long	records__ARCHAR44
	.long	L_1+10898
	.long	records__FRCHAR43
	.long	L_1+10907
	.long	records__ERCHAR43
	.long	L_1+10916
	.long	records__DRCHAR43
	.long	L_1+10925
	.long	records__CRCHAR43
	.long	L_1+10934
	.long	records__BRCHAR43
	.long	L_1+10943
	.long	records__ARCHAR43
	.long	L_1+10952
	.long	records__FRCHAR42
	.long	L_1+10961
	.long	records__ERCHAR42
	.long	L_1+10970
	.long	records__DRCHAR42
	.long	L_1+10979
	.long	records__CRCHAR42
	.long	L_1+10988
	.long	records__BRCHAR42
	.long	L_1+10997
	.long	records__ARCHAR42
	.long	L_1+11006
	.long	records__FRCHAR41
	.long	L_1+11015
	.long	records__ERCHAR41
	.long	L_1+11024
	.long	records__DRCHAR41
	.long	L_1+11033
	.long	records__CRCHAR41
	.long	L_1+11042
	.long	records__BRCHAR41
	.long	L_1+11051
	.long	records__ARCHAR41
	.long	L_1+11060
	.long	records__FRCHAR40
	.long	L_1+11069
	.long	records__ERCHAR40
	.long	L_1+11078
	.long	records__DRCHAR40
	.long	L_1+11087
	.long	records__CRCHAR40
	.long	L_1+11096
	.long	records__BRCHAR40
	.long	L_1+11105
	.long	records__ARCHAR40
	.long	L_1+11114
	.long	records__FRCHAR39
	.long	L_1+11123
	.long	records__ERCHAR39
	.long	L_1+11132
	.long	records__DRCHAR39
	.long	L_1+11141
	.long	records__CRCHAR39
	.long	L_1+11150
	.long	records__BRCHAR39
	.long	L_1+11159
	.long	records__ARCHAR39
	.long	L_1+11168
	.long	records__FRCHAR38
	.long	L_1+11177
	.long	records__ERCHAR38
	.long	L_1+11186
	.long	records__DRCHAR38
	.long	L_1+11195
	.long	records__CRCHAR38
	.long	L_1+11204
	.long	records__BRCHAR38
	.long	L_1+11213
	.long	records__ARCHAR38
	.long	L_1+11222
	.long	records__FRCHAR37
	.long	L_1+11231
	.long	records__ERCHAR37
	.long	L_1+11240
	.long	records__DRCHAR37
	.long	L_1+11249
	.long	records__CRCHAR37
	.long	L_1+11258
	.long	records__BRCHAR37
	.long	L_1+11267
	.long	records__ARCHAR37
	.long	L_1+11276
	.long	records__FRCHAR36
	.long	L_1+11285
	.long	records__ERCHAR36
	.long	L_1+11294
	.long	records__DRCHAR36
	.long	L_1+11303
	.long	records__CRCHAR36
	.long	L_1+11312
	.long	records__BRCHAR36
	.long	L_1+11321
	.long	records__ARCHAR36
	.long	L_1+11330
	.long	records__FRCHAR35
	.long	L_1+11339
	.long	records__ERCHAR35
	.long	L_1+11348
	.long	records__DRCHAR35
	.long	L_1+11357
	.long	records__CRCHAR35
	.long	L_1+11366
	.long	records__BRCHAR35
	.long	L_1+11375
	.long	records__ARCHAR35
	.long	L_1+11384
	.long	records__FRCHAR34
	.long	L_1+11393
	.long	records__ERCHAR34
	.long	L_1+11402
	.long	records__DRCHAR34
	.long	L_1+11411
	.long	records__CRCHAR34
	.long	L_1+11420
	.long	records__BRCHAR34
	.long	L_1+11429
	.long	records__ARCHAR34
	.long	L_1+11438
	.long	records__FRCHAR33
	.long	L_1+11447
	.long	records__ERCHAR33
	.long	L_1+11456
	.long	records__DRCHAR33
	.long	L_1+11465
	.long	records__CRCHAR33
	.long	L_1+11474
	.long	records__BRCHAR33
	.long	L_1+11483
	.long	records__ARCHAR33
	.long	L_1+11492
	.long	records__FRCHAR32
	.long	L_1+11501
	.long	records__ERCHAR32
	.long	L_1+11510
	.long	records__DRCHAR32
	.long	L_1+11519
	.long	records__CRCHAR32
	.long	L_1+11528
	.long	records__BRCHAR32
	.long	L_1+11537
	.long	records__ARCHAR32
	.long	L_1+11546
	.long	records__FRCHAR31
	.long	L_1+11555
	.long	records__ERCHAR31
	.long	L_1+11564
	.long	records__DRCHAR31
	.long	L_1+11573
	.long	records__CRCHAR31
	.long	L_1+11582
	.long	records__BRCHAR31
	.long	L_1+11591
	.long	records__ARCHAR31
	.long	L_1+11600
	.long	records__FRCHAR30
	.long	L_1+11609
	.long	records__ERCHAR30
	.long	L_1+11618
	.long	records__DRCHAR30
	.long	L_1+11627
	.long	records__CRCHAR30
	.long	L_1+11636
	.long	records__BRCHAR30
	.long	L_1+11645
	.long	records__ARCHAR30
	.long	L_1+11654
	.long	records__FRCHAR29
	.long	L_1+11663
	.long	records__ERCHAR29
	.long	L_1+11672
	.long	records__DRCHAR29
	.long	L_1+11681
	.long	records__CRCHAR29
	.long	L_1+11690
	.long	records__BRCHAR29
	.long	L_1+11699
	.long	records__ARCHAR29
	.long	L_1+11708
	.long	records__FRCHAR28
	.long	L_1+11717
	.long	records__ERCHAR28
	.long	L_1+11726
	.long	records__DRCHAR28
	.long	L_1+11735
	.long	records__CRCHAR28
	.long	L_1+11744
	.long	records__BRCHAR28
	.long	L_1+11753
	.long	records__ARCHAR28
	.long	L_1+11762
	.long	records__FRCHAR27
	.long	L_1+11771
	.long	records__ERCHAR27
	.long	L_1+11780
	.long	records__DRCHAR27
	.long	L_1+11789
	.long	records__CRCHAR27
	.long	L_1+11798
	.long	records__BRCHAR27
	.long	L_1+11807
	.long	records__ARCHAR27
	.long	L_1+11816
	.long	records__FRCHAR26
	.long	L_1+11825
	.long	records__ERCHAR26
	.long	L_1+11834
	.long	records__DRCHAR26
	.long	L_1+11843
	.long	records__CRCHAR26
	.long	L_1+11852
	.long	records__BRCHAR26
	.long	L_1+11861
	.long	records__ARCHAR26
	.long	L_1+11870
	.long	records__FRCHAR25
	.long	L_1+11879
	.long	records__ERCHAR25
	.long	L_1+11888
	.long	records__DRCHAR25
	.long	L_1+11897
	.long	records__CRCHAR25
	.long	L_1+11906
	.long	records__BRCHAR25
	.long	L_1+11915
	.long	records__ARCHAR25
	.long	L_1+11924
	.long	records__FRCHAR24
	.long	L_1+11933
	.long	records__ERCHAR24
	.long	L_1+11942
	.long	records__DRCHAR24
	.long	L_1+11951
	.long	records__CRCHAR24
	.long	L_1+11960
	.long	records__BRCHAR24
	.long	L_1+11969
	.long	records__ARCHAR24
	.long	L_1+11978
	.long	records__FRCHAR23
	.long	L_1+11987
	.long	records__ERCHAR23
	.long	L_1+11996
	.long	records__DRCHAR23
	.long	L_1+12005
	.long	records__CRCHAR23
	.long	L_1+12014
	.long	records__BRCHAR23
	.long	L_1+12023
	.long	records__ARCHAR23
	.long	L_1+12032
	.long	records__FRCHAR22
	.long	L_1+12041
	.long	records__ERCHAR22
	.long	L_1+12050
	.long	records__DRCHAR22
	.long	L_1+12059
	.long	records__CRCHAR22
	.long	L_1+12068
	.long	records__BRCHAR22
	.long	L_1+12077
	.long	records__ARCHAR22
	.long	L_1+12086
	.long	records__FRCHAR21
	.long	L_1+12095
	.long	records__ERCHAR21
	.long	L_1+12104
	.long	records__DRCHAR21
	.long	L_1+12113
	.long	records__CRCHAR21
	.long	L_1+12122
	.long	records__BRCHAR21
	.long	L_1+12131
	.long	records__ARCHAR21
	.long	L_1+12140
	.long	records__FRCHAR20
	.long	L_1+12149
	.long	records__ERCHAR20
	.long	L_1+12158
	.long	records__DRCHAR20
	.long	L_1+12167
	.long	records__CRCHAR20
	.long	L_1+12176
	.long	records__BRCHAR20
	.long	L_1+12185
	.long	records__ARCHAR20
	.long	L_1+12194
	.long	records__FRCHAR19
	.long	L_1+12203
	.long	records__ERCHAR19
	.long	L_1+12212
	.long	records__DRCHAR19
	.long	L_1+12221
	.long	records__CRCHAR19
	.long	L_1+12230
	.long	records__BRCHAR19
	.long	L_1+12239
	.long	records__ARCHAR19
	.long	L_1+12248
	.long	records__FRCHAR18
	.long	L_1+12257
	.long	records__ERCHAR18
	.long	L_1+12266
	.long	records__DRCHAR18
	.long	L_1+12275
	.long	records__CRCHAR18
	.long	L_1+12284
	.long	records__BRCHAR18
	.long	L_1+12293
	.long	records__ARCHAR18
	.long	L_1+12302
	.long	records__FRCHAR17
	.long	L_1+12311
	.long	records__ERCHAR17
	.long	L_1+12320
	.long	records__DRCHAR17
	.long	L_1+12329
	.long	records__CRCHAR17
	.long	L_1+12338
	.long	records__BRCHAR17
	.long	L_1+12347
	.long	records__ARCHAR17
	.long	L_1+12356
	.long	records__FRCHAR16
	.long	L_1+12365
	.long	records__ERCHAR16
	.long	L_1+12374
	.long	records__DRCHAR16
	.long	L_1+12383
	.long	records__CRCHAR16
	.long	L_1+12392
	.long	records__BRCHAR16
	.long	L_1+12401
	.long	records__ARCHAR16
	.long	L_1+12410
	.long	records__FRCHAR15
	.long	L_1+12419
	.long	records__ERCHAR15
	.long	L_1+12428
	.long	records__DRCHAR15
	.long	L_1+12437
	.long	records__CRCHAR15
	.long	L_1+12446
	.long	records__BRCHAR15
	.long	L_1+12455
	.long	records__ARCHAR15
	.long	L_1+12464
	.long	records__FRCHAR14
	.long	L_1+12473
	.long	records__ERCHAR14
	.long	L_1+12482
	.long	records__DRCHAR14
	.long	L_1+12491
	.long	records__CRCHAR14
	.long	L_1+12500
	.long	records__BRCHAR14
	.long	L_1+12509
	.long	records__ARCHAR14
	.long	L_1+12518
	.long	records__FRCHAR13
	.long	L_1+12527
	.long	records__ERCHAR13
	.long	L_1+12536
	.long	records__DRCHAR13
	.long	L_1+12545
	.long	records__CRCHAR13
	.long	L_1+12554
	.long	records__BRCHAR13
	.long	L_1+12563
	.long	records__ARCHAR13
	.long	L_1+12572
	.long	records__FRCHAR12
	.long	L_1+12581
	.long	records__ERCHAR12
	.long	L_1+12590
	.long	records__DRCHAR12
	.long	L_1+12599
	.long	records__CRCHAR12
	.long	L_1+12608
	.long	records__BRCHAR12
	.long	L_1+12617
	.long	records__ARCHAR12
	.long	L_1+12626
	.long	records__FRCHAR11
	.long	L_1+12635
	.long	records__ERCHAR11
	.long	L_1+12644
	.long	records__DRCHAR11
	.long	L_1+12653
	.long	records__CRCHAR11
	.long	L_1+12662
	.long	records__BRCHAR11
	.long	L_1+12671
	.long	records__ARCHAR11
	.long	L_1+12680
	.long	records__FRCHAR10
	.long	L_1+12689
	.long	records__ERCHAR10
	.long	L_1+12698
	.long	records__DRCHAR10
	.long	L_1+12707
	.long	records__CRCHAR10
	.long	L_1+12716
	.long	records__BRCHAR10
	.long	L_1+12725
	.long	records__ARCHAR10
	.long	L_1+12734
	.long	records__FRCHAR9
	.long	L_1+12743
	.long	records__ERCHAR9
	.long	L_1+12751
	.long	records__DRCHAR9
	.long	L_1+12759
	.long	records__CRCHAR9
	.long	L_1+12767
	.long	records__BRCHAR9
	.long	L_1+12775
	.long	records__ARCHAR9
	.long	L_1+12783
	.long	records__FRCHAR8
	.long	L_1+12791
	.long	records__ERCHAR8
	.long	L_1+12799
	.long	records__DRCHAR8
	.long	L_1+12807
	.long	records__CRCHAR8
	.long	L_1+12815
	.long	records__BRCHAR8
	.long	L_1+12823
	.long	records__ARCHAR8
	.long	L_1+12831
	.long	records__FRCHAR7
	.long	L_1+12839
	.long	records__ERCHAR7
	.long	L_1+12847
	.long	records__DRCHAR7
	.long	L_1+12855
	.long	records__CRCHAR7
	.long	L_1+12863
	.long	records__BRCHAR7
	.long	L_1+12871
	.long	records__ARCHAR7
	.long	L_1+12879
	.long	records__FRCHAR6
	.long	L_1+12887
	.long	records__ERCHAR6
	.long	L_1+12895
	.long	records__DRCHAR6
	.long	L_1+12903
	.long	records__CRCHAR6
	.long	L_1+12911
	.long	records__BRCHAR6
	.long	L_1+12919
	.long	records__ARCHAR6
	.long	L_1+12927
	.long	records__FRCHAR5
	.long	L_1+12935
	.long	records__ERCHAR5
	.long	L_1+12943
	.long	records__DRCHAR5
	.long	L_1+12951
	.long	records__CRCHAR5
	.long	L_1+12959
	.long	records__BRCHAR5
	.long	L_1+12967
	.long	records__ARCHAR5
	.long	L_1+12975
	.long	records__FRCHAR4
	.long	L_1+12983
	.long	records__ERCHAR4
	.long	L_1+12991
	.long	records__DRCHAR4
	.long	L_1+12999
	.long	records__CRCHAR4
	.long	L_1+13007
	.long	records__BRCHAR4
	.long	L_1+13015
	.long	records__ARCHAR4
	.long	L_1+13023
	.long	records__FRCHAR3
	.long	L_1+13031
	.long	records__ERCHAR3
	.long	L_1+13039
	.long	records__DRCHAR3
	.long	L_1+13047
	.long	records__CRCHAR3
	.long	L_1+13055
	.long	records__BRCHAR3
	.long	L_1+13063
	.long	records__ARCHAR3
	.long	L_1+13071
	.long	records__FRCHAR2
	.long	L_1+13079
	.long	records__ERCHAR2
	.long	L_1+13087
	.long	records__DRCHAR2
	.long	L_1+13095
	.long	records__CRCHAR2
	.long	L_1+13103
	.long	records__BRCHAR2
	.long	L_1+13111
	.long	records__ARCHAR2
	.long	L_1+13119
	.long	records__FRCHAR1
	.long	L_1+13127
	.long	records__ERCHAR1
	.long	L_1+13135
	.long	records__DRCHAR1
	.long	L_1+13143
	.long	records__CRCHAR1
	.long	L_1+13151
	.long	records__BRCHAR1
	.long	L_1+13159
	.long	records__ARCHAR1
	.long	L_1+13167
	.skip 4
	.ascii	"../SOLsun/records.m3"
	.skip 8
	.align 8
	.type	MM_records, #object
	.size	MM_records, 208
MM_records:
	.long	L_1+23748
	.skip 16
	.long	L_1+13176
	.skip 12
	.long	MM_records+148
	.skip 4
	.long	records_M3
	.long	3
	.half	1169
	.skip 2
	.long	0
	.long	1170
	.long	0
	.long	1171
	.long	1172
	.half	1173
	.skip 2
	.long	1174
	.skip 4
	.long	0
	.long	1175
	.long	1176
	.skip 4
	.long	1083335800
	.long	-1601163808
	.byte	-101
	.skip 7
	.long	0
	.long	1180
	.byte	-99
	.skip 3
	.long	1182
	.long	1150542794
	.skip 12
	.long	records_I3
	.long	MM_records+160
	.skip 4
	.long	Long_I3
	.long	MM_records+172
	.skip 4
	.long	Word_I3
	.long	MM_records+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_records+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
