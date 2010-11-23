	.file	"return_parameter_convert.mc"
	.section	".text"
	.align 4
	.type	return_parameter_convert__ret_u16_u16, #function
	.proc	04
return_parameter_convert__ret_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	return_parameter_convert__ret_u16_u16, .-return_parameter_convert__ret_u16_u16
	.align 4
	.type	return_parameter_convert__ret_u16_u64, #function
	.proc	05
return_parameter_convert__ret_u16_u64:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1:
	.size	return_parameter_convert__ret_u16_u64, .-return_parameter_convert__ret_u16_u64
	.align 4
	.type	return_parameter_convert__ret_u16_LC, #function
	.proc	05
return_parameter_convert__ret_u16_LC:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE2:
	.size	return_parameter_convert__ret_u16_LC, .-return_parameter_convert__ret_u16_LC
	.align 4
	.type	return_parameter_convert__ret_u16_i32, #function
	.proc	04
return_parameter_convert__ret_u16_i32:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	return_parameter_convert__ret_u16_i32, .-return_parameter_convert__ret_u16_i32
	.align 4
	.type	return_parameter_convert__ret_u16_I, #function
	.proc	04
return_parameter_convert__ret_u16_I:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	return_parameter_convert__ret_u16_I, .-return_parameter_convert__ret_u16_I
	.align 4
	.type	return_parameter_convert__ret_u16_i16, #function
	.proc	04
return_parameter_convert__ret_u16_i16:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	return_parameter_convert__ret_u16_i16, .-return_parameter_convert__ret_u16_i16
	.align 4
	.type	return_parameter_convert__ret_u16_C, #function
	.proc	04
return_parameter_convert__ret_u16_C:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE6:
	.size	return_parameter_convert__ret_u16_C, .-return_parameter_convert__ret_u16_C
	.align 4
	.type	return_parameter_convert__ret_u16_i64, #function
	.proc	05
return_parameter_convert__ret_u16_i64:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	return_parameter_convert__ret_u16_i64, .-return_parameter_convert__ret_u16_i64
	.align 4
	.type	return_parameter_convert__ret_u16_u8, #function
	.proc	04
return_parameter_convert__ret_u16_u8:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE8:
	.size	return_parameter_convert__ret_u16_u8, .-return_parameter_convert__ret_u16_u8
	.align 4
	.type	return_parameter_convert__ret_u16_L, #function
	.proc	05
return_parameter_convert__ret_u16_L:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	return_parameter_convert__ret_u16_L, .-return_parameter_convert__ret_u16_L
	.align 4
	.type	return_parameter_convert__ret_u16_i8, #function
	.proc	04
return_parameter_convert__ret_u16_i8:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE10:
	.size	return_parameter_convert__ret_u16_i8, .-return_parameter_convert__ret_u16_i8
	.align 4
	.type	return_parameter_convert__ret_u16_u32, #function
	.proc	04
return_parameter_convert__ret_u16_u32:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	return_parameter_convert__ret_u16_u32, .-return_parameter_convert__ret_u16_u32
	.align 4
	.type	return_parameter_convert__ret_u64_u16, #function
	.proc	05
return_parameter_convert__ret_u64_u16:
.LLFB12:
	save	%sp, -104, %sp
.LLCFI12:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE12:
	.size	return_parameter_convert__ret_u64_u16, .-return_parameter_convert__ret_u64_u16
	.align 4
	.type	return_parameter_convert__ret_u64_u64, #function
	.proc	05
return_parameter_convert__ret_u64_u64:
.LLFB13:
	save	%sp, -104, %sp
.LLCFI13:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE13:
	.size	return_parameter_convert__ret_u64_u64, .-return_parameter_convert__ret_u64_u64
	.align 4
	.type	return_parameter_convert__ret_u64_LC, #function
	.proc	05
return_parameter_convert__ret_u64_LC:
.LLFB14:
	save	%sp, -104, %sp
.LLCFI14:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE14:
	.size	return_parameter_convert__ret_u64_LC, .-return_parameter_convert__ret_u64_LC
	.align 4
	.type	return_parameter_convert__ret_u64_i32, #function
	.proc	05
return_parameter_convert__ret_u64_i32:
.LLFB15:
	save	%sp, -104, %sp
.LLCFI15:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE15:
	.size	return_parameter_convert__ret_u64_i32, .-return_parameter_convert__ret_u64_i32
	.align 4
	.type	return_parameter_convert__ret_u64_I, #function
	.proc	05
return_parameter_convert__ret_u64_I:
.LLFB16:
	save	%sp, -104, %sp
.LLCFI16:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE16:
	.size	return_parameter_convert__ret_u64_I, .-return_parameter_convert__ret_u64_I
	.align 4
	.type	return_parameter_convert__ret_u64_i16, #function
	.proc	05
return_parameter_convert__ret_u64_i16:
.LLFB17:
	save	%sp, -104, %sp
.LLCFI17:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE17:
	.size	return_parameter_convert__ret_u64_i16, .-return_parameter_convert__ret_u64_i16
	.align 4
	.type	return_parameter_convert__ret_u64_C, #function
	.proc	05
return_parameter_convert__ret_u64_C:
.LLFB18:
	save	%sp, -104, %sp
.LLCFI18:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE18:
	.size	return_parameter_convert__ret_u64_C, .-return_parameter_convert__ret_u64_C
	.align 4
	.type	return_parameter_convert__ret_u64_i64, #function
	.proc	05
return_parameter_convert__ret_u64_i64:
.LLFB19:
	save	%sp, -104, %sp
.LLCFI19:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE19:
	.size	return_parameter_convert__ret_u64_i64, .-return_parameter_convert__ret_u64_i64
	.align 4
	.type	return_parameter_convert__ret_u64_u8, #function
	.proc	05
return_parameter_convert__ret_u64_u8:
.LLFB20:
	save	%sp, -104, %sp
.LLCFI20:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE20:
	.size	return_parameter_convert__ret_u64_u8, .-return_parameter_convert__ret_u64_u8
	.align 4
	.type	return_parameter_convert__ret_u64_L, #function
	.proc	05
return_parameter_convert__ret_u64_L:
.LLFB21:
	save	%sp, -104, %sp
.LLCFI21:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE21:
	.size	return_parameter_convert__ret_u64_L, .-return_parameter_convert__ret_u64_L
	.align 4
	.type	return_parameter_convert__ret_u64_i8, #function
	.proc	05
return_parameter_convert__ret_u64_i8:
.LLFB22:
	save	%sp, -104, %sp
.LLCFI22:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE22:
	.size	return_parameter_convert__ret_u64_i8, .-return_parameter_convert__ret_u64_i8
	.align 4
	.type	return_parameter_convert__ret_u64_u32, #function
	.proc	05
return_parameter_convert__ret_u64_u32:
.LLFB23:
	save	%sp, -104, %sp
.LLCFI23:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE23:
	.size	return_parameter_convert__ret_u64_u32, .-return_parameter_convert__ret_u64_u32
	.align 4
	.type	return_parameter_convert__ret_LC_u16, #function
	.proc	05
return_parameter_convert__ret_LC_u16:
.LLFB24:
	save	%sp, -104, %sp
.LLCFI24:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	return_parameter_convert__ret_LC_u16, .-return_parameter_convert__ret_LC_u16
	.align 4
	.type	return_parameter_convert__ret_LC_u64, #function
	.proc	05
return_parameter_convert__ret_LC_u64:
.LLFB25:
	save	%sp, -104, %sp
.LLCFI25:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	return_parameter_convert__ret_LC_u64, .-return_parameter_convert__ret_LC_u64
	.align 4
	.type	return_parameter_convert__ret_LC_LC, #function
	.proc	05
return_parameter_convert__ret_LC_LC:
.LLFB26:
	save	%sp, -104, %sp
.LLCFI26:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	return_parameter_convert__ret_LC_LC, .-return_parameter_convert__ret_LC_LC
	.align 4
	.type	return_parameter_convert__ret_LC_i32, #function
	.proc	05
return_parameter_convert__ret_LC_i32:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	return_parameter_convert__ret_LC_i32, .-return_parameter_convert__ret_LC_i32
	.align 4
	.type	return_parameter_convert__ret_LC_I, #function
	.proc	05
return_parameter_convert__ret_LC_I:
.LLFB28:
	save	%sp, -104, %sp
.LLCFI28:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE28:
	.size	return_parameter_convert__ret_LC_I, .-return_parameter_convert__ret_LC_I
	.align 4
	.type	return_parameter_convert__ret_LC_i16, #function
	.proc	05
return_parameter_convert__ret_LC_i16:
.LLFB29:
	save	%sp, -104, %sp
.LLCFI29:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE29:
	.size	return_parameter_convert__ret_LC_i16, .-return_parameter_convert__ret_LC_i16
	.align 4
	.type	return_parameter_convert__ret_LC_C, #function
	.proc	05
return_parameter_convert__ret_LC_C:
.LLFB30:
	save	%sp, -104, %sp
.LLCFI30:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE30:
	.size	return_parameter_convert__ret_LC_C, .-return_parameter_convert__ret_LC_C
	.align 4
	.type	return_parameter_convert__ret_LC_i64, #function
	.proc	05
return_parameter_convert__ret_LC_i64:
.LLFB31:
	save	%sp, -104, %sp
.LLCFI31:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE31:
	.size	return_parameter_convert__ret_LC_i64, .-return_parameter_convert__ret_LC_i64
	.align 4
	.type	return_parameter_convert__ret_LC_u8, #function
	.proc	05
return_parameter_convert__ret_LC_u8:
.LLFB32:
	save	%sp, -104, %sp
.LLCFI32:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE32:
	.size	return_parameter_convert__ret_LC_u8, .-return_parameter_convert__ret_LC_u8
	.align 4
	.type	return_parameter_convert__ret_LC_L, #function
	.proc	05
return_parameter_convert__ret_LC_L:
.LLFB33:
	save	%sp, -104, %sp
.LLCFI33:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE33:
	.size	return_parameter_convert__ret_LC_L, .-return_parameter_convert__ret_LC_L
	.align 4
	.type	return_parameter_convert__ret_LC_i8, #function
	.proc	05
return_parameter_convert__ret_LC_i8:
.LLFB34:
	save	%sp, -104, %sp
.LLCFI34:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE34:
	.size	return_parameter_convert__ret_LC_i8, .-return_parameter_convert__ret_LC_i8
	.align 4
	.type	return_parameter_convert__ret_LC_u32, #function
	.proc	05
return_parameter_convert__ret_LC_u32:
.LLFB35:
	save	%sp, -104, %sp
.LLCFI35:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE35:
	.size	return_parameter_convert__ret_LC_u32, .-return_parameter_convert__ret_LC_u32
	.align 4
	.type	return_parameter_convert__ret_i32_u16, #function
	.proc	04
return_parameter_convert__ret_i32_u16:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE36:
	.size	return_parameter_convert__ret_i32_u16, .-return_parameter_convert__ret_i32_u16
	.align 4
	.type	return_parameter_convert__ret_i32_u64, #function
	.proc	05
return_parameter_convert__ret_i32_u64:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	return_parameter_convert__ret_i32_u64, .-return_parameter_convert__ret_i32_u64
	.align 4
	.type	return_parameter_convert__ret_i32_LC, #function
	.proc	05
return_parameter_convert__ret_i32_LC:
.LLFB38:
	save	%sp, -96, %sp
.LLCFI38:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	return_parameter_convert__ret_i32_LC, .-return_parameter_convert__ret_i32_LC
	.align 4
	.type	return_parameter_convert__ret_i32_i32, #function
	.proc	04
return_parameter_convert__ret_i32_i32:
.LLFB39:
	save	%sp, -96, %sp
.LLCFI39:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE39:
	.size	return_parameter_convert__ret_i32_i32, .-return_parameter_convert__ret_i32_i32
	.align 4
	.type	return_parameter_convert__ret_i32_I, #function
	.proc	04
return_parameter_convert__ret_i32_I:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	return_parameter_convert__ret_i32_I, .-return_parameter_convert__ret_i32_I
	.align 4
	.type	return_parameter_convert__ret_i32_i16, #function
	.proc	04
return_parameter_convert__ret_i32_i16:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	return_parameter_convert__ret_i32_i16, .-return_parameter_convert__ret_i32_i16
	.align 4
	.type	return_parameter_convert__ret_i32_C, #function
	.proc	04
return_parameter_convert__ret_i32_C:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	return_parameter_convert__ret_i32_C, .-return_parameter_convert__ret_i32_C
	.align 4
	.type	return_parameter_convert__ret_i32_i64, #function
	.proc	05
return_parameter_convert__ret_i32_i64:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE43:
	.size	return_parameter_convert__ret_i32_i64, .-return_parameter_convert__ret_i32_i64
	.align 4
	.type	return_parameter_convert__ret_i32_u8, #function
	.proc	04
return_parameter_convert__ret_i32_u8:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	return_parameter_convert__ret_i32_u8, .-return_parameter_convert__ret_i32_u8
	.align 4
	.type	return_parameter_convert__ret_i32_L, #function
	.proc	05
return_parameter_convert__ret_i32_L:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE45:
	.size	return_parameter_convert__ret_i32_L, .-return_parameter_convert__ret_i32_L
	.align 4
	.type	return_parameter_convert__ret_i32_i8, #function
	.proc	04
return_parameter_convert__ret_i32_i8:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	return_parameter_convert__ret_i32_i8, .-return_parameter_convert__ret_i32_i8
	.align 4
	.type	return_parameter_convert__ret_i32_u32, #function
	.proc	04
return_parameter_convert__ret_i32_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	return_parameter_convert__ret_i32_u32, .-return_parameter_convert__ret_i32_u32
	.align 4
	.type	return_parameter_convert__ret_I_u16, #function
	.proc	04
return_parameter_convert__ret_I_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE48:
	.size	return_parameter_convert__ret_I_u16, .-return_parameter_convert__ret_I_u16
	.align 4
	.type	return_parameter_convert__ret_I_u64, #function
	.proc	05
return_parameter_convert__ret_I_u64:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	return_parameter_convert__ret_I_u64, .-return_parameter_convert__ret_I_u64
	.align 4
	.type	return_parameter_convert__ret_I_LC, #function
	.proc	05
return_parameter_convert__ret_I_LC:
.LLFB50:
	save	%sp, -96, %sp
.LLCFI50:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	return_parameter_convert__ret_I_LC, .-return_parameter_convert__ret_I_LC
	.align 4
	.type	return_parameter_convert__ret_I_i32, #function
	.proc	04
return_parameter_convert__ret_I_i32:
.LLFB51:
	save	%sp, -96, %sp
.LLCFI51:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE51:
	.size	return_parameter_convert__ret_I_i32, .-return_parameter_convert__ret_I_i32
	.align 4
	.type	return_parameter_convert__ret_I_I, #function
	.proc	04
return_parameter_convert__ret_I_I:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE52:
	.size	return_parameter_convert__ret_I_I, .-return_parameter_convert__ret_I_I
	.align 4
	.type	return_parameter_convert__ret_I_i16, #function
	.proc	04
return_parameter_convert__ret_I_i16:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE53:
	.size	return_parameter_convert__ret_I_i16, .-return_parameter_convert__ret_I_i16
	.align 4
	.type	return_parameter_convert__ret_I_C, #function
	.proc	04
return_parameter_convert__ret_I_C:
.LLFB54:
	save	%sp, -96, %sp
.LLCFI54:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE54:
	.size	return_parameter_convert__ret_I_C, .-return_parameter_convert__ret_I_C
	.align 4
	.type	return_parameter_convert__ret_I_i64, #function
	.proc	05
return_parameter_convert__ret_I_i64:
.LLFB55:
	save	%sp, -96, %sp
.LLCFI55:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	return_parameter_convert__ret_I_i64, .-return_parameter_convert__ret_I_i64
	.align 4
	.type	return_parameter_convert__ret_I_u8, #function
	.proc	04
return_parameter_convert__ret_I_u8:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE56:
	.size	return_parameter_convert__ret_I_u8, .-return_parameter_convert__ret_I_u8
	.align 4
	.type	return_parameter_convert__ret_I_L, #function
	.proc	05
return_parameter_convert__ret_I_L:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	return_parameter_convert__ret_I_L, .-return_parameter_convert__ret_I_L
	.align 4
	.type	return_parameter_convert__ret_I_i8, #function
	.proc	04
return_parameter_convert__ret_I_i8:
.LLFB58:
	save	%sp, -96, %sp
.LLCFI58:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE58:
	.size	return_parameter_convert__ret_I_i8, .-return_parameter_convert__ret_I_i8
	.align 4
	.type	return_parameter_convert__ret_I_u32, #function
	.proc	04
return_parameter_convert__ret_I_u32:
.LLFB59:
	save	%sp, -96, %sp
.LLCFI59:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE59:
	.size	return_parameter_convert__ret_I_u32, .-return_parameter_convert__ret_I_u32
	.align 4
	.type	return_parameter_convert__ret_i16_u16, #function
	.proc	04
return_parameter_convert__ret_i16_u16:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE60:
	.size	return_parameter_convert__ret_i16_u16, .-return_parameter_convert__ret_i16_u16
	.align 4
	.type	return_parameter_convert__ret_i16_u64, #function
	.proc	05
return_parameter_convert__ret_i16_u64:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	return_parameter_convert__ret_i16_u64, .-return_parameter_convert__ret_i16_u64
	.align 4
	.type	return_parameter_convert__ret_i16_LC, #function
	.proc	05
return_parameter_convert__ret_i16_LC:
.LLFB62:
	save	%sp, -96, %sp
.LLCFI62:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	return_parameter_convert__ret_i16_LC, .-return_parameter_convert__ret_i16_LC
	.align 4
	.type	return_parameter_convert__ret_i16_i32, #function
	.proc	04
return_parameter_convert__ret_i16_i32:
.LLFB63:
	save	%sp, -96, %sp
.LLCFI63:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE63:
	.size	return_parameter_convert__ret_i16_i32, .-return_parameter_convert__ret_i16_i32
	.align 4
	.type	return_parameter_convert__ret_i16_I, #function
	.proc	04
return_parameter_convert__ret_i16_I:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE64:
	.size	return_parameter_convert__ret_i16_I, .-return_parameter_convert__ret_i16_I
	.align 4
	.type	return_parameter_convert__ret_i16_i16, #function
	.proc	04
return_parameter_convert__ret_i16_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE65:
	.size	return_parameter_convert__ret_i16_i16, .-return_parameter_convert__ret_i16_i16
	.align 4
	.type	return_parameter_convert__ret_i16_C, #function
	.proc	04
return_parameter_convert__ret_i16_C:
.LLFB66:
	save	%sp, -96, %sp
.LLCFI66:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE66:
	.size	return_parameter_convert__ret_i16_C, .-return_parameter_convert__ret_i16_C
	.align 4
	.type	return_parameter_convert__ret_i16_i64, #function
	.proc	05
return_parameter_convert__ret_i16_i64:
.LLFB67:
	save	%sp, -96, %sp
.LLCFI67:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	return_parameter_convert__ret_i16_i64, .-return_parameter_convert__ret_i16_i64
	.align 4
	.type	return_parameter_convert__ret_i16_u8, #function
	.proc	04
return_parameter_convert__ret_i16_u8:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE68:
	.size	return_parameter_convert__ret_i16_u8, .-return_parameter_convert__ret_i16_u8
	.align 4
	.type	return_parameter_convert__ret_i16_L, #function
	.proc	05
return_parameter_convert__ret_i16_L:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	return_parameter_convert__ret_i16_L, .-return_parameter_convert__ret_i16_L
	.align 4
	.type	return_parameter_convert__ret_i16_i8, #function
	.proc	04
return_parameter_convert__ret_i16_i8:
.LLFB70:
	save	%sp, -96, %sp
.LLCFI70:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE70:
	.size	return_parameter_convert__ret_i16_i8, .-return_parameter_convert__ret_i16_i8
	.align 4
	.type	return_parameter_convert__ret_i16_u32, #function
	.proc	04
return_parameter_convert__ret_i16_u32:
.LLFB71:
	save	%sp, -96, %sp
.LLCFI71:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE71:
	.size	return_parameter_convert__ret_i16_u32, .-return_parameter_convert__ret_i16_u32
	.align 4
	.type	return_parameter_convert__ret_C_u16, #function
	.proc	04
return_parameter_convert__ret_C_u16:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE72:
	.size	return_parameter_convert__ret_C_u16, .-return_parameter_convert__ret_C_u16
	.align 4
	.type	return_parameter_convert__ret_C_u64, #function
	.proc	05
return_parameter_convert__ret_C_u64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	return_parameter_convert__ret_C_u64, .-return_parameter_convert__ret_C_u64
	.align 4
	.type	return_parameter_convert__ret_C_LC, #function
	.proc	05
return_parameter_convert__ret_C_LC:
.LLFB74:
	save	%sp, -96, %sp
.LLCFI74:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	return_parameter_convert__ret_C_LC, .-return_parameter_convert__ret_C_LC
	.align 4
	.type	return_parameter_convert__ret_C_i32, #function
	.proc	04
return_parameter_convert__ret_C_i32:
.LLFB75:
	save	%sp, -96, %sp
.LLCFI75:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE75:
	.size	return_parameter_convert__ret_C_i32, .-return_parameter_convert__ret_C_i32
	.align 4
	.type	return_parameter_convert__ret_C_I, #function
	.proc	04
return_parameter_convert__ret_C_I:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE76:
	.size	return_parameter_convert__ret_C_I, .-return_parameter_convert__ret_C_I
	.align 4
	.type	return_parameter_convert__ret_C_i16, #function
	.proc	04
return_parameter_convert__ret_C_i16:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE77:
	.size	return_parameter_convert__ret_C_i16, .-return_parameter_convert__ret_C_i16
	.align 4
	.type	return_parameter_convert__ret_C_C, #function
	.proc	04
return_parameter_convert__ret_C_C:
.LLFB78:
	save	%sp, -96, %sp
.LLCFI78:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE78:
	.size	return_parameter_convert__ret_C_C, .-return_parameter_convert__ret_C_C
	.align 4
	.type	return_parameter_convert__ret_C_i64, #function
	.proc	05
return_parameter_convert__ret_C_i64:
.LLFB79:
	save	%sp, -96, %sp
.LLCFI79:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	return_parameter_convert__ret_C_i64, .-return_parameter_convert__ret_C_i64
	.align 4
	.type	return_parameter_convert__ret_C_u8, #function
	.proc	04
return_parameter_convert__ret_C_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE80:
	.size	return_parameter_convert__ret_C_u8, .-return_parameter_convert__ret_C_u8
	.align 4
	.type	return_parameter_convert__ret_C_L, #function
	.proc	05
return_parameter_convert__ret_C_L:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	return_parameter_convert__ret_C_L, .-return_parameter_convert__ret_C_L
	.align 4
	.type	return_parameter_convert__ret_C_i8, #function
	.proc	04
return_parameter_convert__ret_C_i8:
.LLFB82:
	save	%sp, -96, %sp
.LLCFI82:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE82:
	.size	return_parameter_convert__ret_C_i8, .-return_parameter_convert__ret_C_i8
	.align 4
	.type	return_parameter_convert__ret_C_u32, #function
	.proc	04
return_parameter_convert__ret_C_u32:
.LLFB83:
	save	%sp, -96, %sp
.LLCFI83:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE83:
	.size	return_parameter_convert__ret_C_u32, .-return_parameter_convert__ret_C_u32
	.align 4
	.type	return_parameter_convert__ret_i64_u16, #function
	.proc	05
return_parameter_convert__ret_i64_u16:
.LLFB84:
	save	%sp, -104, %sp
.LLCFI84:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	return_parameter_convert__ret_i64_u16, .-return_parameter_convert__ret_i64_u16
	.align 4
	.type	return_parameter_convert__ret_i64_u64, #function
	.proc	05
return_parameter_convert__ret_i64_u64:
.LLFB85:
	save	%sp, -104, %sp
.LLCFI85:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	return_parameter_convert__ret_i64_u64, .-return_parameter_convert__ret_i64_u64
	.align 4
	.type	return_parameter_convert__ret_i64_LC, #function
	.proc	05
return_parameter_convert__ret_i64_LC:
.LLFB86:
	save	%sp, -104, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	return_parameter_convert__ret_i64_LC, .-return_parameter_convert__ret_i64_LC
	.align 4
	.type	return_parameter_convert__ret_i64_i32, #function
	.proc	05
return_parameter_convert__ret_i64_i32:
.LLFB87:
	save	%sp, -104, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	return_parameter_convert__ret_i64_i32, .-return_parameter_convert__ret_i64_i32
	.align 4
	.type	return_parameter_convert__ret_i64_I, #function
	.proc	05
return_parameter_convert__ret_i64_I:
.LLFB88:
	save	%sp, -104, %sp
.LLCFI88:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	return_parameter_convert__ret_i64_I, .-return_parameter_convert__ret_i64_I
	.align 4
	.type	return_parameter_convert__ret_i64_i16, #function
	.proc	05
return_parameter_convert__ret_i64_i16:
.LLFB89:
	save	%sp, -104, %sp
.LLCFI89:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	return_parameter_convert__ret_i64_i16, .-return_parameter_convert__ret_i64_i16
	.align 4
	.type	return_parameter_convert__ret_i64_C, #function
	.proc	05
return_parameter_convert__ret_i64_C:
.LLFB90:
	save	%sp, -104, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	return_parameter_convert__ret_i64_C, .-return_parameter_convert__ret_i64_C
	.align 4
	.type	return_parameter_convert__ret_i64_i64, #function
	.proc	05
return_parameter_convert__ret_i64_i64:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	return_parameter_convert__ret_i64_i64, .-return_parameter_convert__ret_i64_i64
	.align 4
	.type	return_parameter_convert__ret_i64_u8, #function
	.proc	05
return_parameter_convert__ret_i64_u8:
.LLFB92:
	save	%sp, -104, %sp
.LLCFI92:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	return_parameter_convert__ret_i64_u8, .-return_parameter_convert__ret_i64_u8
	.align 4
	.type	return_parameter_convert__ret_i64_L, #function
	.proc	05
return_parameter_convert__ret_i64_L:
.LLFB93:
	save	%sp, -104, %sp
.LLCFI93:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	return_parameter_convert__ret_i64_L, .-return_parameter_convert__ret_i64_L
	.align 4
	.type	return_parameter_convert__ret_i64_i8, #function
	.proc	05
return_parameter_convert__ret_i64_i8:
.LLFB94:
	save	%sp, -104, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	return_parameter_convert__ret_i64_i8, .-return_parameter_convert__ret_i64_i8
	.align 4
	.type	return_parameter_convert__ret_i64_u32, #function
	.proc	05
return_parameter_convert__ret_i64_u32:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	return_parameter_convert__ret_i64_u32, .-return_parameter_convert__ret_i64_u32
	.align 4
	.type	return_parameter_convert__ret_u8_u16, #function
	.proc	04
return_parameter_convert__ret_u8_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE96:
	.size	return_parameter_convert__ret_u8_u16, .-return_parameter_convert__ret_u8_u16
	.align 4
	.type	return_parameter_convert__ret_u8_u64, #function
	.proc	05
return_parameter_convert__ret_u8_u64:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE97:
	.size	return_parameter_convert__ret_u8_u64, .-return_parameter_convert__ret_u8_u64
	.align 4
	.type	return_parameter_convert__ret_u8_LC, #function
	.proc	05
return_parameter_convert__ret_u8_LC:
.LLFB98:
	save	%sp, -96, %sp
.LLCFI98:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE98:
	.size	return_parameter_convert__ret_u8_LC, .-return_parameter_convert__ret_u8_LC
	.align 4
	.type	return_parameter_convert__ret_u8_i32, #function
	.proc	04
return_parameter_convert__ret_u8_i32:
.LLFB99:
	save	%sp, -96, %sp
.LLCFI99:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE99:
	.size	return_parameter_convert__ret_u8_i32, .-return_parameter_convert__ret_u8_i32
	.align 4
	.type	return_parameter_convert__ret_u8_I, #function
	.proc	04
return_parameter_convert__ret_u8_I:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE100:
	.size	return_parameter_convert__ret_u8_I, .-return_parameter_convert__ret_u8_I
	.align 4
	.type	return_parameter_convert__ret_u8_i16, #function
	.proc	04
return_parameter_convert__ret_u8_i16:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE101:
	.size	return_parameter_convert__ret_u8_i16, .-return_parameter_convert__ret_u8_i16
	.align 4
	.type	return_parameter_convert__ret_u8_C, #function
	.proc	04
return_parameter_convert__ret_u8_C:
.LLFB102:
	save	%sp, -96, %sp
.LLCFI102:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE102:
	.size	return_parameter_convert__ret_u8_C, .-return_parameter_convert__ret_u8_C
	.align 4
	.type	return_parameter_convert__ret_u8_i64, #function
	.proc	05
return_parameter_convert__ret_u8_i64:
.LLFB103:
	save	%sp, -96, %sp
.LLCFI103:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE103:
	.size	return_parameter_convert__ret_u8_i64, .-return_parameter_convert__ret_u8_i64
	.align 4
	.type	return_parameter_convert__ret_u8_u8, #function
	.proc	04
return_parameter_convert__ret_u8_u8:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE104:
	.size	return_parameter_convert__ret_u8_u8, .-return_parameter_convert__ret_u8_u8
	.align 4
	.type	return_parameter_convert__ret_u8_L, #function
	.proc	05
return_parameter_convert__ret_u8_L:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE105:
	.size	return_parameter_convert__ret_u8_L, .-return_parameter_convert__ret_u8_L
	.align 4
	.type	return_parameter_convert__ret_u8_i8, #function
	.proc	04
return_parameter_convert__ret_u8_i8:
.LLFB106:
	save	%sp, -96, %sp
.LLCFI106:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE106:
	.size	return_parameter_convert__ret_u8_i8, .-return_parameter_convert__ret_u8_i8
	.align 4
	.type	return_parameter_convert__ret_u8_u32, #function
	.proc	04
return_parameter_convert__ret_u8_u32:
.LLFB107:
	save	%sp, -96, %sp
.LLCFI107:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE107:
	.size	return_parameter_convert__ret_u8_u32, .-return_parameter_convert__ret_u8_u32
	.align 4
	.type	return_parameter_convert__ret_L_u16, #function
	.proc	05
return_parameter_convert__ret_L_u16:
.LLFB108:
	save	%sp, -104, %sp
.LLCFI108:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE108:
	.size	return_parameter_convert__ret_L_u16, .-return_parameter_convert__ret_L_u16
	.align 4
	.type	return_parameter_convert__ret_L_u64, #function
	.proc	05
return_parameter_convert__ret_L_u64:
.LLFB109:
	save	%sp, -104, %sp
.LLCFI109:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE109:
	.size	return_parameter_convert__ret_L_u64, .-return_parameter_convert__ret_L_u64
	.align 4
	.type	return_parameter_convert__ret_L_LC, #function
	.proc	05
return_parameter_convert__ret_L_LC:
.LLFB110:
	save	%sp, -104, %sp
.LLCFI110:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE110:
	.size	return_parameter_convert__ret_L_LC, .-return_parameter_convert__ret_L_LC
	.align 4
	.type	return_parameter_convert__ret_L_i32, #function
	.proc	05
return_parameter_convert__ret_L_i32:
.LLFB111:
	save	%sp, -104, %sp
.LLCFI111:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE111:
	.size	return_parameter_convert__ret_L_i32, .-return_parameter_convert__ret_L_i32
	.align 4
	.type	return_parameter_convert__ret_L_I, #function
	.proc	05
return_parameter_convert__ret_L_I:
.LLFB112:
	save	%sp, -104, %sp
.LLCFI112:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE112:
	.size	return_parameter_convert__ret_L_I, .-return_parameter_convert__ret_L_I
	.align 4
	.type	return_parameter_convert__ret_L_i16, #function
	.proc	05
return_parameter_convert__ret_L_i16:
.LLFB113:
	save	%sp, -104, %sp
.LLCFI113:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE113:
	.size	return_parameter_convert__ret_L_i16, .-return_parameter_convert__ret_L_i16
	.align 4
	.type	return_parameter_convert__ret_L_C, #function
	.proc	05
return_parameter_convert__ret_L_C:
.LLFB114:
	save	%sp, -104, %sp
.LLCFI114:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE114:
	.size	return_parameter_convert__ret_L_C, .-return_parameter_convert__ret_L_C
	.align 4
	.type	return_parameter_convert__ret_L_i64, #function
	.proc	05
return_parameter_convert__ret_L_i64:
.LLFB115:
	save	%sp, -104, %sp
.LLCFI115:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE115:
	.size	return_parameter_convert__ret_L_i64, .-return_parameter_convert__ret_L_i64
	.align 4
	.type	return_parameter_convert__ret_L_u8, #function
	.proc	05
return_parameter_convert__ret_L_u8:
.LLFB116:
	save	%sp, -104, %sp
.LLCFI116:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE116:
	.size	return_parameter_convert__ret_L_u8, .-return_parameter_convert__ret_L_u8
	.align 4
	.type	return_parameter_convert__ret_L_L, #function
	.proc	05
return_parameter_convert__ret_L_L:
.LLFB117:
	save	%sp, -104, %sp
.LLCFI117:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE117:
	.size	return_parameter_convert__ret_L_L, .-return_parameter_convert__ret_L_L
	.align 4
	.type	return_parameter_convert__ret_L_i8, #function
	.proc	05
return_parameter_convert__ret_L_i8:
.LLFB118:
	save	%sp, -104, %sp
.LLCFI118:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE118:
	.size	return_parameter_convert__ret_L_i8, .-return_parameter_convert__ret_L_i8
	.align 4
	.type	return_parameter_convert__ret_L_u32, #function
	.proc	05
return_parameter_convert__ret_L_u32:
.LLFB119:
	save	%sp, -104, %sp
.LLCFI119:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE119:
	.size	return_parameter_convert__ret_L_u32, .-return_parameter_convert__ret_L_u32
	.align 4
	.type	return_parameter_convert__ret_i8_u16, #function
	.proc	04
return_parameter_convert__ret_i8_u16:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE120:
	.size	return_parameter_convert__ret_i8_u16, .-return_parameter_convert__ret_i8_u16
	.align 4
	.type	return_parameter_convert__ret_i8_u64, #function
	.proc	05
return_parameter_convert__ret_i8_u64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE121:
	.size	return_parameter_convert__ret_i8_u64, .-return_parameter_convert__ret_i8_u64
	.align 4
	.type	return_parameter_convert__ret_i8_LC, #function
	.proc	05
return_parameter_convert__ret_i8_LC:
.LLFB122:
	save	%sp, -96, %sp
.LLCFI122:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE122:
	.size	return_parameter_convert__ret_i8_LC, .-return_parameter_convert__ret_i8_LC
	.align 4
	.type	return_parameter_convert__ret_i8_i32, #function
	.proc	04
return_parameter_convert__ret_i8_i32:
.LLFB123:
	save	%sp, -96, %sp
.LLCFI123:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE123:
	.size	return_parameter_convert__ret_i8_i32, .-return_parameter_convert__ret_i8_i32
	.align 4
	.type	return_parameter_convert__ret_i8_I, #function
	.proc	04
return_parameter_convert__ret_i8_I:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE124:
	.size	return_parameter_convert__ret_i8_I, .-return_parameter_convert__ret_i8_I
	.align 4
	.type	return_parameter_convert__ret_i8_i16, #function
	.proc	04
return_parameter_convert__ret_i8_i16:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE125:
	.size	return_parameter_convert__ret_i8_i16, .-return_parameter_convert__ret_i8_i16
	.align 4
	.type	return_parameter_convert__ret_i8_C, #function
	.proc	04
return_parameter_convert__ret_i8_C:
.LLFB126:
	save	%sp, -96, %sp
.LLCFI126:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE126:
	.size	return_parameter_convert__ret_i8_C, .-return_parameter_convert__ret_i8_C
	.align 4
	.type	return_parameter_convert__ret_i8_i64, #function
	.proc	05
return_parameter_convert__ret_i8_i64:
.LLFB127:
	save	%sp, -96, %sp
.LLCFI127:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE127:
	.size	return_parameter_convert__ret_i8_i64, .-return_parameter_convert__ret_i8_i64
	.align 4
	.type	return_parameter_convert__ret_i8_u8, #function
	.proc	04
return_parameter_convert__ret_i8_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE128:
	.size	return_parameter_convert__ret_i8_u8, .-return_parameter_convert__ret_i8_u8
	.align 4
	.type	return_parameter_convert__ret_i8_L, #function
	.proc	05
return_parameter_convert__ret_i8_L:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE129:
	.size	return_parameter_convert__ret_i8_L, .-return_parameter_convert__ret_i8_L
	.align 4
	.type	return_parameter_convert__ret_i8_i8, #function
	.proc	04
return_parameter_convert__ret_i8_i8:
.LLFB130:
	save	%sp, -96, %sp
.LLCFI130:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE130:
	.size	return_parameter_convert__ret_i8_i8, .-return_parameter_convert__ret_i8_i8
	.align 4
	.type	return_parameter_convert__ret_i8_u32, #function
	.proc	04
return_parameter_convert__ret_i8_u32:
.LLFB131:
	save	%sp, -96, %sp
.LLCFI131:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE131:
	.size	return_parameter_convert__ret_i8_u32, .-return_parameter_convert__ret_i8_u32
	.align 4
	.type	return_parameter_convert__ret_u32_u16, #function
	.proc	04
return_parameter_convert__ret_u32_u16:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE132:
	.size	return_parameter_convert__ret_u32_u16, .-return_parameter_convert__ret_u32_u16
	.align 4
	.type	return_parameter_convert__ret_u32_u64, #function
	.proc	05
return_parameter_convert__ret_u32_u64:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE133:
	.size	return_parameter_convert__ret_u32_u64, .-return_parameter_convert__ret_u32_u64
	.align 4
	.type	return_parameter_convert__ret_u32_LC, #function
	.proc	05
return_parameter_convert__ret_u32_LC:
.LLFB134:
	save	%sp, -96, %sp
.LLCFI134:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE134:
	.size	return_parameter_convert__ret_u32_LC, .-return_parameter_convert__ret_u32_LC
	.align 4
	.type	return_parameter_convert__ret_u32_i32, #function
	.proc	04
return_parameter_convert__ret_u32_i32:
.LLFB135:
	save	%sp, -96, %sp
.LLCFI135:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE135:
	.size	return_parameter_convert__ret_u32_i32, .-return_parameter_convert__ret_u32_i32
	.align 4
	.type	return_parameter_convert__ret_u32_I, #function
	.proc	04
return_parameter_convert__ret_u32_I:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE136:
	.size	return_parameter_convert__ret_u32_I, .-return_parameter_convert__ret_u32_I
	.align 4
	.type	return_parameter_convert__ret_u32_i16, #function
	.proc	04
return_parameter_convert__ret_u32_i16:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE137:
	.size	return_parameter_convert__ret_u32_i16, .-return_parameter_convert__ret_u32_i16
	.align 4
	.type	return_parameter_convert__ret_u32_C, #function
	.proc	04
return_parameter_convert__ret_u32_C:
.LLFB138:
	save	%sp, -96, %sp
.LLCFI138:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE138:
	.size	return_parameter_convert__ret_u32_C, .-return_parameter_convert__ret_u32_C
	.align 4
	.type	return_parameter_convert__ret_u32_i64, #function
	.proc	05
return_parameter_convert__ret_u32_i64:
.LLFB139:
	save	%sp, -96, %sp
.LLCFI139:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE139:
	.size	return_parameter_convert__ret_u32_i64, .-return_parameter_convert__ret_u32_i64
	.align 4
	.type	return_parameter_convert__ret_u32_u8, #function
	.proc	04
return_parameter_convert__ret_u32_u8:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE140:
	.size	return_parameter_convert__ret_u32_u8, .-return_parameter_convert__ret_u32_u8
	.align 4
	.type	return_parameter_convert__ret_u32_L, #function
	.proc	05
return_parameter_convert__ret_u32_L:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	st	%i0, [%fp+68]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE141:
	.size	return_parameter_convert__ret_u32_L, .-return_parameter_convert__ret_u32_L
	.align 4
	.type	return_parameter_convert__ret_u32_i8, #function
	.proc	04
return_parameter_convert__ret_u32_i8:
.LLFB142:
	save	%sp, -96, %sp
.LLCFI142:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE142:
	.size	return_parameter_convert__ret_u32_i8, .-return_parameter_convert__ret_u32_i8
	.align 4
	.type	return_parameter_convert__ret_u32_u32, #function
	.proc	04
return_parameter_convert__ret_u32_u32:
.LLFB143:
	save	%sp, -96, %sp
.LLCFI143:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE143:
	.size	return_parameter_convert__ret_u32_u32, .-return_parameter_convert__ret_u32_u32
	.align 4
	.global return_parameter_convert_M3
	.type	return_parameter_convert_M3, #function
	.proc	0120
return_parameter_convert_M3:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	st	%i0, [%fp+68]
	sethi	%hi(MM_return_parameter_convert), %g1
	or	%g1, %lo(MM_return_parameter_convert), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	return_parameter_convert_M3, .-return_parameter_convert_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 2744
L_1:
	.ascii	"return_parameter_convert_M3"
	.skip 1
	.ascii	"ret_u32_u32"
	.skip 1
	.ascii	"ret_u32_i8"
	.skip 1
	.ascii	"ret_u32_L"
	.skip 1
	.ascii	"ret_u32_u8"
	.skip 1
	.ascii	"ret_u32_i64"
	.skip 1
	.ascii	"ret_u32_C"
	.skip 1
	.ascii	"ret_u32_i16"
	.skip 1
	.ascii	"ret_u32_I"
	.skip 1
	.ascii	"ret_u32_i32"
	.skip 1
	.ascii	"ret_u32_LC"
	.skip 1
	.ascii	"ret_u32_u64"
	.skip 1
	.ascii	"ret_u32_u16"
	.skip 1
	.ascii	"ret_i8_u32"
	.skip 1
	.ascii	"ret_i8_i8"
	.skip 1
	.ascii	"ret_i8_L"
	.skip 1
	.ascii	"ret_i8_u8"
	.skip 1
	.ascii	"ret_i8_i64"
	.skip 1
	.ascii	"ret_i8_C"
	.skip 1
	.ascii	"ret_i8_i16"
	.skip 1
	.ascii	"ret_i8_I"
	.skip 1
	.ascii	"ret_i8_i32"
	.skip 1
	.ascii	"ret_i8_LC"
	.skip 1
	.ascii	"ret_i8_u64"
	.skip 1
	.ascii	"ret_i8_u16"
	.skip 1
	.ascii	"ret_L_u32"
	.skip 1
	.ascii	"ret_L_i8"
	.skip 1
	.ascii	"ret_L_L"
	.skip 1
	.ascii	"ret_L_u8"
	.skip 1
	.ascii	"ret_L_i64"
	.skip 1
	.ascii	"ret_L_C"
	.skip 1
	.ascii	"ret_L_i16"
	.skip 1
	.ascii	"ret_L_I"
	.skip 1
	.ascii	"ret_L_i32"
	.skip 1
	.ascii	"ret_L_LC"
	.skip 1
	.ascii	"ret_L_u64"
	.skip 1
	.ascii	"ret_L_u16"
	.skip 1
	.ascii	"ret_u8_u32"
	.skip 1
	.ascii	"ret_u8_i8"
	.skip 1
	.ascii	"ret_u8_L"
	.skip 1
	.ascii	"ret_u8_u8"
	.skip 1
	.ascii	"ret_u8_i64"
	.skip 1
	.ascii	"ret_u8_C"
	.skip 1
	.ascii	"ret_u8_i16"
	.skip 1
	.ascii	"ret_u8_I"
	.skip 1
	.ascii	"ret_u8_i32"
	.skip 1
	.ascii	"ret_u8_LC"
	.skip 1
	.ascii	"ret_u8_u64"
	.skip 1
	.ascii	"ret_u8_u16"
	.skip 1
	.ascii	"ret_i64_u32"
	.skip 1
	.ascii	"ret_i64_i8"
	.skip 1
	.ascii	"ret_i64_L"
	.skip 1
	.ascii	"ret_i64_u8"
	.skip 1
	.ascii	"ret_i64_i64"
	.skip 1
	.ascii	"ret_i64_C"
	.skip 1
	.ascii	"ret_i64_i16"
	.skip 1
	.ascii	"ret_i64_I"
	.skip 1
	.ascii	"ret_i64_i32"
	.skip 1
	.ascii	"ret_i64_LC"
	.skip 1
	.ascii	"ret_i64_u64"
	.skip 1
	.ascii	"ret_i64_u16"
	.skip 1
	.ascii	"ret_C_u32"
	.skip 1
	.ascii	"ret_C_i8"
	.skip 1
	.ascii	"ret_C_L"
	.skip 1
	.ascii	"ret_C_u8"
	.skip 1
	.ascii	"ret_C_i64"
	.skip 1
	.ascii	"ret_C_C"
	.skip 1
	.ascii	"ret_C_i16"
	.skip 1
	.ascii	"ret_C_I"
	.skip 1
	.ascii	"ret_C_i32"
	.skip 1
	.ascii	"ret_C_LC"
	.skip 1
	.ascii	"ret_C_u64"
	.skip 1
	.ascii	"ret_C_u16"
	.skip 1
	.ascii	"ret_i16_u32"
	.skip 1
	.ascii	"ret_i16_i8"
	.skip 1
	.ascii	"ret_i16_L"
	.skip 1
	.ascii	"ret_i16_u8"
	.skip 1
	.ascii	"ret_i16_i64"
	.skip 1
	.ascii	"ret_i16_C"
	.skip 1
	.ascii	"ret_i16_i16"
	.skip 1
	.ascii	"ret_i16_I"
	.skip 1
	.ascii	"ret_i16_i32"
	.skip 1
	.ascii	"ret_i16_LC"
	.skip 1
	.ascii	"ret_i16_u64"
	.skip 1
	.ascii	"ret_i16_u16"
	.skip 1
	.ascii	"ret_I_u32"
	.skip 1
	.ascii	"ret_I_i8"
	.skip 1
	.ascii	"ret_I_L"
	.skip 1
	.ascii	"ret_I_u8"
	.skip 1
	.ascii	"ret_I_i64"
	.skip 1
	.ascii	"ret_I_C"
	.skip 1
	.ascii	"ret_I_i16"
	.skip 1
	.ascii	"ret_I_I"
	.skip 1
	.ascii	"ret_I_i32"
	.skip 1
	.ascii	"ret_I_LC"
	.skip 1
	.ascii	"ret_I_u64"
	.skip 1
	.ascii	"ret_I_u16"
	.skip 1
	.ascii	"ret_i32_u32"
	.skip 1
	.ascii	"ret_i32_i8"
	.skip 1
	.ascii	"ret_i32_L"
	.skip 1
	.ascii	"ret_i32_u8"
	.skip 1
	.ascii	"ret_i32_i64"
	.skip 1
	.ascii	"ret_i32_C"
	.skip 1
	.ascii	"ret_i32_i16"
	.skip 1
	.ascii	"ret_i32_I"
	.skip 1
	.ascii	"ret_i32_i32"
	.skip 1
	.ascii	"ret_i32_LC"
	.skip 1
	.ascii	"ret_i32_u64"
	.skip 1
	.ascii	"ret_i32_u16"
	.skip 1
	.ascii	"ret_LC_u32"
	.skip 1
	.ascii	"ret_LC_i8"
	.skip 1
	.ascii	"ret_LC_L"
	.skip 1
	.ascii	"ret_LC_u8"
	.skip 1
	.ascii	"ret_LC_i64"
	.skip 1
	.ascii	"ret_LC_C"
	.skip 1
	.ascii	"ret_LC_i16"
	.skip 1
	.ascii	"ret_LC_I"
	.skip 1
	.ascii	"ret_LC_i32"
	.skip 1
	.ascii	"ret_LC_LC"
	.skip 1
	.ascii	"ret_LC_u64"
	.skip 1
	.ascii	"ret_LC_u16"
	.skip 1
	.ascii	"ret_u64_u32"
	.skip 1
	.ascii	"ret_u64_i8"
	.skip 1
	.ascii	"ret_u64_L"
	.skip 1
	.ascii	"ret_u64_u8"
	.skip 1
	.ascii	"ret_u64_i64"
	.skip 1
	.ascii	"ret_u64_C"
	.skip 1
	.ascii	"ret_u64_i16"
	.skip 1
	.ascii	"ret_u64_I"
	.skip 1
	.ascii	"ret_u64_i32"
	.skip 1
	.ascii	"ret_u64_LC"
	.skip 1
	.ascii	"ret_u64_u64"
	.skip 1
	.ascii	"ret_u64_u16"
	.skip 1
	.ascii	"ret_u16_u32"
	.skip 1
	.ascii	"ret_u16_i8"
	.skip 1
	.ascii	"ret_u16_L"
	.skip 1
	.ascii	"ret_u16_u8"
	.skip 1
	.ascii	"ret_u16_i64"
	.skip 1
	.ascii	"ret_u16_C"
	.skip 1
	.ascii	"ret_u16_i16"
	.skip 1
	.ascii	"ret_u16_I"
	.skip 1
	.ascii	"ret_u16_i32"
	.skip 1
	.ascii	"ret_u16_LC"
	.skip 1
	.ascii	"ret_u16_u64"
	.skip 1
	.ascii	"ret_u16_u16"
	.skip 1
	.long	return_parameter_convert_M3
	.long	L_1
	.long	return_parameter_convert__ret_u32_u32
	.long	L_1+28
	.long	return_parameter_convert__ret_u32_i8
	.long	L_1+40
	.long	return_parameter_convert__ret_u32_L
	.long	L_1+51
	.long	return_parameter_convert__ret_u32_u8
	.long	L_1+61
	.long	return_parameter_convert__ret_u32_i64
	.long	L_1+72
	.long	return_parameter_convert__ret_u32_C
	.long	L_1+84
	.long	return_parameter_convert__ret_u32_i16
	.long	L_1+94
	.long	return_parameter_convert__ret_u32_I
	.long	L_1+106
	.long	return_parameter_convert__ret_u32_i32
	.long	L_1+116
	.long	return_parameter_convert__ret_u32_LC
	.long	L_1+128
	.long	return_parameter_convert__ret_u32_u64
	.long	L_1+139
	.long	return_parameter_convert__ret_u32_u16
	.long	L_1+151
	.long	return_parameter_convert__ret_i8_u32
	.long	L_1+163
	.long	return_parameter_convert__ret_i8_i8
	.long	L_1+174
	.long	return_parameter_convert__ret_i8_L
	.long	L_1+184
	.long	return_parameter_convert__ret_i8_u8
	.long	L_1+193
	.long	return_parameter_convert__ret_i8_i64
	.long	L_1+203
	.long	return_parameter_convert__ret_i8_C
	.long	L_1+214
	.long	return_parameter_convert__ret_i8_i16
	.long	L_1+223
	.long	return_parameter_convert__ret_i8_I
	.long	L_1+234
	.long	return_parameter_convert__ret_i8_i32
	.long	L_1+243
	.long	return_parameter_convert__ret_i8_LC
	.long	L_1+254
	.long	return_parameter_convert__ret_i8_u64
	.long	L_1+264
	.long	return_parameter_convert__ret_i8_u16
	.long	L_1+275
	.long	return_parameter_convert__ret_L_u32
	.long	L_1+286
	.long	return_parameter_convert__ret_L_i8
	.long	L_1+296
	.long	return_parameter_convert__ret_L_L
	.long	L_1+305
	.long	return_parameter_convert__ret_L_u8
	.long	L_1+313
	.long	return_parameter_convert__ret_L_i64
	.long	L_1+322
	.long	return_parameter_convert__ret_L_C
	.long	L_1+332
	.long	return_parameter_convert__ret_L_i16
	.long	L_1+340
	.long	return_parameter_convert__ret_L_I
	.long	L_1+350
	.long	return_parameter_convert__ret_L_i32
	.long	L_1+358
	.long	return_parameter_convert__ret_L_LC
	.long	L_1+368
	.long	return_parameter_convert__ret_L_u64
	.long	L_1+377
	.long	return_parameter_convert__ret_L_u16
	.long	L_1+387
	.long	return_parameter_convert__ret_u8_u32
	.long	L_1+397
	.long	return_parameter_convert__ret_u8_i8
	.long	L_1+408
	.long	return_parameter_convert__ret_u8_L
	.long	L_1+418
	.long	return_parameter_convert__ret_u8_u8
	.long	L_1+427
	.long	return_parameter_convert__ret_u8_i64
	.long	L_1+437
	.long	return_parameter_convert__ret_u8_C
	.long	L_1+448
	.long	return_parameter_convert__ret_u8_i16
	.long	L_1+457
	.long	return_parameter_convert__ret_u8_I
	.long	L_1+468
	.long	return_parameter_convert__ret_u8_i32
	.long	L_1+477
	.long	return_parameter_convert__ret_u8_LC
	.long	L_1+488
	.long	return_parameter_convert__ret_u8_u64
	.long	L_1+498
	.long	return_parameter_convert__ret_u8_u16
	.long	L_1+509
	.long	return_parameter_convert__ret_i64_u32
	.long	L_1+520
	.long	return_parameter_convert__ret_i64_i8
	.long	L_1+532
	.long	return_parameter_convert__ret_i64_L
	.long	L_1+543
	.long	return_parameter_convert__ret_i64_u8
	.long	L_1+553
	.long	return_parameter_convert__ret_i64_i64
	.long	L_1+564
	.long	return_parameter_convert__ret_i64_C
	.long	L_1+576
	.long	return_parameter_convert__ret_i64_i16
	.long	L_1+586
	.long	return_parameter_convert__ret_i64_I
	.long	L_1+598
	.long	return_parameter_convert__ret_i64_i32
	.long	L_1+608
	.long	return_parameter_convert__ret_i64_LC
	.long	L_1+620
	.long	return_parameter_convert__ret_i64_u64
	.long	L_1+631
	.long	return_parameter_convert__ret_i64_u16
	.long	L_1+643
	.long	return_parameter_convert__ret_C_u32
	.long	L_1+655
	.long	return_parameter_convert__ret_C_i8
	.long	L_1+665
	.long	return_parameter_convert__ret_C_L
	.long	L_1+674
	.long	return_parameter_convert__ret_C_u8
	.long	L_1+682
	.long	return_parameter_convert__ret_C_i64
	.long	L_1+691
	.long	return_parameter_convert__ret_C_C
	.long	L_1+701
	.long	return_parameter_convert__ret_C_i16
	.long	L_1+709
	.long	return_parameter_convert__ret_C_I
	.long	L_1+719
	.long	return_parameter_convert__ret_C_i32
	.long	L_1+727
	.long	return_parameter_convert__ret_C_LC
	.long	L_1+737
	.long	return_parameter_convert__ret_C_u64
	.long	L_1+746
	.long	return_parameter_convert__ret_C_u16
	.long	L_1+756
	.long	return_parameter_convert__ret_i16_u32
	.long	L_1+766
	.long	return_parameter_convert__ret_i16_i8
	.long	L_1+778
	.long	return_parameter_convert__ret_i16_L
	.long	L_1+789
	.long	return_parameter_convert__ret_i16_u8
	.long	L_1+799
	.long	return_parameter_convert__ret_i16_i64
	.long	L_1+810
	.long	return_parameter_convert__ret_i16_C
	.long	L_1+822
	.long	return_parameter_convert__ret_i16_i16
	.long	L_1+832
	.long	return_parameter_convert__ret_i16_I
	.long	L_1+844
	.long	return_parameter_convert__ret_i16_i32
	.long	L_1+854
	.long	return_parameter_convert__ret_i16_LC
	.long	L_1+866
	.long	return_parameter_convert__ret_i16_u64
	.long	L_1+877
	.long	return_parameter_convert__ret_i16_u16
	.long	L_1+889
	.long	return_parameter_convert__ret_I_u32
	.long	L_1+901
	.long	return_parameter_convert__ret_I_i8
	.long	L_1+911
	.long	return_parameter_convert__ret_I_L
	.long	L_1+920
	.long	return_parameter_convert__ret_I_u8
	.long	L_1+928
	.long	return_parameter_convert__ret_I_i64
	.long	L_1+937
	.long	return_parameter_convert__ret_I_C
	.long	L_1+947
	.long	return_parameter_convert__ret_I_i16
	.long	L_1+955
	.long	return_parameter_convert__ret_I_I
	.long	L_1+965
	.long	return_parameter_convert__ret_I_i32
	.long	L_1+973
	.long	return_parameter_convert__ret_I_LC
	.long	L_1+983
	.long	return_parameter_convert__ret_I_u64
	.long	L_1+992
	.long	return_parameter_convert__ret_I_u16
	.long	L_1+1002
	.long	return_parameter_convert__ret_i32_u32
	.long	L_1+1012
	.long	return_parameter_convert__ret_i32_i8
	.long	L_1+1024
	.long	return_parameter_convert__ret_i32_L
	.long	L_1+1035
	.long	return_parameter_convert__ret_i32_u8
	.long	L_1+1045
	.long	return_parameter_convert__ret_i32_i64
	.long	L_1+1056
	.long	return_parameter_convert__ret_i32_C
	.long	L_1+1068
	.long	return_parameter_convert__ret_i32_i16
	.long	L_1+1078
	.long	return_parameter_convert__ret_i32_I
	.long	L_1+1090
	.long	return_parameter_convert__ret_i32_i32
	.long	L_1+1100
	.long	return_parameter_convert__ret_i32_LC
	.long	L_1+1112
	.long	return_parameter_convert__ret_i32_u64
	.long	L_1+1123
	.long	return_parameter_convert__ret_i32_u16
	.long	L_1+1135
	.long	return_parameter_convert__ret_LC_u32
	.long	L_1+1147
	.long	return_parameter_convert__ret_LC_i8
	.long	L_1+1158
	.long	return_parameter_convert__ret_LC_L
	.long	L_1+1168
	.long	return_parameter_convert__ret_LC_u8
	.long	L_1+1177
	.long	return_parameter_convert__ret_LC_i64
	.long	L_1+1187
	.long	return_parameter_convert__ret_LC_C
	.long	L_1+1198
	.long	return_parameter_convert__ret_LC_i16
	.long	L_1+1207
	.long	return_parameter_convert__ret_LC_I
	.long	L_1+1218
	.long	return_parameter_convert__ret_LC_i32
	.long	L_1+1227
	.long	return_parameter_convert__ret_LC_LC
	.long	L_1+1238
	.long	return_parameter_convert__ret_LC_u64
	.long	L_1+1248
	.long	return_parameter_convert__ret_LC_u16
	.long	L_1+1259
	.long	return_parameter_convert__ret_u64_u32
	.long	L_1+1270
	.long	return_parameter_convert__ret_u64_i8
	.long	L_1+1282
	.long	return_parameter_convert__ret_u64_L
	.long	L_1+1293
	.long	return_parameter_convert__ret_u64_u8
	.long	L_1+1303
	.long	return_parameter_convert__ret_u64_i64
	.long	L_1+1314
	.long	return_parameter_convert__ret_u64_C
	.long	L_1+1326
	.long	return_parameter_convert__ret_u64_i16
	.long	L_1+1336
	.long	return_parameter_convert__ret_u64_I
	.long	L_1+1348
	.long	return_parameter_convert__ret_u64_i32
	.long	L_1+1358
	.long	return_parameter_convert__ret_u64_LC
	.long	L_1+1370
	.long	return_parameter_convert__ret_u64_u64
	.long	L_1+1381
	.long	return_parameter_convert__ret_u64_u16
	.long	L_1+1393
	.long	return_parameter_convert__ret_u16_u32
	.long	L_1+1405
	.long	return_parameter_convert__ret_u16_i8
	.long	L_1+1417
	.long	return_parameter_convert__ret_u16_L
	.long	L_1+1428
	.long	return_parameter_convert__ret_u16_u8
	.long	L_1+1438
	.long	return_parameter_convert__ret_u16_i64
	.long	L_1+1449
	.long	return_parameter_convert__ret_u16_C
	.long	L_1+1461
	.long	return_parameter_convert__ret_u16_i16
	.long	L_1+1471
	.long	return_parameter_convert__ret_u16_I
	.long	L_1+1483
	.long	return_parameter_convert__ret_u16_i32
	.long	L_1+1493
	.long	return_parameter_convert__ret_u16_LC
	.long	L_1+1505
	.long	return_parameter_convert__ret_u16_u64
	.long	L_1+1516
	.long	return_parameter_convert__ret_u16_u16
	.long	L_1+1528
	.skip 4
	.ascii	"../SOLsun/return_parameter_convert.m3"
	.skip 3
	.align 8
	.type	MM_return_parameter_convert, #object
	.size	MM_return_parameter_convert, 208
MM_return_parameter_convert:
	.long	L_1+2704
	.skip 16
	.long	L_1+1540
	.skip 12
	.long	MM_return_parameter_convert+148
	.skip 4
	.long	return_parameter_convert_M3
	.long	3
	.half	113
	.skip 2
	.long	0
	.long	114
	.long	0
	.long	115
	.long	116
	.half	117
	.skip 2
	.long	118
	.skip 4
	.long	0
	.long	119
	.long	120
	.skip 4
	.long	1079920590
	.long	-652835029
	.byte	123
	.skip 7
	.long	0
	.long	124
	.byte	125
	.skip 3
	.long	126
	.long	1123959177
	.skip 12
	.long	return_parameter_convert_I3
	.long	MM_return_parameter_convert+160
	.skip 4
	.long	Long_I3
	.long	MM_return_parameter_convert+172
	.skip 4
	.long	Word_I3
	.long	MM_return_parameter_convert+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_return_parameter_convert+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
