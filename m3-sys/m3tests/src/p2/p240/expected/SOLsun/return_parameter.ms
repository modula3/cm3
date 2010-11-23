	.file	"return_parameter.mc"
	.section	".text"
	.align 4
	.type	return_parameter__ret_pu16, #function
	.proc	015
return_parameter__ret_pu16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	return_parameter__ret_pu16, .-return_parameter__ret_pu16
	.align 4
	.type	return_parameter__ret_pu64, #function
	.proc	05
return_parameter__ret_pu64:
.LLFB1:
	save	%sp, -104, %sp
.LLCFI1:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1:
	.size	return_parameter__ret_pu64, .-return_parameter__ret_pu64
	.align 4
	.type	return_parameter__ret_pLC, #function
	.proc	017
return_parameter__ret_pLC:
.LLFB2:
	save	%sp, -104, %sp
.LLCFI2:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE2:
	.size	return_parameter__ret_pLC, .-return_parameter__ret_pLC
	.align 4
	.type	return_parameter__ret_pi32, #function
	.proc	04
return_parameter__ret_pi32:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	return_parameter__ret_pi32, .-return_parameter__ret_pi32
	.align 4
	.type	return_parameter__ret_pi16, #function
	.proc	03
return_parameter__ret_pi16:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sth	%i0, [%fp+68]
	lduh	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	return_parameter__ret_pi16, .-return_parameter__ret_pi16
	.align 4
	.type	return_parameter__ret_pI, #function
	.proc	04
return_parameter__ret_pI:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	return_parameter__ret_pI, .-return_parameter__ret_pI
	.align 4
	.type	return_parameter__ret_pi64, #function
	.proc	05
return_parameter__ret_pi64:
.LLFB6:
	save	%sp, -104, %sp
.LLCFI6:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	return_parameter__ret_pi64, .-return_parameter__ret_pi64
	.align 4
	.type	return_parameter__ret_pC, #function
	.proc	016
return_parameter__ret_pC:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	return_parameter__ret_pC, .-return_parameter__ret_pC
	.align 4
	.type	return_parameter__ret_pf64, #function
	.proc	07
return_parameter__ret_pf64:
.LLFB8:
	save	%sp, -104, %sp
.LLCFI8:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE8:
	.size	return_parameter__ret_pf64, .-return_parameter__ret_pf64
	.align 4
	.type	return_parameter__ret_pu8, #function
	.proc	014
return_parameter__ret_pu8:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	return_parameter__ret_pu8, .-return_parameter__ret_pu8
	.align 4
	.type	return_parameter__ret_pL, #function
	.proc	05
return_parameter__ret_pL:
.LLFB10:
	save	%sp, -104, %sp
.LLCFI10:
	std	%i0, [%fp-8]
	ldd	[%fp-8], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	return_parameter__ret_pL, .-return_parameter__ret_pL
	.align 4
	.type	return_parameter__ret_pi8, #function
	.proc	02
return_parameter__ret_pi8:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	stb	%i0, [%fp+68]
	ldub	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	return_parameter__ret_pi8, .-return_parameter__ret_pi8
	.align 4
	.type	return_parameter__ret_pu32, #function
	.proc	04
return_parameter__ret_pu32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	st	%i0, [%fp+68]
	ld	[%fp+68], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	return_parameter__ret_pu32, .-return_parameter__ret_pu32
	.align 4
	.type	return_parameter__ret_pf32, #function
	.proc	06
return_parameter__ret_pf32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	st	%i0, [%fp+68]
	ld	[%fp+68], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE13:
	.size	return_parameter__ret_pf32, .-return_parameter__ret_pf32
	.align 4
	.global return_parameter_M3
	.type	return_parameter_M3, #function
	.proc	0120
return_parameter_M3:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	sethi	%hi(MM_return_parameter), %g1
	or	%g1, %lo(MM_return_parameter), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	return_parameter_M3, .-return_parameter_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 296
L_1:
	.ascii	"return_parameter_M3"
	.skip 1
	.ascii	"ret_pf32"
	.skip 1
	.ascii	"ret_pu32"
	.skip 1
	.ascii	"ret_pi8"
	.skip 1
	.ascii	"ret_pL"
	.skip 1
	.ascii	"ret_pu8"
	.skip 1
	.ascii	"ret_pf64"
	.skip 1
	.ascii	"ret_pC"
	.skip 1
	.ascii	"ret_pi64"
	.skip 1
	.ascii	"ret_pI"
	.skip 1
	.ascii	"ret_pi16"
	.skip 1
	.ascii	"ret_pi32"
	.skip 1
	.ascii	"ret_pLC"
	.skip 1
	.ascii	"ret_pu64"
	.skip 1
	.ascii	"ret_pu16"
	.skip 4
	.long	return_parameter_M3
	.long	L_1
	.long	return_parameter__ret_pf32
	.long	L_1+20
	.long	return_parameter__ret_pu32
	.long	L_1+29
	.long	return_parameter__ret_pi8
	.long	L_1+38
	.long	return_parameter__ret_pL
	.long	L_1+46
	.long	return_parameter__ret_pu8
	.long	L_1+53
	.long	return_parameter__ret_pf64
	.long	L_1+61
	.long	return_parameter__ret_pC
	.long	L_1+70
	.long	return_parameter__ret_pi64
	.long	L_1+77
	.long	return_parameter__ret_pI
	.long	L_1+86
	.long	return_parameter__ret_pi16
	.long	L_1+93
	.long	return_parameter__ret_pi32
	.long	L_1+102
	.long	return_parameter__ret_pLC
	.long	L_1+111
	.long	return_parameter__ret_pu64
	.long	L_1+119
	.long	return_parameter__ret_pu16
	.long	L_1+128
	.skip 4
	.ascii	"../SOLsun/return_parameter.m3"
	.skip 3
	.align 8
	.type	MM_return_parameter, #object
	.size	MM_return_parameter, 208
MM_return_parameter:
	.long	L_1+264
	.skip 16
	.long	L_1+140
	.skip 12
	.long	MM_return_parameter+148
	.skip 4
	.long	return_parameter_M3
	.long	3
	.half	81
	.skip 2
	.long	0
	.long	82
	.long	0
	.long	83
	.long	84
	.half	85
	.skip 2
	.long	86
	.skip 4
	.long	0
	.long	87
	.long	88
	.skip 4
	.long	1079409049
	.long	-1717986918
	.byte	91
	.skip 7
	.long	0
	.long	92
	.byte	93
	.skip 3
	.long	94
	.long	1119873925
	.skip 12
	.long	return_parameter_I3
	.long	MM_return_parameter+160
	.skip 4
	.long	Long_I3
	.long	MM_return_parameter+172
	.skip 4
	.long	Word_I3
	.long	MM_return_parameter+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_return_parameter+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
