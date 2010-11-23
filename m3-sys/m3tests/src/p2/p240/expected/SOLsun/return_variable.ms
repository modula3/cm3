	.file	"return_variable.mc"
	.section	".text"
	.align 4
	.type	return_variable__ret_vu16, #function
	.proc	015
return_variable__ret_vu16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	return_variable__ret_vu16, .-return_variable__ret_vu16
	.align 4
	.type	return_variable__ret_vu64, #function
	.proc	05
return_variable__ret_vu64:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1:
	.size	return_variable__ret_vu64, .-return_variable__ret_vu64
	.align 4
	.type	return_variable__ret_vLC, #function
	.proc	017
return_variable__ret_vLC:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE2:
	.size	return_variable__ret_vLC, .-return_variable__ret_vLC
	.align 4
	.type	return_variable__ret_vi32, #function
	.proc	04
return_variable__ret_vi32:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+72], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	return_variable__ret_vi32, .-return_variable__ret_vi32
	.align 4
	.type	return_variable__ret_vi16, #function
	.proc	03
return_variable__ret_vi16:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	return_variable__ret_vi16, .-return_variable__ret_vi16
	.align 4
	.type	return_variable__ret_vI, #function
	.proc	04
return_variable__ret_vI:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+80], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	return_variable__ret_vI, .-return_variable__ret_vI
	.align 4
	.type	return_variable__ret_vi64, #function
	.proc	05
return_variable__ret_vi64:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	return_variable__ret_vi64, .-return_variable__ret_vi64
	.align 4
	.type	return_variable__ret_vC, #function
	.proc	016
return_variable__ret_vC:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+96], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	return_variable__ret_vC, .-return_variable__ret_vC
	.align 4
	.type	return_variable__ret_vf64, #function
	.proc	07
return_variable__ret_vf64:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE8:
	.size	return_variable__ret_vf64, .-return_variable__ret_vf64
	.align 4
	.type	return_variable__ret_vu8, #function
	.proc	014
return_variable__ret_vu8:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	return_variable__ret_vu8, .-return_variable__ret_vu8
	.align 4
	.type	return_variable__ret_vL, #function
	.proc	05
return_variable__ret_vL:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	return_variable__ret_vL, .-return_variable__ret_vL
	.align 4
	.type	return_variable__ret_vi8, #function
	.proc	02
return_variable__ret_vi8:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	return_variable__ret_vi8, .-return_variable__ret_vi8
	.align 4
	.type	return_variable__ret_vu32, #function
	.proc	04
return_variable__ret_vu32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	ld	[%g1+132], %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	return_variable__ret_vu32, .-return_variable__ret_vu32
	.align 4
	.type	return_variable__ret_vf32, #function
	.proc	06
return_variable__ret_vf32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE13:
	.size	return_variable__ret_vf32, .-return_variable__ret_vf32
	.align 4
	.global return_variable_M3
	.type	return_variable_M3, #function
	.proc	0120
return_variable_M3:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	sethi	%hi(MM_return_variable), %g1
	or	%g1, %lo(MM_return_variable), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	return_variable_M3, .-return_variable_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 296
L_1:
	.ascii	"return_variable_M3"
	.skip 1
	.ascii	"ret_vf32"
	.skip 1
	.ascii	"ret_vu32"
	.skip 1
	.ascii	"ret_vi8"
	.skip 1
	.ascii	"ret_vL"
	.skip 1
	.ascii	"ret_vu8"
	.skip 1
	.ascii	"ret_vf64"
	.skip 1
	.ascii	"ret_vC"
	.skip 1
	.ascii	"ret_vi64"
	.skip 1
	.ascii	"ret_vI"
	.skip 1
	.ascii	"ret_vi16"
	.skip 1
	.ascii	"ret_vi32"
	.skip 1
	.ascii	"ret_vLC"
	.skip 1
	.ascii	"ret_vu64"
	.skip 1
	.ascii	"ret_vu16"
	.skip 1
	.long	return_variable_M3
	.long	L_1
	.long	return_variable__ret_vf32
	.long	L_1+19
	.long	return_variable__ret_vu32
	.long	L_1+28
	.long	return_variable__ret_vi8
	.long	L_1+37
	.long	return_variable__ret_vL
	.long	L_1+45
	.long	return_variable__ret_vu8
	.long	L_1+52
	.long	return_variable__ret_vf64
	.long	L_1+60
	.long	return_variable__ret_vC
	.long	L_1+69
	.long	return_variable__ret_vi64
	.long	L_1+76
	.long	return_variable__ret_vI
	.long	L_1+85
	.long	return_variable__ret_vi16
	.long	L_1+92
	.long	return_variable__ret_vi32
	.long	L_1+101
	.long	return_variable__ret_vLC
	.long	L_1+110
	.long	return_variable__ret_vu64
	.long	L_1+118
	.long	return_variable__ret_vu16
	.long	L_1+127
	.skip 4
	.ascii	"../SOLsun/return_variable.m3"
	.skip 8
	.align 8
	.type	MM_return_variable, #object
	.size	MM_return_variable, 208
MM_return_variable:
	.long	L_1+260
	.skip 16
	.long	L_1+136
	.skip 12
	.long	MM_return_variable+148
	.skip 4
	.long	return_variable_M3
	.long	3
	.half	49
	.skip 2
	.long	0
	.long	50
	.long	0
	.long	51
	.long	52
	.half	53
	.skip 2
	.long	54
	.skip 4
	.long	0
	.long	55
	.long	56
	.skip 4
	.long	1078774333
	.long	1889785610
	.byte	59
	.skip 7
	.long	0
	.long	60
	.byte	61
	.skip 3
	.long	62
	.long	1115590492
	.skip 12
	.long	return_variable_I3
	.long	MM_return_variable+160
	.skip 4
	.long	Long_I3
	.long	MM_return_variable+172
	.skip 4
	.long	Word_I3
	.long	MM_return_variable+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_return_variable+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
