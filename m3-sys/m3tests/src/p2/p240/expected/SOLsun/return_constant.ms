	.file	"return_constant.mc"
	.section	".text"
	.align 4
	.type	return_constant__ret_ku16, #function
	.proc	015
return_constant__ret_ku16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	mov	1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	return_constant__ret_ku16, .-return_constant__ret_ku16
	.align 4
	.type	return_constant__ret_ku64, #function
	.proc	05
return_constant__ret_ku64:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	mov	0, %o4
	mov	2, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE1:
	.size	return_constant__ret_ku64, .-return_constant__ret_ku64
	.align 4
	.type	return_constant__ret_kLC, #function
	.proc	017
return_constant__ret_kLC:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	mov	0, %o4
	mov	3, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE2:
	.size	return_constant__ret_kLC, .-return_constant__ret_kLC
	.align 4
	.type	return_constant__ret_ki32, #function
	.proc	04
return_constant__ret_ki32:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	mov	4, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	return_constant__ret_ki32, .-return_constant__ret_ki32
	.align 4
	.type	return_constant__ret_ki16, #function
	.proc	03
return_constant__ret_ki16:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	mov	5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE4:
	.size	return_constant__ret_ki16, .-return_constant__ret_ki16
	.align 4
	.type	return_constant__ret_kI, #function
	.proc	04
return_constant__ret_kI:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	mov	6, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE5:
	.size	return_constant__ret_kI, .-return_constant__ret_kI
	.align 4
	.type	return_constant__ret_ki64, #function
	.proc	05
return_constant__ret_ki64:
.LLFB6:
	save	%sp, -96, %sp
.LLCFI6:
	mov	0, %o4
	mov	7, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	return_constant__ret_ki64, .-return_constant__ret_ki64
	.align 4
	.type	return_constant__ret_kC, #function
	.proc	016
return_constant__ret_kC:
.LLFB7:
	save	%sp, -96, %sp
.LLCFI7:
	mov	8, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE7:
	.size	return_constant__ret_kC, .-return_constant__ret_kC
	.section	".rodata"
	.align 8
.LLC0:
	.long	1075983155
	.long	858993459
	.section	".text"
	.align 4
	.type	return_constant__ret_kf64, #function
	.proc	07
return_constant__ret_kf64:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(.LLC0), %g1
	or	%g1, %lo(.LLC0), %g1
	ldd	[%g1], %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE8:
	.size	return_constant__ret_kf64, .-return_constant__ret_kf64
	.align 4
	.type	return_constant__ret_ku8, #function
	.proc	014
return_constant__ret_ku8:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	mov	11, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE9:
	.size	return_constant__ret_ku8, .-return_constant__ret_ku8
	.align 4
	.type	return_constant__ret_kL, #function
	.proc	05
return_constant__ret_kL:
.LLFB10:
	save	%sp, -96, %sp
.LLCFI10:
	mov	0, %o4
	mov	12, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	return_constant__ret_kL, .-return_constant__ret_kL
	.align 4
	.type	return_constant__ret_ki8, #function
	.proc	02
return_constant__ret_ki8:
.LLFB11:
	save	%sp, -96, %sp
.LLCFI11:
	mov	13, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE11:
	.size	return_constant__ret_ki8, .-return_constant__ret_ki8
	.align 4
	.type	return_constant__ret_ku32, #function
	.proc	04
return_constant__ret_ku32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	mov	14, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	return_constant__ret_ku32, .-return_constant__ret_ku32
	.section	".rodata"
	.align 4
.LLC1:
	.long	1098026844
	.section	".text"
	.align 4
	.type	return_constant__ret_kf32, #function
	.proc	06
return_constant__ret_kf32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(.LLC1), %g1
	or	%g1, %lo(.LLC1), %g1
	ld	[%g1], %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE13:
	.size	return_constant__ret_kf32, .-return_constant__ret_kf32
	.align 4
	.global return_constant_M3
	.type	return_constant_M3, #function
	.proc	0120
return_constant_M3:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	st	%i0, [%fp+68]
	sethi	%hi(MM_return_constant), %g1
	or	%g1, %lo(MM_return_constant), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	return_constant_M3, .-return_constant_M3
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 296
L_1:
	.ascii	"return_constant_M3"
	.skip 1
	.ascii	"ret_kf32"
	.skip 1
	.ascii	"ret_ku32"
	.skip 1
	.ascii	"ret_ki8"
	.skip 1
	.ascii	"ret_kL"
	.skip 1
	.ascii	"ret_ku8"
	.skip 1
	.ascii	"ret_kf64"
	.skip 1
	.ascii	"ret_kC"
	.skip 1
	.ascii	"ret_ki64"
	.skip 1
	.ascii	"ret_kI"
	.skip 1
	.ascii	"ret_ki16"
	.skip 1
	.ascii	"ret_ki32"
	.skip 1
	.ascii	"ret_kLC"
	.skip 1
	.ascii	"ret_ku64"
	.skip 1
	.ascii	"ret_ku16"
	.skip 1
	.long	return_constant_M3
	.long	L_1
	.long	return_constant__ret_kf32
	.long	L_1+19
	.long	return_constant__ret_ku32
	.long	L_1+28
	.long	return_constant__ret_ki8
	.long	L_1+37
	.long	return_constant__ret_kL
	.long	L_1+45
	.long	return_constant__ret_ku8
	.long	L_1+52
	.long	return_constant__ret_kf64
	.long	L_1+60
	.long	return_constant__ret_kC
	.long	L_1+69
	.long	return_constant__ret_ki64
	.long	L_1+76
	.long	return_constant__ret_kI
	.long	L_1+85
	.long	return_constant__ret_ki16
	.long	L_1+92
	.long	return_constant__ret_ki32
	.long	L_1+101
	.long	return_constant__ret_kLC
	.long	L_1+110
	.long	return_constant__ret_ku64
	.long	L_1+118
	.long	return_constant__ret_ku16
	.long	L_1+127
	.skip 4
	.ascii	"../SOLsun/return_constant.m3"
	.skip 8
	.align 8
	.type	MM_return_constant, #object
	.size	MM_return_constant, 208
MM_return_constant:
	.long	L_1+260
	.skip 16
	.long	L_1+136
	.skip 12
	.long	MM_return_constant+148
	.skip 4
	.long	return_constant_M3
	.long	3
	.half	17
	.skip 2
	.long	0
	.long	18
	.long	0
	.long	19
	.long	20
	.half	21
	.skip 2
	.long	22
	.skip 4
	.long	0
	.long	23
	.long	24
	.skip 4
	.long	1077494415
	.long	1546188227
	.byte	27
	.skip 7
	.long	0
	.long	28
	.byte	29
	.skip 3
	.long	30
	.long	1106939740
	.skip 12
	.long	return_constant_I3
	.long	MM_return_constant+160
	.skip 4
	.long	Long_I3
	.long	MM_return_constant+172
	.skip 4
	.long	Word_I3
	.long	MM_return_constant+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_return_constant+196
	.skip 4
	.long	RTHooks_I3
	.skip 4
	.ident	"GCC: (GNU) 4.5.1"
