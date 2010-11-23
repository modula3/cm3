	.file	"Mod.mc"
	.section	".text"
	.align 4
	.type	Mod__uMod_var_u16_u16, #function
	.proc	04
Mod__uMod_var_u16_u16:
.LLFB0:
	save	%sp, -96, %sp
.LLCFI0:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE0:
	.size	Mod__uMod_var_u16_u16, .-Mod__uMod_var_u16_u16
	.align 4
	.type	Mod__Mod_var_u16_u16, #function
	.proc	015
Mod__Mod_var_u16_u16:
.LLFB1:
	save	%sp, -96, %sp
.LLCFI1:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL4
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL7
	 nop
.LL3:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL6
	 nop
.LL4:
	cmp	%g1, 0
	bg	%icc, .LL5
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL7
	 nop
.LL5:
	add	%g1, -1, %g1
.LL6:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL7:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE1:
	.size	Mod__Mod_var_u16_u16, .-Mod__Mod_var_u16_u16
	.align 4
	.type	Mod__uMod_param_u16_u16, #function
	.proc	04
Mod__uMod_param_u16_u16:
.LLFB2:
	save	%sp, -96, %sp
.LLCFI2:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE2:
	.size	Mod__uMod_param_u16_u16, .-Mod__uMod_param_u16_u16
	.align 4
	.type	Mod__Mod_param_u16_u16, #function
	.proc	015
Mod__Mod_param_u16_u16:
.LLFB3:
	save	%sp, -96, %sp
.LLCFI3:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL11
	 nop
	cmp	%g1, 0
	bl	%icc, .LL10
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL14
	 nop
.LL10:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL13
	 nop
.LL11:
	cmp	%g1, 0
	bg	%icc, .LL12
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL14
	 nop
.LL12:
	add	%g1, -1, %g1
.LL13:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL14:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE3:
	.size	Mod__Mod_param_u16_u16, .-Mod__Mod_param_u16_u16
	.global __umoddi3
	.align 4
	.type	Mod__uMod_var_u16_u64, #function
	.proc	05
Mod__uMod_var_u16_u64:
.LLFB4:
	save	%sp, -96, %sp
.LLCFI4:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE4:
	.size	Mod__uMod_var_u16_u64, .-Mod__uMod_var_u16_u64
	.global __divdi3
	.align 4
	.type	Mod__Mod_var_u16_u64, #function
	.proc	05
Mod__Mod_var_u16_u64:
.LLFB5:
	save	%sp, -96, %sp
.LLCFI5:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL18
	 nop
	cmp	%l0, 0
	bne	%icc, .LL22
	 nop
	cmp	%l1, 0
.LL22:
	cmp	%o4, 0
	bl	%icc, .LL17
	 nop
	cmp	%o4, 0
	bne	%icc, .LL23
	 nop
	cmp	%o5, 0
.LL23:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL21
	 nop
.LL17:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL20
	 nop
.LL18:
	cmp	%o4, 0
	bg	%icc, .LL19
	 nop
	cmp	%o4, 0
	bne	%icc, .LL24
	 nop
	cmp	%o5, 0
	bne	%icc, .LL19
	 nop
	cmp	%o5, 0
.LL24:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL21
	 nop
.LL19:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL20:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL21:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE5:
	.size	Mod__Mod_var_u16_u64, .-Mod__Mod_var_u16_u64
	.align 4
	.type	Mod__uMod_param_u16_u64, #function
	.proc	05
Mod__uMod_param_u16_u64:
.LLFB6:
	save	%sp, -104, %sp
.LLCFI6:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE6:
	.size	Mod__uMod_param_u16_u64, .-Mod__uMod_param_u16_u64
	.align 4
	.type	Mod__Mod_param_u16_u64, #function
	.proc	05
Mod__Mod_param_u16_u64:
.LLFB7:
	save	%sp, -104, %sp
.LLCFI7:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL28
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL32
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL32:
	cmp	%o4, 0
	bl	%icc, .LL27
	 nop
	cmp	%o4, 0
	bne	%icc, .LL33
	 nop
	cmp	%o5, 0
.LL33:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL31
	 nop
.LL27:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL30
	 nop
.LL28:
	cmp	%o4, 0
	bg	%icc, .LL29
	 nop
	cmp	%o4, 0
	bne	%icc, .LL34
	 nop
	cmp	%o5, 0
	bne	%icc, .LL29
	 nop
	cmp	%o5, 0
.LL34:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL31
	 nop
.LL29:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL30:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL31:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE7:
	.size	Mod__Mod_param_u16_u64, .-Mod__Mod_param_u16_u64
	.align 4
	.type	Mod__uMod_var_u16_LC, #function
	.proc	05
Mod__uMod_var_u16_LC:
.LLFB8:
	save	%sp, -96, %sp
.LLCFI8:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE8:
	.size	Mod__uMod_var_u16_LC, .-Mod__uMod_var_u16_LC
	.align 4
	.type	Mod__Mod_var_u16_LC, #function
	.proc	05
Mod__Mod_var_u16_LC:
.LLFB9:
	save	%sp, -96, %sp
.LLCFI9:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL38
	 nop
	cmp	%l0, 0
	bne	%icc, .LL42
	 nop
	cmp	%l1, 0
.LL42:
	cmp	%o4, 0
	bl	%icc, .LL37
	 nop
	cmp	%o4, 0
	bne	%icc, .LL43
	 nop
	cmp	%o5, 0
.LL43:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL41
	 nop
.LL37:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL40
	 nop
.LL38:
	cmp	%o4, 0
	bg	%icc, .LL39
	 nop
	cmp	%o4, 0
	bne	%icc, .LL44
	 nop
	cmp	%o5, 0
	bne	%icc, .LL39
	 nop
	cmp	%o5, 0
.LL44:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL41
	 nop
.LL39:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL40:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL41:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE9:
	.size	Mod__Mod_var_u16_LC, .-Mod__Mod_var_u16_LC
	.align 4
	.type	Mod__uMod_param_u16_LC, #function
	.proc	05
Mod__uMod_param_u16_LC:
.LLFB10:
	save	%sp, -104, %sp
.LLCFI10:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE10:
	.size	Mod__uMod_param_u16_LC, .-Mod__uMod_param_u16_LC
	.align 4
	.type	Mod__Mod_param_u16_LC, #function
	.proc	05
Mod__Mod_param_u16_LC:
.LLFB11:
	save	%sp, -104, %sp
.LLCFI11:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l2
	mov	%g1, %l3
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL48
	 nop
	cmp	%l0, 0
	bne	%icc, .LL52
	 nop
	cmp	%l1, 0
.LL52:
	cmp	%o4, 0
	bl	%icc, .LL47
	 nop
	cmp	%o4, 0
	bne	%icc, .LL53
	 nop
	cmp	%o5, 0
.LL53:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL51
	 nop
.LL47:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL50
	 nop
.LL48:
	cmp	%o4, 0
	bg	%icc, .LL49
	 nop
	cmp	%o4, 0
	bne	%icc, .LL54
	 nop
	cmp	%o5, 0
	bne	%icc, .LL49
	 nop
	cmp	%o5, 0
.LL54:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL51
	 nop
.LL49:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL50:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL51:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE11:
	.size	Mod__Mod_param_u16_LC, .-Mod__Mod_param_u16_LC
	.align 4
	.type	Mod__uMod_var_u16_i32, #function
	.proc	04
Mod__uMod_var_u16_i32:
.LLFB12:
	save	%sp, -96, %sp
.LLCFI12:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE12:
	.size	Mod__uMod_var_u16_i32, .-Mod__uMod_var_u16_i32
	.align 4
	.type	Mod__Mod_var_u16_i32, #function
	.proc	015
Mod__Mod_var_u16_i32:
.LLFB13:
	save	%sp, -96, %sp
.LLCFI13:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL58
	 nop
	cmp	%g1, 0
	bl	%icc, .LL57
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL61
	 nop
.LL57:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL60
	 nop
.LL58:
	cmp	%g1, 0
	bg	%icc, .LL59
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL61
	 nop
.LL59:
	add	%g1, -1, %g1
.LL60:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL61:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL62
	 nop
	mov	2145, %o0
	call	_m3_fault, 0
	 nop
.LL62:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL64
	 nop
	cmp	%g1, 0
	bl	%icc, .LL63
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL67
	 nop
.LL63:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL66
	 nop
.LL64:
	cmp	%g1, 0
	bg	%icc, .LL65
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL67
	 nop
.LL65:
	add	%g1, -1, %g1
.LL66:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL67:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE13:
	.size	Mod__Mod_var_u16_i32, .-Mod__Mod_var_u16_i32
	.align 4
	.type	Mod__uMod_param_u16_i32, #function
	.proc	04
Mod__uMod_param_u16_i32:
.LLFB14:
	save	%sp, -96, %sp
.LLCFI14:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE14:
	.size	Mod__uMod_param_u16_i32, .-Mod__uMod_param_u16_i32
	.align 4
	.type	Mod__Mod_param_u16_i32, #function
	.proc	015
Mod__Mod_param_u16_i32:
.LLFB15:
	save	%sp, -96, %sp
.LLCFI15:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL71
	 nop
	cmp	%g1, 0
	bl	%icc, .LL70
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL74
	 nop
.LL70:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL73
	 nop
.LL71:
	cmp	%g1, 0
	bg	%icc, .LL72
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL74
	 nop
.LL72:
	add	%g1, -1, %g1
.LL73:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL74:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL75
	 nop
	mov	2209, %o0
	call	_m3_fault, 0
	 nop
.LL75:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL77
	 nop
	cmp	%g1, 0
	bl	%icc, .LL76
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL80
	 nop
.LL76:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL79
	 nop
.LL77:
	cmp	%g1, 0
	bg	%icc, .LL78
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL80
	 nop
.LL78:
	add	%g1, -1, %g1
.LL79:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL80:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE15:
	.size	Mod__Mod_param_u16_i32, .-Mod__Mod_param_u16_i32
	.align 4
	.type	Mod__uMod_var_u16_i16, #function
	.proc	04
Mod__uMod_var_u16_i16:
.LLFB16:
	save	%sp, -96, %sp
.LLCFI16:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE16:
	.size	Mod__uMod_var_u16_i16, .-Mod__uMod_var_u16_i16
	.align 4
	.type	Mod__Mod_var_u16_i16, #function
	.proc	015
Mod__Mod_var_u16_i16:
.LLFB17:
	save	%sp, -96, %sp
.LLCFI17:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL84
	 nop
	cmp	%g1, 0
	bl	%icc, .LL83
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL87
	 nop
.LL83:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL86
	 nop
.LL84:
	cmp	%g1, 0
	bg	%icc, .LL85
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL87
	 nop
.LL85:
	add	%g1, -1, %g1
.LL86:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL87:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL88
	 nop
	mov	2273, %o0
	call	_m3_fault, 0
	 nop
.LL88:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL90
	 nop
	cmp	%g1, 0
	bl	%icc, .LL89
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL93
	 nop
.LL89:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL92
	 nop
.LL90:
	cmp	%g1, 0
	bg	%icc, .LL91
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL93
	 nop
.LL91:
	add	%g1, -1, %g1
.LL92:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL93:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE17:
	.size	Mod__Mod_var_u16_i16, .-Mod__Mod_var_u16_i16
	.align 4
	.type	Mod__uMod_param_u16_i16, #function
	.proc	04
Mod__uMod_param_u16_i16:
.LLFB18:
	save	%sp, -96, %sp
.LLCFI18:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE18:
	.size	Mod__uMod_param_u16_i16, .-Mod__uMod_param_u16_i16
	.align 4
	.type	Mod__Mod_param_u16_i16, #function
	.proc	015
Mod__Mod_param_u16_i16:
.LLFB19:
	save	%sp, -96, %sp
.LLCFI19:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL97
	 nop
	cmp	%g1, 0
	bl	%icc, .LL96
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL100
	 nop
.LL96:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL99
	 nop
.LL97:
	cmp	%g1, 0
	bg	%icc, .LL98
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL100
	 nop
.LL98:
	add	%g1, -1, %g1
.LL99:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL100:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL101
	 nop
	mov	2337, %o0
	call	_m3_fault, 0
	 nop
.LL101:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL103
	 nop
	cmp	%g1, 0
	bl	%icc, .LL102
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL106
	 nop
.LL102:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL105
	 nop
.LL103:
	cmp	%g1, 0
	bg	%icc, .LL104
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL106
	 nop
.LL104:
	add	%g1, -1, %g1
.LL105:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL106:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE19:
	.size	Mod__Mod_param_u16_i16, .-Mod__Mod_param_u16_i16
	.align 4
	.type	Mod__uMod_var_u16_I, #function
	.proc	04
Mod__uMod_var_u16_I:
.LLFB20:
	save	%sp, -96, %sp
.LLCFI20:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE20:
	.size	Mod__uMod_var_u16_I, .-Mod__uMod_var_u16_I
	.align 4
	.type	Mod__Mod_var_u16_I, #function
	.proc	015
Mod__Mod_var_u16_I:
.LLFB21:
	save	%sp, -96, %sp
.LLCFI21:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL110
	 nop
	cmp	%g1, 0
	bl	%icc, .LL109
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL113
	 nop
.LL109:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL112
	 nop
.LL110:
	cmp	%g1, 0
	bg	%icc, .LL111
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL113
	 nop
.LL111:
	add	%g1, -1, %g1
.LL112:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL113:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL114
	 nop
	mov	2401, %o0
	call	_m3_fault, 0
	 nop
.LL114:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL116
	 nop
	cmp	%g1, 0
	bl	%icc, .LL115
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL119
	 nop
.LL115:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL118
	 nop
.LL116:
	cmp	%g1, 0
	bg	%icc, .LL117
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL119
	 nop
.LL117:
	add	%g1, -1, %g1
.LL118:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL119:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE21:
	.size	Mod__Mod_var_u16_I, .-Mod__Mod_var_u16_I
	.align 4
	.type	Mod__uMod_param_u16_I, #function
	.proc	04
Mod__uMod_param_u16_I:
.LLFB22:
	save	%sp, -96, %sp
.LLCFI22:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE22:
	.size	Mod__uMod_param_u16_I, .-Mod__uMod_param_u16_I
	.align 4
	.type	Mod__Mod_param_u16_I, #function
	.proc	015
Mod__Mod_param_u16_I:
.LLFB23:
	save	%sp, -96, %sp
.LLCFI23:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL123
	 nop
	cmp	%g1, 0
	bl	%icc, .LL122
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL126
	 nop
.LL122:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL125
	 nop
.LL123:
	cmp	%g1, 0
	bg	%icc, .LL124
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL126
	 nop
.LL124:
	add	%g1, -1, %g1
.LL125:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL126:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL127
	 nop
	mov	2465, %o0
	call	_m3_fault, 0
	 nop
.LL127:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL129
	 nop
	cmp	%g1, 0
	bl	%icc, .LL128
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL132
	 nop
.LL128:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL131
	 nop
.LL129:
	cmp	%g1, 0
	bg	%icc, .LL130
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL132
	 nop
.LL130:
	add	%g1, -1, %g1
.LL131:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL132:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE23:
	.size	Mod__Mod_param_u16_I, .-Mod__Mod_param_u16_I
	.align 4
	.type	Mod__uMod_var_u16_i64, #function
	.proc	05
Mod__uMod_var_u16_i64:
.LLFB24:
	save	%sp, -96, %sp
.LLCFI24:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE24:
	.size	Mod__uMod_var_u16_i64, .-Mod__uMod_var_u16_i64
	.align 4
	.type	Mod__Mod_var_u16_i64, #function
	.proc	05
Mod__Mod_var_u16_i64:
.LLFB25:
	save	%sp, -96, %sp
.LLCFI25:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL136
	 nop
	cmp	%l0, 0
	bne	%icc, .LL140
	 nop
	cmp	%l1, 0
.LL140:
	cmp	%o4, 0
	bl	%icc, .LL135
	 nop
	cmp	%o4, 0
	bne	%icc, .LL141
	 nop
	cmp	%o5, 0
.LL141:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL139
	 nop
.LL135:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL138
	 nop
.LL136:
	cmp	%o4, 0
	bg	%icc, .LL137
	 nop
	cmp	%o4, 0
	bne	%icc, .LL142
	 nop
	cmp	%o5, 0
	bne	%icc, .LL137
	 nop
	cmp	%o5, 0
.LL142:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL139
	 nop
.LL137:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL138:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL139:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE25:
	.size	Mod__Mod_var_u16_i64, .-Mod__Mod_var_u16_i64
	.align 4
	.type	Mod__uMod_param_u16_i64, #function
	.proc	05
Mod__uMod_param_u16_i64:
.LLFB26:
	save	%sp, -104, %sp
.LLCFI26:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE26:
	.size	Mod__uMod_param_u16_i64, .-Mod__uMod_param_u16_i64
	.align 4
	.type	Mod__Mod_param_u16_i64, #function
	.proc	05
Mod__Mod_param_u16_i64:
.LLFB27:
	save	%sp, -104, %sp
.LLCFI27:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL146
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL150
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL150:
	cmp	%o4, 0
	bl	%icc, .LL145
	 nop
	cmp	%o4, 0
	bne	%icc, .LL151
	 nop
	cmp	%o5, 0
.LL151:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL149
	 nop
.LL145:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL148
	 nop
.LL146:
	cmp	%o4, 0
	bg	%icc, .LL147
	 nop
	cmp	%o4, 0
	bne	%icc, .LL152
	 nop
	cmp	%o5, 0
	bne	%icc, .LL147
	 nop
	cmp	%o5, 0
.LL152:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL149
	 nop
.LL147:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL148:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL149:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE27:
	.size	Mod__Mod_param_u16_i64, .-Mod__Mod_param_u16_i64
	.align 4
	.type	Mod__uMod_var_u16_C, #function
	.proc	04
Mod__uMod_var_u16_C:
.LLFB28:
	save	%sp, -96, %sp
.LLCFI28:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE28:
	.size	Mod__uMod_var_u16_C, .-Mod__uMod_var_u16_C
	.align 4
	.type	Mod__Mod_var_u16_C, #function
	.proc	015
Mod__Mod_var_u16_C:
.LLFB29:
	save	%sp, -96, %sp
.LLCFI29:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL156
	 nop
	cmp	%g1, 0
	bl	%icc, .LL155
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL159
	 nop
.LL155:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL158
	 nop
.LL156:
	cmp	%g1, 0
	bg	%icc, .LL157
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL159
	 nop
.LL157:
	add	%g1, -1, %g1
.LL158:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL159:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL160
	 nop
	mov	2657, %o0
	call	_m3_fault, 0
	 nop
.LL160:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL162
	 nop
	cmp	%g1, 0
	bl	%icc, .LL161
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL165
	 nop
.LL161:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL164
	 nop
.LL162:
	cmp	%g1, 0
	bg	%icc, .LL163
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL165
	 nop
.LL163:
	add	%g1, -1, %g1
.LL164:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL165:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE29:
	.size	Mod__Mod_var_u16_C, .-Mod__Mod_var_u16_C
	.align 4
	.type	Mod__uMod_param_u16_C, #function
	.proc	04
Mod__uMod_param_u16_C:
.LLFB30:
	save	%sp, -96, %sp
.LLCFI30:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE30:
	.size	Mod__uMod_param_u16_C, .-Mod__uMod_param_u16_C
	.align 4
	.type	Mod__Mod_param_u16_C, #function
	.proc	015
Mod__Mod_param_u16_C:
.LLFB31:
	save	%sp, -96, %sp
.LLCFI31:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL169
	 nop
	cmp	%g1, 0
	bl	%icc, .LL168
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL172
	 nop
.LL168:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL171
	 nop
.LL169:
	cmp	%g1, 0
	bg	%icc, .LL170
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL172
	 nop
.LL170:
	add	%g1, -1, %g1
.LL171:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL172:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL173
	 nop
	mov	2721, %o0
	call	_m3_fault, 0
	 nop
.LL173:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL175
	 nop
	cmp	%g1, 0
	bl	%icc, .LL174
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL178
	 nop
.LL174:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL177
	 nop
.LL175:
	cmp	%g1, 0
	bg	%icc, .LL176
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL178
	 nop
.LL176:
	add	%g1, -1, %g1
.LL177:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL178:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE31:
	.size	Mod__Mod_param_u16_C, .-Mod__Mod_param_u16_C
	.align 4
	.type	Mod__uMod_var_u16_u8, #function
	.proc	04
Mod__uMod_var_u16_u8:
.LLFB32:
	save	%sp, -96, %sp
.LLCFI32:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE32:
	.size	Mod__uMod_var_u16_u8, .-Mod__uMod_var_u16_u8
	.align 4
	.type	Mod__Mod_var_u16_u8, #function
	.proc	015
Mod__Mod_var_u16_u8:
.LLFB33:
	save	%sp, -96, %sp
.LLCFI33:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL182
	 nop
	cmp	%g1, 0
	bl	%icc, .LL181
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL185
	 nop
.LL181:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL184
	 nop
.LL182:
	cmp	%g1, 0
	bg	%icc, .LL183
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL185
	 nop
.LL183:
	add	%g1, -1, %g1
.LL184:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL185:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE33:
	.size	Mod__Mod_var_u16_u8, .-Mod__Mod_var_u16_u8
	.align 4
	.type	Mod__uMod_param_u16_u8, #function
	.proc	04
Mod__uMod_param_u16_u8:
.LLFB34:
	save	%sp, -96, %sp
.LLCFI34:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE34:
	.size	Mod__uMod_param_u16_u8, .-Mod__uMod_param_u16_u8
	.align 4
	.type	Mod__Mod_param_u16_u8, #function
	.proc	015
Mod__Mod_param_u16_u8:
.LLFB35:
	save	%sp, -96, %sp
.LLCFI35:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL189
	 nop
	cmp	%g1, 0
	bl	%icc, .LL188
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL192
	 nop
.LL188:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL191
	 nop
.LL189:
	cmp	%g1, 0
	bg	%icc, .LL190
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL192
	 nop
.LL190:
	add	%g1, -1, %g1
.LL191:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL192:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE35:
	.size	Mod__Mod_param_u16_u8, .-Mod__Mod_param_u16_u8
	.align 4
	.type	Mod__uMod_var_u16_L, #function
	.proc	05
Mod__uMod_var_u16_L:
.LLFB36:
	save	%sp, -96, %sp
.LLCFI36:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE36:
	.size	Mod__uMod_var_u16_L, .-Mod__uMod_var_u16_L
	.align 4
	.type	Mod__Mod_var_u16_L, #function
	.proc	05
Mod__Mod_var_u16_L:
.LLFB37:
	save	%sp, -96, %sp
.LLCFI37:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL196
	 nop
	cmp	%l0, 0
	bne	%icc, .LL200
	 nop
	cmp	%l1, 0
.LL200:
	cmp	%o4, 0
	bl	%icc, .LL195
	 nop
	cmp	%o4, 0
	bne	%icc, .LL201
	 nop
	cmp	%o5, 0
.LL201:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL199
	 nop
.LL195:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL198
	 nop
.LL196:
	cmp	%o4, 0
	bg	%icc, .LL197
	 nop
	cmp	%o4, 0
	bne	%icc, .LL202
	 nop
	cmp	%o5, 0
	bne	%icc, .LL197
	 nop
	cmp	%o5, 0
.LL202:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL199
	 nop
.LL197:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL198:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL199:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE37:
	.size	Mod__Mod_var_u16_L, .-Mod__Mod_var_u16_L
	.align 4
	.type	Mod__uMod_param_u16_L, #function
	.proc	05
Mod__uMod_param_u16_L:
.LLFB38:
	save	%sp, -104, %sp
.LLCFI38:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE38:
	.size	Mod__uMod_param_u16_L, .-Mod__uMod_param_u16_L
	.align 4
	.type	Mod__Mod_param_u16_L, #function
	.proc	05
Mod__Mod_param_u16_L:
.LLFB39:
	save	%sp, -104, %sp
.LLCFI39:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL206
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL210
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL210:
	cmp	%o4, 0
	bl	%icc, .LL205
	 nop
	cmp	%o4, 0
	bne	%icc, .LL211
	 nop
	cmp	%o5, 0
.LL211:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL209
	 nop
.LL205:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL208
	 nop
.LL206:
	cmp	%o4, 0
	bg	%icc, .LL207
	 nop
	cmp	%o4, 0
	bne	%icc, .LL212
	 nop
	cmp	%o5, 0
	bne	%icc, .LL207
	 nop
	cmp	%o5, 0
.LL212:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL209
	 nop
.LL207:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL208:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL209:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE39:
	.size	Mod__Mod_param_u16_L, .-Mod__Mod_param_u16_L
	.align 4
	.type	Mod__uMod_var_u16_i8, #function
	.proc	04
Mod__uMod_var_u16_i8:
.LLFB40:
	save	%sp, -96, %sp
.LLCFI40:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE40:
	.size	Mod__uMod_var_u16_i8, .-Mod__uMod_var_u16_i8
	.align 4
	.type	Mod__Mod_var_u16_i8, #function
	.proc	015
Mod__Mod_var_u16_i8:
.LLFB41:
	save	%sp, -96, %sp
.LLCFI41:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL216
	 nop
	cmp	%g1, 0
	bl	%icc, .LL215
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL219
	 nop
.LL215:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL218
	 nop
.LL216:
	cmp	%g1, 0
	bg	%icc, .LL217
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL219
	 nop
.LL217:
	add	%g1, -1, %g1
.LL218:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL219:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL220
	 nop
	mov	3041, %o0
	call	_m3_fault, 0
	 nop
.LL220:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL222
	 nop
	cmp	%g1, 0
	bl	%icc, .LL221
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL225
	 nop
.LL221:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL224
	 nop
.LL222:
	cmp	%g1, 0
	bg	%icc, .LL223
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL225
	 nop
.LL223:
	add	%g1, -1, %g1
.LL224:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL225:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE41:
	.size	Mod__Mod_var_u16_i8, .-Mod__Mod_var_u16_i8
	.align 4
	.type	Mod__uMod_param_u16_i8, #function
	.proc	04
Mod__uMod_param_u16_i8:
.LLFB42:
	save	%sp, -96, %sp
.LLCFI42:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE42:
	.size	Mod__uMod_param_u16_i8, .-Mod__uMod_param_u16_i8
	.align 4
	.type	Mod__Mod_param_u16_i8, #function
	.proc	015
Mod__Mod_param_u16_i8:
.LLFB43:
	save	%sp, -96, %sp
.LLCFI43:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL229
	 nop
	cmp	%g1, 0
	bl	%icc, .LL228
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL232
	 nop
.LL228:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL231
	 nop
.LL229:
	cmp	%g1, 0
	bg	%icc, .LL230
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL232
	 nop
.LL230:
	add	%g1, -1, %g1
.LL231:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL232:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL233
	 nop
	mov	3105, %o0
	call	_m3_fault, 0
	 nop
.LL233:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL235
	 nop
	cmp	%g1, 0
	bl	%icc, .LL234
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL238
	 nop
.LL234:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL237
	 nop
.LL235:
	cmp	%g1, 0
	bg	%icc, .LL236
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL238
	 nop
.LL236:
	add	%g1, -1, %g1
.LL237:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL238:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE43:
	.size	Mod__Mod_param_u16_i8, .-Mod__Mod_param_u16_i8
	.align 4
	.type	Mod__uMod_var_u16_u32, #function
	.proc	04
Mod__uMod_var_u16_u32:
.LLFB44:
	save	%sp, -96, %sp
.LLCFI44:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE44:
	.size	Mod__uMod_var_u16_u32, .-Mod__uMod_var_u16_u32
	.align 4
	.type	Mod__Mod_var_u16_u32, #function
	.proc	015
Mod__Mod_var_u16_u32:
.LLFB45:
	save	%sp, -96, %sp
.LLCFI45:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL242
	 nop
	cmp	%g1, 0
	bl	%icc, .LL241
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL245
	 nop
.LL241:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL244
	 nop
.LL242:
	cmp	%g1, 0
	bg	%icc, .LL243
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL245
	 nop
.LL243:
	add	%g1, -1, %g1
.LL244:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL245:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL246
	 nop
	mov	3169, %o0
	call	_m3_fault, 0
	 nop
.LL246:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL248
	 nop
	cmp	%g1, 0
	bl	%icc, .LL247
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL251
	 nop
.LL247:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL250
	 nop
.LL248:
	cmp	%g1, 0
	bg	%icc, .LL249
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL251
	 nop
.LL249:
	add	%g1, -1, %g1
.LL250:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL251:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE45:
	.size	Mod__Mod_var_u16_u32, .-Mod__Mod_var_u16_u32
	.align 4
	.type	Mod__uMod_param_u16_u32, #function
	.proc	04
Mod__uMod_param_u16_u32:
.LLFB46:
	save	%sp, -96, %sp
.LLCFI46:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE46:
	.size	Mod__uMod_param_u16_u32, .-Mod__uMod_param_u16_u32
	.align 4
	.type	Mod__Mod_param_u16_u32, #function
	.proc	015
Mod__Mod_param_u16_u32:
.LLFB47:
	save	%sp, -96, %sp
.LLCFI47:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL255
	 nop
	cmp	%g1, 0
	bl	%icc, .LL254
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL258
	 nop
.LL254:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL257
	 nop
.LL255:
	cmp	%g1, 0
	bg	%icc, .LL256
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL258
	 nop
.LL256:
	add	%g1, -1, %g1
.LL257:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL258:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(64512), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL259
	 nop
	mov	3233, %o0
	call	_m3_fault, 0
	 nop
.LL259:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL261
	 nop
	cmp	%g1, 0
	bl	%icc, .LL260
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL264
	 nop
.LL260:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL263
	 nop
.LL261:
	cmp	%g1, 0
	bg	%icc, .LL262
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL264
	 nop
.LL262:
	add	%g1, -1, %g1
.LL263:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL264:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE47:
	.size	Mod__Mod_param_u16_u32, .-Mod__Mod_param_u16_u32
	.align 4
	.type	Mod__uMod_var_u64_u16, #function
	.proc	05
Mod__uMod_var_u64_u16:
.LLFB48:
	save	%sp, -96, %sp
.LLCFI48:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE48:
	.size	Mod__uMod_var_u64_u16, .-Mod__uMod_var_u64_u16
	.align 4
	.type	Mod__Mod_var_u64_u16, #function
	.proc	05
Mod__Mod_var_u64_u16:
.LLFB49:
	save	%sp, -96, %sp
.LLCFI49:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL268
	 nop
	cmp	%l0, 0
	bne	%icc, .LL272
	 nop
	cmp	%l1, 0
.LL272:
	cmp	%o4, 0
	bl	%icc, .LL267
	 nop
	cmp	%o4, 0
	bne	%icc, .LL273
	 nop
	cmp	%o5, 0
.LL273:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL271
	 nop
.LL267:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL270
	 nop
.LL268:
	cmp	%o4, 0
	bg	%icc, .LL269
	 nop
	cmp	%o4, 0
	bne	%icc, .LL274
	 nop
	cmp	%o5, 0
	bne	%icc, .LL269
	 nop
	cmp	%o5, 0
.LL274:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL271
	 nop
.LL269:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL270:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL271:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE49:
	.size	Mod__Mod_var_u64_u16, .-Mod__Mod_var_u64_u16
	.align 4
	.type	Mod__uMod_param_u64_u16, #function
	.proc	05
Mod__uMod_param_u64_u16:
.LLFB50:
	save	%sp, -104, %sp
.LLCFI50:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE50:
	.size	Mod__uMod_param_u64_u16, .-Mod__uMod_param_u64_u16
	.align 4
	.type	Mod__Mod_param_u64_u16, #function
	.proc	05
Mod__Mod_param_u64_u16:
.LLFB51:
	save	%sp, -104, %sp
.LLCFI51:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL278
	 nop
	cmp	%l0, 0
	bne	%icc, .LL282
	 nop
	cmp	%l1, 0
.LL282:
	cmp	%o4, 0
	bl	%icc, .LL277
	 nop
	cmp	%o4, 0
	bne	%icc, .LL283
	 nop
	cmp	%o5, 0
.LL283:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL281
	 nop
.LL277:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL280
	 nop
.LL278:
	cmp	%o4, 0
	bg	%icc, .LL279
	 nop
	cmp	%o4, 0
	bne	%icc, .LL284
	 nop
	cmp	%o5, 0
	bne	%icc, .LL279
	 nop
	cmp	%o5, 0
.LL284:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL281
	 nop
.LL279:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL280:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL281:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE51:
	.size	Mod__Mod_param_u64_u16, .-Mod__Mod_param_u64_u16
	.align 4
	.type	Mod__uMod_var_u64_u64, #function
	.proc	05
Mod__uMod_var_u64_u64:
.LLFB52:
	save	%sp, -96, %sp
.LLCFI52:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE52:
	.size	Mod__uMod_var_u64_u64, .-Mod__uMod_var_u64_u64
	.align 4
	.type	Mod__Mod_var_u64_u64, #function
	.proc	05
Mod__Mod_var_u64_u64:
.LLFB53:
	save	%sp, -96, %sp
.LLCFI53:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL288
	 nop
	cmp	%l0, 0
	bne	%icc, .LL292
	 nop
	cmp	%l1, 0
.LL292:
	cmp	%o4, 0
	bl	%icc, .LL287
	 nop
	cmp	%o4, 0
	bne	%icc, .LL293
	 nop
	cmp	%o5, 0
.LL293:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL291
	 nop
.LL287:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL290
	 nop
.LL288:
	cmp	%o4, 0
	bg	%icc, .LL289
	 nop
	cmp	%o4, 0
	bne	%icc, .LL294
	 nop
	cmp	%o5, 0
	bne	%icc, .LL289
	 nop
	cmp	%o5, 0
.LL294:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL291
	 nop
.LL289:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL290:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL291:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE53:
	.size	Mod__Mod_var_u64_u64, .-Mod__Mod_var_u64_u64
	.align 4
	.type	Mod__uMod_param_u64_u64, #function
	.proc	05
Mod__uMod_param_u64_u64:
.LLFB54:
	save	%sp, -112, %sp
.LLCFI54:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE54:
	.size	Mod__uMod_param_u64_u64, .-Mod__uMod_param_u64_u64
	.align 4
	.type	Mod__Mod_param_u64_u64, #function
	.proc	05
Mod__Mod_param_u64_u64:
.LLFB55:
	save	%sp, -112, %sp
.LLCFI55:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL298
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL302
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL302:
	cmp	%o4, 0
	bl	%icc, .LL297
	 nop
	cmp	%o4, 0
	bne	%icc, .LL303
	 nop
	cmp	%o5, 0
.LL303:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL301
	 nop
.LL297:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL300
	 nop
.LL298:
	cmp	%o4, 0
	bg	%icc, .LL299
	 nop
	cmp	%o4, 0
	bne	%icc, .LL304
	 nop
	cmp	%o5, 0
	bne	%icc, .LL299
	 nop
	cmp	%o5, 0
.LL304:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL301
	 nop
.LL299:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL300:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL301:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE55:
	.size	Mod__Mod_param_u64_u64, .-Mod__Mod_param_u64_u64
	.align 4
	.type	Mod__uMod_var_u64_LC, #function
	.proc	05
Mod__uMod_var_u64_LC:
.LLFB56:
	save	%sp, -96, %sp
.LLCFI56:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE56:
	.size	Mod__uMod_var_u64_LC, .-Mod__uMod_var_u64_LC
	.align 4
	.type	Mod__Mod_var_u64_LC, #function
	.proc	05
Mod__Mod_var_u64_LC:
.LLFB57:
	save	%sp, -96, %sp
.LLCFI57:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL308
	 nop
	cmp	%l0, 0
	bne	%icc, .LL312
	 nop
	cmp	%l1, 0
.LL312:
	cmp	%o4, 0
	bl	%icc, .LL307
	 nop
	cmp	%o4, 0
	bne	%icc, .LL313
	 nop
	cmp	%o5, 0
.LL313:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL311
	 nop
.LL307:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL310
	 nop
.LL308:
	cmp	%o4, 0
	bg	%icc, .LL309
	 nop
	cmp	%o4, 0
	bne	%icc, .LL314
	 nop
	cmp	%o5, 0
	bne	%icc, .LL309
	 nop
	cmp	%o5, 0
.LL314:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL311
	 nop
.LL309:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL310:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL311:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE57:
	.size	Mod__Mod_var_u64_LC, .-Mod__Mod_var_u64_LC
	.align 4
	.type	Mod__uMod_param_u64_LC, #function
	.proc	05
Mod__uMod_param_u64_LC:
.LLFB58:
	save	%sp, -112, %sp
.LLCFI58:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE58:
	.size	Mod__uMod_param_u64_LC, .-Mod__uMod_param_u64_LC
	.align 4
	.type	Mod__Mod_param_u64_LC, #function
	.proc	05
Mod__Mod_param_u64_LC:
.LLFB59:
	save	%sp, -112, %sp
.LLCFI59:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL318
	 nop
	cmp	%l0, 0
	bne	%icc, .LL322
	 nop
	cmp	%l1, 0
.LL322:
	cmp	%o4, 0
	bl	%icc, .LL317
	 nop
	cmp	%o4, 0
	bne	%icc, .LL323
	 nop
	cmp	%o5, 0
.LL323:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL321
	 nop
.LL317:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL320
	 nop
.LL318:
	cmp	%o4, 0
	bg	%icc, .LL319
	 nop
	cmp	%o4, 0
	bne	%icc, .LL324
	 nop
	cmp	%o5, 0
	bne	%icc, .LL319
	 nop
	cmp	%o5, 0
.LL324:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL321
	 nop
.LL319:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL320:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL321:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE59:
	.size	Mod__Mod_param_u64_LC, .-Mod__Mod_param_u64_LC
	.align 4
	.type	Mod__uMod_var_u64_i32, #function
	.proc	05
Mod__uMod_var_u64_i32:
.LLFB60:
	save	%sp, -96, %sp
.LLCFI60:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE60:
	.size	Mod__uMod_var_u64_i32, .-Mod__uMod_var_u64_i32
	.align 4
	.type	Mod__Mod_var_u64_i32, #function
	.proc	05
Mod__Mod_var_u64_i32:
.LLFB61:
	save	%sp, -96, %sp
.LLCFI61:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL328
	 nop
	cmp	%l0, 0
	bne	%icc, .LL332
	 nop
	cmp	%l1, 0
.LL332:
	cmp	%o4, 0
	bl	%icc, .LL327
	 nop
	cmp	%o4, 0
	bne	%icc, .LL333
	 nop
	cmp	%o5, 0
.LL333:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL331
	 nop
.LL327:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL330
	 nop
.LL328:
	cmp	%o4, 0
	bg	%icc, .LL329
	 nop
	cmp	%o4, 0
	bne	%icc, .LL334
	 nop
	cmp	%o5, 0
	bne	%icc, .LL329
	 nop
	cmp	%o5, 0
.LL334:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL331
	 nop
.LL329:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL330:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL331:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE61:
	.size	Mod__Mod_var_u64_i32, .-Mod__Mod_var_u64_i32
	.align 4
	.type	Mod__uMod_param_u64_i32, #function
	.proc	05
Mod__uMod_param_u64_i32:
.LLFB62:
	save	%sp, -104, %sp
.LLCFI62:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE62:
	.size	Mod__uMod_param_u64_i32, .-Mod__uMod_param_u64_i32
	.align 4
	.type	Mod__Mod_param_u64_i32, #function
	.proc	05
Mod__Mod_param_u64_i32:
.LLFB63:
	save	%sp, -104, %sp
.LLCFI63:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL338
	 nop
	cmp	%l0, 0
	bne	%icc, .LL342
	 nop
	cmp	%l1, 0
.LL342:
	cmp	%o4, 0
	bl	%icc, .LL337
	 nop
	cmp	%o4, 0
	bne	%icc, .LL343
	 nop
	cmp	%o5, 0
.LL343:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL341
	 nop
.LL337:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL340
	 nop
.LL338:
	cmp	%o4, 0
	bg	%icc, .LL339
	 nop
	cmp	%o4, 0
	bne	%icc, .LL344
	 nop
	cmp	%o5, 0
	bne	%icc, .LL339
	 nop
	cmp	%o5, 0
.LL344:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL341
	 nop
.LL339:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL340:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL341:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE63:
	.size	Mod__Mod_param_u64_i32, .-Mod__Mod_param_u64_i32
	.align 4
	.type	Mod__uMod_var_u64_i16, #function
	.proc	05
Mod__uMod_var_u64_i16:
.LLFB64:
	save	%sp, -96, %sp
.LLCFI64:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE64:
	.size	Mod__uMod_var_u64_i16, .-Mod__uMod_var_u64_i16
	.align 4
	.type	Mod__Mod_var_u64_i16, #function
	.proc	05
Mod__Mod_var_u64_i16:
.LLFB65:
	save	%sp, -96, %sp
.LLCFI65:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL348
	 nop
	cmp	%l0, 0
	bne	%icc, .LL352
	 nop
	cmp	%l1, 0
.LL352:
	cmp	%o4, 0
	bl	%icc, .LL347
	 nop
	cmp	%o4, 0
	bne	%icc, .LL353
	 nop
	cmp	%o5, 0
.LL353:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL351
	 nop
.LL347:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL350
	 nop
.LL348:
	cmp	%o4, 0
	bg	%icc, .LL349
	 nop
	cmp	%o4, 0
	bne	%icc, .LL354
	 nop
	cmp	%o5, 0
	bne	%icc, .LL349
	 nop
	cmp	%o5, 0
.LL354:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL351
	 nop
.LL349:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL350:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL351:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE65:
	.size	Mod__Mod_var_u64_i16, .-Mod__Mod_var_u64_i16
	.align 4
	.type	Mod__uMod_param_u64_i16, #function
	.proc	05
Mod__uMod_param_u64_i16:
.LLFB66:
	save	%sp, -104, %sp
.LLCFI66:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE66:
	.size	Mod__uMod_param_u64_i16, .-Mod__uMod_param_u64_i16
	.align 4
	.type	Mod__Mod_param_u64_i16, #function
	.proc	05
Mod__Mod_param_u64_i16:
.LLFB67:
	save	%sp, -104, %sp
.LLCFI67:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL358
	 nop
	cmp	%l0, 0
	bne	%icc, .LL362
	 nop
	cmp	%l1, 0
.LL362:
	cmp	%o4, 0
	bl	%icc, .LL357
	 nop
	cmp	%o4, 0
	bne	%icc, .LL363
	 nop
	cmp	%o5, 0
.LL363:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL361
	 nop
.LL357:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL360
	 nop
.LL358:
	cmp	%o4, 0
	bg	%icc, .LL359
	 nop
	cmp	%o4, 0
	bne	%icc, .LL364
	 nop
	cmp	%o5, 0
	bne	%icc, .LL359
	 nop
	cmp	%o5, 0
.LL364:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL361
	 nop
.LL359:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL360:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL361:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE67:
	.size	Mod__Mod_param_u64_i16, .-Mod__Mod_param_u64_i16
	.align 4
	.type	Mod__uMod_var_u64_I, #function
	.proc	05
Mod__uMod_var_u64_I:
.LLFB68:
	save	%sp, -96, %sp
.LLCFI68:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE68:
	.size	Mod__uMod_var_u64_I, .-Mod__uMod_var_u64_I
	.align 4
	.type	Mod__Mod_var_u64_I, #function
	.proc	05
Mod__Mod_var_u64_I:
.LLFB69:
	save	%sp, -96, %sp
.LLCFI69:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL368
	 nop
	cmp	%l0, 0
	bne	%icc, .LL372
	 nop
	cmp	%l1, 0
.LL372:
	cmp	%o4, 0
	bl	%icc, .LL367
	 nop
	cmp	%o4, 0
	bne	%icc, .LL373
	 nop
	cmp	%o5, 0
.LL373:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL371
	 nop
.LL367:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL370
	 nop
.LL368:
	cmp	%o4, 0
	bg	%icc, .LL369
	 nop
	cmp	%o4, 0
	bne	%icc, .LL374
	 nop
	cmp	%o5, 0
	bne	%icc, .LL369
	 nop
	cmp	%o5, 0
.LL374:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL371
	 nop
.LL369:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL370:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL371:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE69:
	.size	Mod__Mod_var_u64_I, .-Mod__Mod_var_u64_I
	.align 4
	.type	Mod__uMod_param_u64_I, #function
	.proc	05
Mod__uMod_param_u64_I:
.LLFB70:
	save	%sp, -104, %sp
.LLCFI70:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE70:
	.size	Mod__uMod_param_u64_I, .-Mod__uMod_param_u64_I
	.align 4
	.type	Mod__Mod_param_u64_I, #function
	.proc	05
Mod__Mod_param_u64_I:
.LLFB71:
	save	%sp, -104, %sp
.LLCFI71:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL378
	 nop
	cmp	%l0, 0
	bne	%icc, .LL382
	 nop
	cmp	%l1, 0
.LL382:
	cmp	%o4, 0
	bl	%icc, .LL377
	 nop
	cmp	%o4, 0
	bne	%icc, .LL383
	 nop
	cmp	%o5, 0
.LL383:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL381
	 nop
.LL377:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL380
	 nop
.LL378:
	cmp	%o4, 0
	bg	%icc, .LL379
	 nop
	cmp	%o4, 0
	bne	%icc, .LL384
	 nop
	cmp	%o5, 0
	bne	%icc, .LL379
	 nop
	cmp	%o5, 0
.LL384:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL381
	 nop
.LL379:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL380:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL381:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE71:
	.size	Mod__Mod_param_u64_I, .-Mod__Mod_param_u64_I
	.align 4
	.type	Mod__uMod_var_u64_i64, #function
	.proc	05
Mod__uMod_var_u64_i64:
.LLFB72:
	save	%sp, -96, %sp
.LLCFI72:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE72:
	.size	Mod__uMod_var_u64_i64, .-Mod__uMod_var_u64_i64
	.align 4
	.type	Mod__Mod_var_u64_i64, #function
	.proc	05
Mod__Mod_var_u64_i64:
.LLFB73:
	save	%sp, -96, %sp
.LLCFI73:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL388
	 nop
	cmp	%l0, 0
	bne	%icc, .LL392
	 nop
	cmp	%l1, 0
.LL392:
	cmp	%o4, 0
	bl	%icc, .LL387
	 nop
	cmp	%o4, 0
	bne	%icc, .LL393
	 nop
	cmp	%o5, 0
.LL393:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL391
	 nop
.LL387:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL390
	 nop
.LL388:
	cmp	%o4, 0
	bg	%icc, .LL389
	 nop
	cmp	%o4, 0
	bne	%icc, .LL394
	 nop
	cmp	%o5, 0
	bne	%icc, .LL389
	 nop
	cmp	%o5, 0
.LL394:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL391
	 nop
.LL389:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL390:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL391:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE73:
	.size	Mod__Mod_var_u64_i64, .-Mod__Mod_var_u64_i64
	.align 4
	.type	Mod__uMod_param_u64_i64, #function
	.proc	05
Mod__uMod_param_u64_i64:
.LLFB74:
	save	%sp, -112, %sp
.LLCFI74:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE74:
	.size	Mod__uMod_param_u64_i64, .-Mod__uMod_param_u64_i64
	.align 4
	.type	Mod__Mod_param_u64_i64, #function
	.proc	05
Mod__Mod_param_u64_i64:
.LLFB75:
	save	%sp, -112, %sp
.LLCFI75:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL398
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL402
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL402:
	cmp	%o4, 0
	bl	%icc, .LL397
	 nop
	cmp	%o4, 0
	bne	%icc, .LL403
	 nop
	cmp	%o5, 0
.LL403:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL401
	 nop
.LL397:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL400
	 nop
.LL398:
	cmp	%o4, 0
	bg	%icc, .LL399
	 nop
	cmp	%o4, 0
	bne	%icc, .LL404
	 nop
	cmp	%o5, 0
	bne	%icc, .LL399
	 nop
	cmp	%o5, 0
.LL404:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL401
	 nop
.LL399:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL400:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL401:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE75:
	.size	Mod__Mod_param_u64_i64, .-Mod__Mod_param_u64_i64
	.align 4
	.type	Mod__uMod_var_u64_C, #function
	.proc	05
Mod__uMod_var_u64_C:
.LLFB76:
	save	%sp, -96, %sp
.LLCFI76:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE76:
	.size	Mod__uMod_var_u64_C, .-Mod__uMod_var_u64_C
	.align 4
	.type	Mod__Mod_var_u64_C, #function
	.proc	05
Mod__Mod_var_u64_C:
.LLFB77:
	save	%sp, -96, %sp
.LLCFI77:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL408
	 nop
	cmp	%l0, 0
	bne	%icc, .LL412
	 nop
	cmp	%l1, 0
.LL412:
	cmp	%o4, 0
	bl	%icc, .LL407
	 nop
	cmp	%o4, 0
	bne	%icc, .LL413
	 nop
	cmp	%o5, 0
.LL413:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL411
	 nop
.LL407:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL410
	 nop
.LL408:
	cmp	%o4, 0
	bg	%icc, .LL409
	 nop
	cmp	%o4, 0
	bne	%icc, .LL414
	 nop
	cmp	%o5, 0
	bne	%icc, .LL409
	 nop
	cmp	%o5, 0
.LL414:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL411
	 nop
.LL409:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL410:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL411:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE77:
	.size	Mod__Mod_var_u64_C, .-Mod__Mod_var_u64_C
	.align 4
	.type	Mod__uMod_param_u64_C, #function
	.proc	05
Mod__uMod_param_u64_C:
.LLFB78:
	save	%sp, -104, %sp
.LLCFI78:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE78:
	.size	Mod__uMod_param_u64_C, .-Mod__uMod_param_u64_C
	.align 4
	.type	Mod__Mod_param_u64_C, #function
	.proc	05
Mod__Mod_param_u64_C:
.LLFB79:
	save	%sp, -104, %sp
.LLCFI79:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL418
	 nop
	cmp	%l0, 0
	bne	%icc, .LL422
	 nop
	cmp	%l1, 0
.LL422:
	cmp	%o4, 0
	bl	%icc, .LL417
	 nop
	cmp	%o4, 0
	bne	%icc, .LL423
	 nop
	cmp	%o5, 0
.LL423:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL421
	 nop
.LL417:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL420
	 nop
.LL418:
	cmp	%o4, 0
	bg	%icc, .LL419
	 nop
	cmp	%o4, 0
	bne	%icc, .LL424
	 nop
	cmp	%o5, 0
	bne	%icc, .LL419
	 nop
	cmp	%o5, 0
.LL424:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL421
	 nop
.LL419:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL420:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL421:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE79:
	.size	Mod__Mod_param_u64_C, .-Mod__Mod_param_u64_C
	.align 4
	.type	Mod__uMod_var_u64_u8, #function
	.proc	05
Mod__uMod_var_u64_u8:
.LLFB80:
	save	%sp, -96, %sp
.LLCFI80:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE80:
	.size	Mod__uMod_var_u64_u8, .-Mod__uMod_var_u64_u8
	.align 4
	.type	Mod__Mod_var_u64_u8, #function
	.proc	05
Mod__Mod_var_u64_u8:
.LLFB81:
	save	%sp, -96, %sp
.LLCFI81:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL428
	 nop
	cmp	%l0, 0
	bne	%icc, .LL432
	 nop
	cmp	%l1, 0
.LL432:
	cmp	%o4, 0
	bl	%icc, .LL427
	 nop
	cmp	%o4, 0
	bne	%icc, .LL433
	 nop
	cmp	%o5, 0
.LL433:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL431
	 nop
.LL427:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL430
	 nop
.LL428:
	cmp	%o4, 0
	bg	%icc, .LL429
	 nop
	cmp	%o4, 0
	bne	%icc, .LL434
	 nop
	cmp	%o5, 0
	bne	%icc, .LL429
	 nop
	cmp	%o5, 0
.LL434:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL431
	 nop
.LL429:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL430:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL431:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE81:
	.size	Mod__Mod_var_u64_u8, .-Mod__Mod_var_u64_u8
	.align 4
	.type	Mod__uMod_param_u64_u8, #function
	.proc	05
Mod__uMod_param_u64_u8:
.LLFB82:
	save	%sp, -104, %sp
.LLCFI82:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE82:
	.size	Mod__uMod_param_u64_u8, .-Mod__uMod_param_u64_u8
	.align 4
	.type	Mod__Mod_param_u64_u8, #function
	.proc	05
Mod__Mod_param_u64_u8:
.LLFB83:
	save	%sp, -104, %sp
.LLCFI83:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL438
	 nop
	cmp	%l0, 0
	bne	%icc, .LL442
	 nop
	cmp	%l1, 0
.LL442:
	cmp	%o4, 0
	bl	%icc, .LL437
	 nop
	cmp	%o4, 0
	bne	%icc, .LL443
	 nop
	cmp	%o5, 0
.LL443:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL441
	 nop
.LL437:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL440
	 nop
.LL438:
	cmp	%o4, 0
	bg	%icc, .LL439
	 nop
	cmp	%o4, 0
	bne	%icc, .LL444
	 nop
	cmp	%o5, 0
	bne	%icc, .LL439
	 nop
	cmp	%o5, 0
.LL444:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL441
	 nop
.LL439:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL440:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL441:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE83:
	.size	Mod__Mod_param_u64_u8, .-Mod__Mod_param_u64_u8
	.align 4
	.type	Mod__uMod_var_u64_L, #function
	.proc	05
Mod__uMod_var_u64_L:
.LLFB84:
	save	%sp, -96, %sp
.LLCFI84:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE84:
	.size	Mod__uMod_var_u64_L, .-Mod__uMod_var_u64_L
	.align 4
	.type	Mod__Mod_var_u64_L, #function
	.proc	05
Mod__Mod_var_u64_L:
.LLFB85:
	save	%sp, -96, %sp
.LLCFI85:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL448
	 nop
	cmp	%l0, 0
	bne	%icc, .LL452
	 nop
	cmp	%l1, 0
.LL452:
	cmp	%o4, 0
	bl	%icc, .LL447
	 nop
	cmp	%o4, 0
	bne	%icc, .LL453
	 nop
	cmp	%o5, 0
.LL453:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL451
	 nop
.LL447:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL450
	 nop
.LL448:
	cmp	%o4, 0
	bg	%icc, .LL449
	 nop
	cmp	%o4, 0
	bne	%icc, .LL454
	 nop
	cmp	%o5, 0
	bne	%icc, .LL449
	 nop
	cmp	%o5, 0
.LL454:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL451
	 nop
.LL449:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL450:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL451:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE85:
	.size	Mod__Mod_var_u64_L, .-Mod__Mod_var_u64_L
	.align 4
	.type	Mod__uMod_param_u64_L, #function
	.proc	05
Mod__uMod_param_u64_L:
.LLFB86:
	save	%sp, -112, %sp
.LLCFI86:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE86:
	.size	Mod__uMod_param_u64_L, .-Mod__uMod_param_u64_L
	.align 4
	.type	Mod__Mod_param_u64_L, #function
	.proc	05
Mod__Mod_param_u64_L:
.LLFB87:
	save	%sp, -112, %sp
.LLCFI87:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL458
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL462
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL462:
	cmp	%o4, 0
	bl	%icc, .LL457
	 nop
	cmp	%o4, 0
	bne	%icc, .LL463
	 nop
	cmp	%o5, 0
.LL463:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL461
	 nop
.LL457:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL460
	 nop
.LL458:
	cmp	%o4, 0
	bg	%icc, .LL459
	 nop
	cmp	%o4, 0
	bne	%icc, .LL464
	 nop
	cmp	%o5, 0
	bne	%icc, .LL459
	 nop
	cmp	%o5, 0
.LL464:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL461
	 nop
.LL459:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL460:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL461:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE87:
	.size	Mod__Mod_param_u64_L, .-Mod__Mod_param_u64_L
	.align 4
	.type	Mod__uMod_var_u64_i8, #function
	.proc	05
Mod__uMod_var_u64_i8:
.LLFB88:
	save	%sp, -96, %sp
.LLCFI88:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE88:
	.size	Mod__uMod_var_u64_i8, .-Mod__uMod_var_u64_i8
	.align 4
	.type	Mod__Mod_var_u64_i8, #function
	.proc	05
Mod__Mod_var_u64_i8:
.LLFB89:
	save	%sp, -96, %sp
.LLCFI89:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL468
	 nop
	cmp	%l0, 0
	bne	%icc, .LL472
	 nop
	cmp	%l1, 0
.LL472:
	cmp	%o4, 0
	bl	%icc, .LL467
	 nop
	cmp	%o4, 0
	bne	%icc, .LL473
	 nop
	cmp	%o5, 0
.LL473:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL471
	 nop
.LL467:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL470
	 nop
.LL468:
	cmp	%o4, 0
	bg	%icc, .LL469
	 nop
	cmp	%o4, 0
	bne	%icc, .LL474
	 nop
	cmp	%o5, 0
	bne	%icc, .LL469
	 nop
	cmp	%o5, 0
.LL474:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL471
	 nop
.LL469:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL470:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL471:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE89:
	.size	Mod__Mod_var_u64_i8, .-Mod__Mod_var_u64_i8
	.align 4
	.type	Mod__uMod_param_u64_i8, #function
	.proc	05
Mod__uMod_param_u64_i8:
.LLFB90:
	save	%sp, -104, %sp
.LLCFI90:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE90:
	.size	Mod__uMod_param_u64_i8, .-Mod__uMod_param_u64_i8
	.align 4
	.type	Mod__Mod_param_u64_i8, #function
	.proc	05
Mod__Mod_param_u64_i8:
.LLFB91:
	save	%sp, -104, %sp
.LLCFI91:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL478
	 nop
	cmp	%l0, 0
	bne	%icc, .LL482
	 nop
	cmp	%l1, 0
.LL482:
	cmp	%o4, 0
	bl	%icc, .LL477
	 nop
	cmp	%o4, 0
	bne	%icc, .LL483
	 nop
	cmp	%o5, 0
.LL483:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL481
	 nop
.LL477:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL480
	 nop
.LL478:
	cmp	%o4, 0
	bg	%icc, .LL479
	 nop
	cmp	%o4, 0
	bne	%icc, .LL484
	 nop
	cmp	%o5, 0
	bne	%icc, .LL479
	 nop
	cmp	%o5, 0
.LL484:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL481
	 nop
.LL479:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL480:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL481:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE91:
	.size	Mod__Mod_param_u64_i8, .-Mod__Mod_param_u64_i8
	.align 4
	.type	Mod__uMod_var_u64_u32, #function
	.proc	05
Mod__uMod_var_u64_u32:
.LLFB92:
	save	%sp, -96, %sp
.LLCFI92:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE92:
	.size	Mod__uMod_var_u64_u32, .-Mod__uMod_var_u64_u32
	.align 4
	.type	Mod__Mod_var_u64_u32, #function
	.proc	05
Mod__Mod_var_u64_u32:
.LLFB93:
	save	%sp, -96, %sp
.LLCFI93:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL488
	 nop
	cmp	%l0, 0
	bne	%icc, .LL492
	 nop
	cmp	%l1, 0
.LL492:
	cmp	%o4, 0
	bl	%icc, .LL487
	 nop
	cmp	%o4, 0
	bne	%icc, .LL493
	 nop
	cmp	%o5, 0
.LL493:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL491
	 nop
.LL487:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL490
	 nop
.LL488:
	cmp	%o4, 0
	bg	%icc, .LL489
	 nop
	cmp	%o4, 0
	bne	%icc, .LL494
	 nop
	cmp	%o5, 0
	bne	%icc, .LL489
	 nop
	cmp	%o5, 0
.LL494:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL491
	 nop
.LL489:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL490:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL491:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE93:
	.size	Mod__Mod_var_u64_u32, .-Mod__Mod_var_u64_u32
	.align 4
	.type	Mod__uMod_param_u64_u32, #function
	.proc	05
Mod__uMod_param_u64_u32:
.LLFB94:
	save	%sp, -104, %sp
.LLCFI94:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE94:
	.size	Mod__uMod_param_u64_u32, .-Mod__uMod_param_u64_u32
	.align 4
	.type	Mod__Mod_param_u64_u32, #function
	.proc	05
Mod__Mod_param_u64_u32:
.LLFB95:
	save	%sp, -104, %sp
.LLCFI95:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL498
	 nop
	cmp	%l0, 0
	bne	%icc, .LL502
	 nop
	cmp	%l1, 0
.LL502:
	cmp	%o4, 0
	bl	%icc, .LL497
	 nop
	cmp	%o4, 0
	bne	%icc, .LL503
	 nop
	cmp	%o5, 0
.LL503:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL501
	 nop
.LL497:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL500
	 nop
.LL498:
	cmp	%o4, 0
	bg	%icc, .LL499
	 nop
	cmp	%o4, 0
	bne	%icc, .LL504
	 nop
	cmp	%o5, 0
	bne	%icc, .LL499
	 nop
	cmp	%o5, 0
.LL504:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL501
	 nop
.LL499:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL500:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL501:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE95:
	.size	Mod__Mod_param_u64_u32, .-Mod__Mod_param_u64_u32
	.align 4
	.type	Mod__uMod_var_LC_u16, #function
	.proc	05
Mod__uMod_var_LC_u16:
.LLFB96:
	save	%sp, -96, %sp
.LLCFI96:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE96:
	.size	Mod__uMod_var_LC_u16, .-Mod__uMod_var_LC_u16
	.align 4
	.type	Mod__Mod_var_LC_u16, #function
	.proc	05
Mod__Mod_var_LC_u16:
.LLFB97:
	save	%sp, -96, %sp
.LLCFI97:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL508
	 nop
	cmp	%l0, 0
	bne	%icc, .LL512
	 nop
	cmp	%l1, 0
.LL512:
	cmp	%o4, 0
	bl	%icc, .LL507
	 nop
	cmp	%o4, 0
	bne	%icc, .LL513
	 nop
	cmp	%o5, 0
.LL513:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL511
	 nop
.LL507:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL510
	 nop
.LL508:
	cmp	%o4, 0
	bg	%icc, .LL509
	 nop
	cmp	%o4, 0
	bne	%icc, .LL514
	 nop
	cmp	%o5, 0
	bne	%icc, .LL509
	 nop
	cmp	%o5, 0
.LL514:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL511
	 nop
.LL509:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL510:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL511:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE97:
	.size	Mod__Mod_var_LC_u16, .-Mod__Mod_var_LC_u16
	.align 4
	.type	Mod__uMod_param_LC_u16, #function
	.proc	05
Mod__uMod_param_LC_u16:
.LLFB98:
	save	%sp, -104, %sp
.LLCFI98:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE98:
	.size	Mod__uMod_param_LC_u16, .-Mod__uMod_param_LC_u16
	.align 4
	.type	Mod__Mod_param_LC_u16, #function
	.proc	05
Mod__Mod_param_LC_u16:
.LLFB99:
	save	%sp, -104, %sp
.LLCFI99:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL518
	 nop
	cmp	%l0, 0
	bne	%icc, .LL522
	 nop
	cmp	%l1, 0
.LL522:
	cmp	%o4, 0
	bl	%icc, .LL517
	 nop
	cmp	%o4, 0
	bne	%icc, .LL523
	 nop
	cmp	%o5, 0
.LL523:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL521
	 nop
.LL517:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL520
	 nop
.LL518:
	cmp	%o4, 0
	bg	%icc, .LL519
	 nop
	cmp	%o4, 0
	bne	%icc, .LL524
	 nop
	cmp	%o5, 0
	bne	%icc, .LL519
	 nop
	cmp	%o5, 0
.LL524:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL521
	 nop
.LL519:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL520:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL521:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE99:
	.size	Mod__Mod_param_LC_u16, .-Mod__Mod_param_LC_u16
	.align 4
	.type	Mod__uMod_var_LC_u64, #function
	.proc	05
Mod__uMod_var_LC_u64:
.LLFB100:
	save	%sp, -96, %sp
.LLCFI100:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE100:
	.size	Mod__uMod_var_LC_u64, .-Mod__uMod_var_LC_u64
	.align 4
	.type	Mod__Mod_var_LC_u64, #function
	.proc	05
Mod__Mod_var_LC_u64:
.LLFB101:
	save	%sp, -96, %sp
.LLCFI101:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL528
	 nop
	cmp	%l0, 0
	bne	%icc, .LL532
	 nop
	cmp	%l1, 0
.LL532:
	cmp	%o4, 0
	bl	%icc, .LL527
	 nop
	cmp	%o4, 0
	bne	%icc, .LL533
	 nop
	cmp	%o5, 0
.LL533:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL531
	 nop
.LL527:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL530
	 nop
.LL528:
	cmp	%o4, 0
	bg	%icc, .LL529
	 nop
	cmp	%o4, 0
	bne	%icc, .LL534
	 nop
	cmp	%o5, 0
	bne	%icc, .LL529
	 nop
	cmp	%o5, 0
.LL534:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL531
	 nop
.LL529:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL530:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL531:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE101:
	.size	Mod__Mod_var_LC_u64, .-Mod__Mod_var_LC_u64
	.align 4
	.type	Mod__uMod_param_LC_u64, #function
	.proc	05
Mod__uMod_param_LC_u64:
.LLFB102:
	save	%sp, -112, %sp
.LLCFI102:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE102:
	.size	Mod__uMod_param_LC_u64, .-Mod__uMod_param_LC_u64
	.align 4
	.type	Mod__Mod_param_LC_u64, #function
	.proc	05
Mod__Mod_param_LC_u64:
.LLFB103:
	save	%sp, -112, %sp
.LLCFI103:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL538
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL542
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL542:
	cmp	%o4, 0
	bl	%icc, .LL537
	 nop
	cmp	%o4, 0
	bne	%icc, .LL543
	 nop
	cmp	%o5, 0
.LL543:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL541
	 nop
.LL537:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL540
	 nop
.LL538:
	cmp	%o4, 0
	bg	%icc, .LL539
	 nop
	cmp	%o4, 0
	bne	%icc, .LL544
	 nop
	cmp	%o5, 0
	bne	%icc, .LL539
	 nop
	cmp	%o5, 0
.LL544:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL541
	 nop
.LL539:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL540:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL541:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE103:
	.size	Mod__Mod_param_LC_u64, .-Mod__Mod_param_LC_u64
	.align 4
	.type	Mod__uMod_var_LC_LC, #function
	.proc	05
Mod__uMod_var_LC_LC:
.LLFB104:
	save	%sp, -96, %sp
.LLCFI104:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE104:
	.size	Mod__uMod_var_LC_LC, .-Mod__uMod_var_LC_LC
	.align 4
	.type	Mod__Mod_var_LC_LC, #function
	.proc	05
Mod__Mod_var_LC_LC:
.LLFB105:
	save	%sp, -96, %sp
.LLCFI105:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL548
	 nop
	cmp	%l0, 0
	bne	%icc, .LL552
	 nop
	cmp	%l1, 0
.LL552:
	cmp	%o4, 0
	bl	%icc, .LL547
	 nop
	cmp	%o4, 0
	bne	%icc, .LL553
	 nop
	cmp	%o5, 0
.LL553:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL551
	 nop
.LL547:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL550
	 nop
.LL548:
	cmp	%o4, 0
	bg	%icc, .LL549
	 nop
	cmp	%o4, 0
	bne	%icc, .LL554
	 nop
	cmp	%o5, 0
	bne	%icc, .LL549
	 nop
	cmp	%o5, 0
.LL554:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL551
	 nop
.LL549:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL550:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL551:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE105:
	.size	Mod__Mod_var_LC_LC, .-Mod__Mod_var_LC_LC
	.align 4
	.type	Mod__uMod_param_LC_LC, #function
	.proc	05
Mod__uMod_param_LC_LC:
.LLFB106:
	save	%sp, -112, %sp
.LLCFI106:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE106:
	.size	Mod__uMod_param_LC_LC, .-Mod__uMod_param_LC_LC
	.align 4
	.type	Mod__Mod_param_LC_LC, #function
	.proc	05
Mod__Mod_param_LC_LC:
.LLFB107:
	save	%sp, -112, %sp
.LLCFI107:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l2
	ldd	[%fp-16], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL558
	 nop
	cmp	%l0, 0
	bne	%icc, .LL562
	 nop
	cmp	%l1, 0
.LL562:
	cmp	%o4, 0
	bl	%icc, .LL557
	 nop
	cmp	%o4, 0
	bne	%icc, .LL563
	 nop
	cmp	%o5, 0
.LL563:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL561
	 nop
.LL557:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL560
	 nop
.LL558:
	cmp	%o4, 0
	bg	%icc, .LL559
	 nop
	cmp	%o4, 0
	bne	%icc, .LL564
	 nop
	cmp	%o5, 0
	bne	%icc, .LL559
	 nop
	cmp	%o5, 0
.LL564:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL561
	 nop
.LL559:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL560:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL561:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE107:
	.size	Mod__Mod_param_LC_LC, .-Mod__Mod_param_LC_LC
	.align 4
	.type	Mod__uMod_var_LC_i32, #function
	.proc	05
Mod__uMod_var_LC_i32:
.LLFB108:
	save	%sp, -96, %sp
.LLCFI108:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE108:
	.size	Mod__uMod_var_LC_i32, .-Mod__uMod_var_LC_i32
	.align 4
	.type	Mod__Mod_var_LC_i32, #function
	.proc	05
Mod__Mod_var_LC_i32:
.LLFB109:
	save	%sp, -96, %sp
.LLCFI109:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL568
	 nop
	cmp	%l0, 0
	bne	%icc, .LL572
	 nop
	cmp	%l1, 0
.LL572:
	cmp	%o4, 0
	bl	%icc, .LL567
	 nop
	cmp	%o4, 0
	bne	%icc, .LL573
	 nop
	cmp	%o5, 0
.LL573:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL571
	 nop
.LL567:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL570
	 nop
.LL568:
	cmp	%o4, 0
	bg	%icc, .LL569
	 nop
	cmp	%o4, 0
	bne	%icc, .LL574
	 nop
	cmp	%o5, 0
	bne	%icc, .LL569
	 nop
	cmp	%o5, 0
.LL574:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL571
	 nop
.LL569:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL570:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL571:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE109:
	.size	Mod__Mod_var_LC_i32, .-Mod__Mod_var_LC_i32
	.align 4
	.type	Mod__uMod_param_LC_i32, #function
	.proc	05
Mod__uMod_param_LC_i32:
.LLFB110:
	save	%sp, -104, %sp
.LLCFI110:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE110:
	.size	Mod__uMod_param_LC_i32, .-Mod__uMod_param_LC_i32
	.align 4
	.type	Mod__Mod_param_LC_i32, #function
	.proc	05
Mod__Mod_param_LC_i32:
.LLFB111:
	save	%sp, -104, %sp
.LLCFI111:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL578
	 nop
	cmp	%l0, 0
	bne	%icc, .LL582
	 nop
	cmp	%l1, 0
.LL582:
	cmp	%o4, 0
	bl	%icc, .LL577
	 nop
	cmp	%o4, 0
	bne	%icc, .LL583
	 nop
	cmp	%o5, 0
.LL583:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL581
	 nop
.LL577:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL580
	 nop
.LL578:
	cmp	%o4, 0
	bg	%icc, .LL579
	 nop
	cmp	%o4, 0
	bne	%icc, .LL584
	 nop
	cmp	%o5, 0
	bne	%icc, .LL579
	 nop
	cmp	%o5, 0
.LL584:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL581
	 nop
.LL579:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL580:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL581:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE111:
	.size	Mod__Mod_param_LC_i32, .-Mod__Mod_param_LC_i32
	.align 4
	.type	Mod__uMod_var_LC_i16, #function
	.proc	05
Mod__uMod_var_LC_i16:
.LLFB112:
	save	%sp, -96, %sp
.LLCFI112:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE112:
	.size	Mod__uMod_var_LC_i16, .-Mod__uMod_var_LC_i16
	.align 4
	.type	Mod__Mod_var_LC_i16, #function
	.proc	05
Mod__Mod_var_LC_i16:
.LLFB113:
	save	%sp, -96, %sp
.LLCFI113:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL588
	 nop
	cmp	%l0, 0
	bne	%icc, .LL592
	 nop
	cmp	%l1, 0
.LL592:
	cmp	%o4, 0
	bl	%icc, .LL587
	 nop
	cmp	%o4, 0
	bne	%icc, .LL593
	 nop
	cmp	%o5, 0
.LL593:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL591
	 nop
.LL587:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL590
	 nop
.LL588:
	cmp	%o4, 0
	bg	%icc, .LL589
	 nop
	cmp	%o4, 0
	bne	%icc, .LL594
	 nop
	cmp	%o5, 0
	bne	%icc, .LL589
	 nop
	cmp	%o5, 0
.LL594:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL591
	 nop
.LL589:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL590:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL591:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE113:
	.size	Mod__Mod_var_LC_i16, .-Mod__Mod_var_LC_i16
	.align 4
	.type	Mod__uMod_param_LC_i16, #function
	.proc	05
Mod__uMod_param_LC_i16:
.LLFB114:
	save	%sp, -104, %sp
.LLCFI114:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE114:
	.size	Mod__uMod_param_LC_i16, .-Mod__uMod_param_LC_i16
	.align 4
	.type	Mod__Mod_param_LC_i16, #function
	.proc	05
Mod__Mod_param_LC_i16:
.LLFB115:
	save	%sp, -104, %sp
.LLCFI115:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL598
	 nop
	cmp	%l0, 0
	bne	%icc, .LL602
	 nop
	cmp	%l1, 0
.LL602:
	cmp	%o4, 0
	bl	%icc, .LL597
	 nop
	cmp	%o4, 0
	bne	%icc, .LL603
	 nop
	cmp	%o5, 0
.LL603:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL601
	 nop
.LL597:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL600
	 nop
.LL598:
	cmp	%o4, 0
	bg	%icc, .LL599
	 nop
	cmp	%o4, 0
	bne	%icc, .LL604
	 nop
	cmp	%o5, 0
	bne	%icc, .LL599
	 nop
	cmp	%o5, 0
.LL604:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL601
	 nop
.LL599:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL600:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL601:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE115:
	.size	Mod__Mod_param_LC_i16, .-Mod__Mod_param_LC_i16
	.align 4
	.type	Mod__uMod_var_LC_I, #function
	.proc	05
Mod__uMod_var_LC_I:
.LLFB116:
	save	%sp, -96, %sp
.LLCFI116:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE116:
	.size	Mod__uMod_var_LC_I, .-Mod__uMod_var_LC_I
	.align 4
	.type	Mod__Mod_var_LC_I, #function
	.proc	05
Mod__Mod_var_LC_I:
.LLFB117:
	save	%sp, -96, %sp
.LLCFI117:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL608
	 nop
	cmp	%l0, 0
	bne	%icc, .LL612
	 nop
	cmp	%l1, 0
.LL612:
	cmp	%o4, 0
	bl	%icc, .LL607
	 nop
	cmp	%o4, 0
	bne	%icc, .LL613
	 nop
	cmp	%o5, 0
.LL613:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL611
	 nop
.LL607:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL610
	 nop
.LL608:
	cmp	%o4, 0
	bg	%icc, .LL609
	 nop
	cmp	%o4, 0
	bne	%icc, .LL614
	 nop
	cmp	%o5, 0
	bne	%icc, .LL609
	 nop
	cmp	%o5, 0
.LL614:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL611
	 nop
.LL609:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL610:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL611:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE117:
	.size	Mod__Mod_var_LC_I, .-Mod__Mod_var_LC_I
	.align 4
	.type	Mod__uMod_param_LC_I, #function
	.proc	05
Mod__uMod_param_LC_I:
.LLFB118:
	save	%sp, -104, %sp
.LLCFI118:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE118:
	.size	Mod__uMod_param_LC_I, .-Mod__uMod_param_LC_I
	.align 4
	.type	Mod__Mod_param_LC_I, #function
	.proc	05
Mod__Mod_param_LC_I:
.LLFB119:
	save	%sp, -104, %sp
.LLCFI119:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL618
	 nop
	cmp	%l0, 0
	bne	%icc, .LL622
	 nop
	cmp	%l1, 0
.LL622:
	cmp	%o4, 0
	bl	%icc, .LL617
	 nop
	cmp	%o4, 0
	bne	%icc, .LL623
	 nop
	cmp	%o5, 0
.LL623:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL621
	 nop
.LL617:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL620
	 nop
.LL618:
	cmp	%o4, 0
	bg	%icc, .LL619
	 nop
	cmp	%o4, 0
	bne	%icc, .LL624
	 nop
	cmp	%o5, 0
	bne	%icc, .LL619
	 nop
	cmp	%o5, 0
.LL624:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL621
	 nop
.LL619:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL620:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL621:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE119:
	.size	Mod__Mod_param_LC_I, .-Mod__Mod_param_LC_I
	.align 4
	.type	Mod__uMod_var_LC_i64, #function
	.proc	05
Mod__uMod_var_LC_i64:
.LLFB120:
	save	%sp, -96, %sp
.LLCFI120:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE120:
	.size	Mod__uMod_var_LC_i64, .-Mod__uMod_var_LC_i64
	.align 4
	.type	Mod__Mod_var_LC_i64, #function
	.proc	05
Mod__Mod_var_LC_i64:
.LLFB121:
	save	%sp, -96, %sp
.LLCFI121:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL628
	 nop
	cmp	%l0, 0
	bne	%icc, .LL632
	 nop
	cmp	%l1, 0
.LL632:
	cmp	%o4, 0
	bl	%icc, .LL627
	 nop
	cmp	%o4, 0
	bne	%icc, .LL633
	 nop
	cmp	%o5, 0
.LL633:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL631
	 nop
.LL627:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL630
	 nop
.LL628:
	cmp	%o4, 0
	bg	%icc, .LL629
	 nop
	cmp	%o4, 0
	bne	%icc, .LL634
	 nop
	cmp	%o5, 0
	bne	%icc, .LL629
	 nop
	cmp	%o5, 0
.LL634:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL631
	 nop
.LL629:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL630:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL631:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE121:
	.size	Mod__Mod_var_LC_i64, .-Mod__Mod_var_LC_i64
	.align 4
	.type	Mod__uMod_param_LC_i64, #function
	.proc	05
Mod__uMod_param_LC_i64:
.LLFB122:
	save	%sp, -112, %sp
.LLCFI122:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE122:
	.size	Mod__uMod_param_LC_i64, .-Mod__uMod_param_LC_i64
	.align 4
	.type	Mod__Mod_param_LC_i64, #function
	.proc	05
Mod__Mod_param_LC_i64:
.LLFB123:
	save	%sp, -112, %sp
.LLCFI123:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL638
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL642
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL642:
	cmp	%o4, 0
	bl	%icc, .LL637
	 nop
	cmp	%o4, 0
	bne	%icc, .LL643
	 nop
	cmp	%o5, 0
.LL643:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL641
	 nop
.LL637:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL640
	 nop
.LL638:
	cmp	%o4, 0
	bg	%icc, .LL639
	 nop
	cmp	%o4, 0
	bne	%icc, .LL644
	 nop
	cmp	%o5, 0
	bne	%icc, .LL639
	 nop
	cmp	%o5, 0
.LL644:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL641
	 nop
.LL639:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL640:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL641:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE123:
	.size	Mod__Mod_param_LC_i64, .-Mod__Mod_param_LC_i64
	.align 4
	.type	Mod__uMod_var_LC_C, #function
	.proc	05
Mod__uMod_var_LC_C:
.LLFB124:
	save	%sp, -96, %sp
.LLCFI124:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE124:
	.size	Mod__uMod_var_LC_C, .-Mod__uMod_var_LC_C
	.align 4
	.type	Mod__Mod_var_LC_C, #function
	.proc	05
Mod__Mod_var_LC_C:
.LLFB125:
	save	%sp, -96, %sp
.LLCFI125:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL648
	 nop
	cmp	%l0, 0
	bne	%icc, .LL652
	 nop
	cmp	%l1, 0
.LL652:
	cmp	%o4, 0
	bl	%icc, .LL647
	 nop
	cmp	%o4, 0
	bne	%icc, .LL653
	 nop
	cmp	%o5, 0
.LL653:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL651
	 nop
.LL647:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL650
	 nop
.LL648:
	cmp	%o4, 0
	bg	%icc, .LL649
	 nop
	cmp	%o4, 0
	bne	%icc, .LL654
	 nop
	cmp	%o5, 0
	bne	%icc, .LL649
	 nop
	cmp	%o5, 0
.LL654:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL651
	 nop
.LL649:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL650:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL651:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE125:
	.size	Mod__Mod_var_LC_C, .-Mod__Mod_var_LC_C
	.align 4
	.type	Mod__uMod_param_LC_C, #function
	.proc	05
Mod__uMod_param_LC_C:
.LLFB126:
	save	%sp, -104, %sp
.LLCFI126:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE126:
	.size	Mod__uMod_param_LC_C, .-Mod__uMod_param_LC_C
	.align 4
	.type	Mod__Mod_param_LC_C, #function
	.proc	05
Mod__Mod_param_LC_C:
.LLFB127:
	save	%sp, -104, %sp
.LLCFI127:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	ld	[%fp+76], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL658
	 nop
	cmp	%l0, 0
	bne	%icc, .LL662
	 nop
	cmp	%l1, 0
.LL662:
	cmp	%o4, 0
	bl	%icc, .LL657
	 nop
	cmp	%o4, 0
	bne	%icc, .LL663
	 nop
	cmp	%o5, 0
.LL663:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL661
	 nop
.LL657:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL660
	 nop
.LL658:
	cmp	%o4, 0
	bg	%icc, .LL659
	 nop
	cmp	%o4, 0
	bne	%icc, .LL664
	 nop
	cmp	%o5, 0
	bne	%icc, .LL659
	 nop
	cmp	%o5, 0
.LL664:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL661
	 nop
.LL659:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL660:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL661:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE127:
	.size	Mod__Mod_param_LC_C, .-Mod__Mod_param_LC_C
	.align 4
	.type	Mod__uMod_var_LC_u8, #function
	.proc	05
Mod__uMod_var_LC_u8:
.LLFB128:
	save	%sp, -96, %sp
.LLCFI128:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE128:
	.size	Mod__uMod_var_LC_u8, .-Mod__uMod_var_LC_u8
	.align 4
	.type	Mod__Mod_var_LC_u8, #function
	.proc	05
Mod__Mod_var_LC_u8:
.LLFB129:
	save	%sp, -96, %sp
.LLCFI129:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL668
	 nop
	cmp	%l0, 0
	bne	%icc, .LL672
	 nop
	cmp	%l1, 0
.LL672:
	cmp	%o4, 0
	bl	%icc, .LL667
	 nop
	cmp	%o4, 0
	bne	%icc, .LL673
	 nop
	cmp	%o5, 0
.LL673:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL671
	 nop
.LL667:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL670
	 nop
.LL668:
	cmp	%o4, 0
	bg	%icc, .LL669
	 nop
	cmp	%o4, 0
	bne	%icc, .LL674
	 nop
	cmp	%o5, 0
	bne	%icc, .LL669
	 nop
	cmp	%o5, 0
.LL674:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL671
	 nop
.LL669:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL670:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL671:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE129:
	.size	Mod__Mod_var_LC_u8, .-Mod__Mod_var_LC_u8
	.align 4
	.type	Mod__uMod_param_LC_u8, #function
	.proc	05
Mod__uMod_param_LC_u8:
.LLFB130:
	save	%sp, -104, %sp
.LLCFI130:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE130:
	.size	Mod__uMod_param_LC_u8, .-Mod__uMod_param_LC_u8
	.align 4
	.type	Mod__Mod_param_LC_u8, #function
	.proc	05
Mod__Mod_param_LC_u8:
.LLFB131:
	save	%sp, -104, %sp
.LLCFI131:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL678
	 nop
	cmp	%l0, 0
	bne	%icc, .LL682
	 nop
	cmp	%l1, 0
.LL682:
	cmp	%o4, 0
	bl	%icc, .LL677
	 nop
	cmp	%o4, 0
	bne	%icc, .LL683
	 nop
	cmp	%o5, 0
.LL683:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL681
	 nop
.LL677:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL680
	 nop
.LL678:
	cmp	%o4, 0
	bg	%icc, .LL679
	 nop
	cmp	%o4, 0
	bne	%icc, .LL684
	 nop
	cmp	%o5, 0
	bne	%icc, .LL679
	 nop
	cmp	%o5, 0
.LL684:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL681
	 nop
.LL679:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL680:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL681:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE131:
	.size	Mod__Mod_param_LC_u8, .-Mod__Mod_param_LC_u8
	.align 4
	.type	Mod__uMod_var_LC_L, #function
	.proc	05
Mod__uMod_var_LC_L:
.LLFB132:
	save	%sp, -96, %sp
.LLCFI132:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE132:
	.size	Mod__uMod_var_LC_L, .-Mod__uMod_var_LC_L
	.align 4
	.type	Mod__Mod_var_LC_L, #function
	.proc	05
Mod__Mod_var_LC_L:
.LLFB133:
	save	%sp, -96, %sp
.LLCFI133:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL688
	 nop
	cmp	%l0, 0
	bne	%icc, .LL692
	 nop
	cmp	%l1, 0
.LL692:
	cmp	%o4, 0
	bl	%icc, .LL687
	 nop
	cmp	%o4, 0
	bne	%icc, .LL693
	 nop
	cmp	%o5, 0
.LL693:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL691
	 nop
.LL687:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL690
	 nop
.LL688:
	cmp	%o4, 0
	bg	%icc, .LL689
	 nop
	cmp	%o4, 0
	bne	%icc, .LL694
	 nop
	cmp	%o5, 0
	bne	%icc, .LL689
	 nop
	cmp	%o5, 0
.LL694:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL691
	 nop
.LL689:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL690:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL691:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE133:
	.size	Mod__Mod_var_LC_L, .-Mod__Mod_var_LC_L
	.align 4
	.type	Mod__uMod_param_LC_L, #function
	.proc	05
Mod__uMod_param_LC_L:
.LLFB134:
	save	%sp, -112, %sp
.LLCFI134:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE134:
	.size	Mod__uMod_param_LC_L, .-Mod__uMod_param_LC_L
	.align 4
	.type	Mod__Mod_param_LC_L, #function
	.proc	05
Mod__Mod_param_LC_L:
.LLFB135:
	save	%sp, -112, %sp
.LLCFI135:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL698
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL702
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL702:
	cmp	%o4, 0
	bl	%icc, .LL697
	 nop
	cmp	%o4, 0
	bne	%icc, .LL703
	 nop
	cmp	%o5, 0
.LL703:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL701
	 nop
.LL697:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL700
	 nop
.LL698:
	cmp	%o4, 0
	bg	%icc, .LL699
	 nop
	cmp	%o4, 0
	bne	%icc, .LL704
	 nop
	cmp	%o5, 0
	bne	%icc, .LL699
	 nop
	cmp	%o5, 0
.LL704:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL701
	 nop
.LL699:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL700:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL701:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE135:
	.size	Mod__Mod_param_LC_L, .-Mod__Mod_param_LC_L
	.align 4
	.type	Mod__uMod_var_LC_i8, #function
	.proc	05
Mod__uMod_var_LC_i8:
.LLFB136:
	save	%sp, -96, %sp
.LLCFI136:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE136:
	.size	Mod__uMod_var_LC_i8, .-Mod__uMod_var_LC_i8
	.align 4
	.type	Mod__Mod_var_LC_i8, #function
	.proc	05
Mod__Mod_var_LC_i8:
.LLFB137:
	save	%sp, -96, %sp
.LLCFI137:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL708
	 nop
	cmp	%l0, 0
	bne	%icc, .LL712
	 nop
	cmp	%l1, 0
.LL712:
	cmp	%o4, 0
	bl	%icc, .LL707
	 nop
	cmp	%o4, 0
	bne	%icc, .LL713
	 nop
	cmp	%o5, 0
.LL713:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL711
	 nop
.LL707:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL710
	 nop
.LL708:
	cmp	%o4, 0
	bg	%icc, .LL709
	 nop
	cmp	%o4, 0
	bne	%icc, .LL714
	 nop
	cmp	%o5, 0
	bne	%icc, .LL709
	 nop
	cmp	%o5, 0
.LL714:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL711
	 nop
.LL709:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL710:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL711:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE137:
	.size	Mod__Mod_var_LC_i8, .-Mod__Mod_var_LC_i8
	.align 4
	.type	Mod__uMod_param_LC_i8, #function
	.proc	05
Mod__uMod_param_LC_i8:
.LLFB138:
	save	%sp, -104, %sp
.LLCFI138:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE138:
	.size	Mod__uMod_param_LC_i8, .-Mod__uMod_param_LC_i8
	.align 4
	.type	Mod__Mod_param_LC_i8, #function
	.proc	05
Mod__Mod_param_LC_i8:
.LLFB139:
	save	%sp, -104, %sp
.LLCFI139:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL718
	 nop
	cmp	%l0, 0
	bne	%icc, .LL722
	 nop
	cmp	%l1, 0
.LL722:
	cmp	%o4, 0
	bl	%icc, .LL717
	 nop
	cmp	%o4, 0
	bne	%icc, .LL723
	 nop
	cmp	%o5, 0
.LL723:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL721
	 nop
.LL717:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL720
	 nop
.LL718:
	cmp	%o4, 0
	bg	%icc, .LL719
	 nop
	cmp	%o4, 0
	bne	%icc, .LL724
	 nop
	cmp	%o5, 0
	bne	%icc, .LL719
	 nop
	cmp	%o5, 0
.LL724:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL721
	 nop
.LL719:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL720:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL721:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE139:
	.size	Mod__Mod_param_LC_i8, .-Mod__Mod_param_LC_i8
	.align 4
	.type	Mod__uMod_var_LC_u32, #function
	.proc	05
Mod__uMod_var_LC_u32:
.LLFB140:
	save	%sp, -96, %sp
.LLCFI140:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE140:
	.size	Mod__uMod_var_LC_u32, .-Mod__uMod_var_LC_u32
	.align 4
	.type	Mod__Mod_var_LC_u32, #function
	.proc	05
Mod__Mod_var_LC_u32:
.LLFB141:
	save	%sp, -96, %sp
.LLCFI141:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l2
	mov	%o5, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL728
	 nop
	cmp	%l0, 0
	bne	%icc, .LL732
	 nop
	cmp	%l1, 0
.LL732:
	cmp	%o4, 0
	bl	%icc, .LL727
	 nop
	cmp	%o4, 0
	bne	%icc, .LL733
	 nop
	cmp	%o5, 0
.LL733:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL731
	 nop
.LL727:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL730
	 nop
.LL728:
	cmp	%o4, 0
	bg	%icc, .LL729
	 nop
	cmp	%o4, 0
	bne	%icc, .LL734
	 nop
	cmp	%o5, 0
	bne	%icc, .LL729
	 nop
	cmp	%o5, 0
.LL734:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL731
	 nop
.LL729:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL730:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL731:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE141:
	.size	Mod__Mod_var_LC_u32, .-Mod__Mod_var_LC_u32
	.align 4
	.type	Mod__uMod_param_LC_u32, #function
	.proc	05
Mod__uMod_param_LC_u32:
.LLFB142:
	save	%sp, -104, %sp
.LLCFI142:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE142:
	.size	Mod__uMod_param_LC_u32, .-Mod__uMod_param_LC_u32
	.align 4
	.type	Mod__Mod_param_LC_u32, #function
	.proc	05
Mod__Mod_param_LC_u32:
.LLFB143:
	save	%sp, -104, %sp
.LLCFI143:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %l2
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL738
	 nop
	cmp	%l0, 0
	bne	%icc, .LL742
	 nop
	cmp	%l1, 0
.LL742:
	cmp	%o4, 0
	bl	%icc, .LL737
	 nop
	cmp	%o4, 0
	bne	%icc, .LL743
	 nop
	cmp	%o5, 0
.LL743:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL741
	 nop
.LL737:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL740
	 nop
.LL738:
	cmp	%o4, 0
	bg	%icc, .LL739
	 nop
	cmp	%o4, 0
	bne	%icc, .LL744
	 nop
	cmp	%o5, 0
	bne	%icc, .LL739
	 nop
	cmp	%o5, 0
.LL744:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL741
	 nop
.LL739:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL740:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL741:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE143:
	.size	Mod__Mod_param_LC_u32, .-Mod__Mod_param_LC_u32
	.align 4
	.type	Mod__uMod_var_i32_u16, #function
	.proc	04
Mod__uMod_var_i32_u16:
.LLFB144:
	save	%sp, -96, %sp
.LLCFI144:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE144:
	.size	Mod__uMod_var_i32_u16, .-Mod__uMod_var_i32_u16
	.align 4
	.type	Mod__Mod_var_i32_u16, #function
	.proc	04
Mod__Mod_var_i32_u16:
.LLFB145:
	save	%sp, -96, %sp
.LLCFI145:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL748
	 nop
	cmp	%g1, 0
	bl	%icc, .LL747
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL751
	 nop
.LL747:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL750
	 nop
.LL748:
	cmp	%g1, 0
	bg	%icc, .LL749
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL751
	 nop
.LL749:
	add	%g1, -1, %g1
.LL750:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL751:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE145:
	.size	Mod__Mod_var_i32_u16, .-Mod__Mod_var_i32_u16
	.align 4
	.type	Mod__uMod_param_i32_u16, #function
	.proc	04
Mod__uMod_param_i32_u16:
.LLFB146:
	save	%sp, -96, %sp
.LLCFI146:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE146:
	.size	Mod__uMod_param_i32_u16, .-Mod__uMod_param_i32_u16
	.align 4
	.type	Mod__Mod_param_i32_u16, #function
	.proc	04
Mod__Mod_param_i32_u16:
.LLFB147:
	save	%sp, -96, %sp
.LLCFI147:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL755
	 nop
	cmp	%g1, 0
	bl	%icc, .LL754
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL758
	 nop
.LL754:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL757
	 nop
.LL755:
	cmp	%g1, 0
	bg	%icc, .LL756
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL758
	 nop
.LL756:
	add	%g1, -1, %g1
.LL757:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL758:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE147:
	.size	Mod__Mod_param_i32_u16, .-Mod__Mod_param_i32_u16
	.align 4
	.type	Mod__uMod_var_i32_u64, #function
	.proc	05
Mod__uMod_var_i32_u64:
.LLFB148:
	save	%sp, -96, %sp
.LLCFI148:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE148:
	.size	Mod__uMod_var_i32_u64, .-Mod__uMod_var_i32_u64
	.align 4
	.type	Mod__Mod_var_i32_u64, #function
	.proc	05
Mod__Mod_var_i32_u64:
.LLFB149:
	save	%sp, -96, %sp
.LLCFI149:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL762
	 nop
	cmp	%l0, 0
	bne	%icc, .LL766
	 nop
	cmp	%l1, 0
.LL766:
	cmp	%o4, 0
	bl	%icc, .LL761
	 nop
	cmp	%o4, 0
	bne	%icc, .LL767
	 nop
	cmp	%o5, 0
.LL767:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL765
	 nop
.LL761:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL764
	 nop
.LL762:
	cmp	%o4, 0
	bg	%icc, .LL763
	 nop
	cmp	%o4, 0
	bne	%icc, .LL768
	 nop
	cmp	%o5, 0
	bne	%icc, .LL763
	 nop
	cmp	%o5, 0
.LL768:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL765
	 nop
.LL763:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL764:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL765:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE149:
	.size	Mod__Mod_var_i32_u64, .-Mod__Mod_var_i32_u64
	.align 4
	.type	Mod__uMod_param_i32_u64, #function
	.proc	05
Mod__uMod_param_i32_u64:
.LLFB150:
	save	%sp, -104, %sp
.LLCFI150:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE150:
	.size	Mod__uMod_param_i32_u64, .-Mod__uMod_param_i32_u64
	.align 4
	.type	Mod__Mod_param_i32_u64, #function
	.proc	05
Mod__Mod_param_i32_u64:
.LLFB151:
	save	%sp, -104, %sp
.LLCFI151:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL772
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL776
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL776:
	cmp	%o4, 0
	bl	%icc, .LL771
	 nop
	cmp	%o4, 0
	bne	%icc, .LL777
	 nop
	cmp	%o5, 0
.LL777:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL775
	 nop
.LL771:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL774
	 nop
.LL772:
	cmp	%o4, 0
	bg	%icc, .LL773
	 nop
	cmp	%o4, 0
	bne	%icc, .LL778
	 nop
	cmp	%o5, 0
	bne	%icc, .LL773
	 nop
	cmp	%o5, 0
.LL778:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL775
	 nop
.LL773:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL774:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL775:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE151:
	.size	Mod__Mod_param_i32_u64, .-Mod__Mod_param_i32_u64
	.align 4
	.type	Mod__uMod_var_i32_LC, #function
	.proc	05
Mod__uMod_var_i32_LC:
.LLFB152:
	save	%sp, -96, %sp
.LLCFI152:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE152:
	.size	Mod__uMod_var_i32_LC, .-Mod__uMod_var_i32_LC
	.align 4
	.type	Mod__Mod_var_i32_LC, #function
	.proc	05
Mod__Mod_var_i32_LC:
.LLFB153:
	save	%sp, -96, %sp
.LLCFI153:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL782
	 nop
	cmp	%l0, 0
	bne	%icc, .LL786
	 nop
	cmp	%l1, 0
.LL786:
	cmp	%o4, 0
	bl	%icc, .LL781
	 nop
	cmp	%o4, 0
	bne	%icc, .LL787
	 nop
	cmp	%o5, 0
.LL787:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL785
	 nop
.LL781:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL784
	 nop
.LL782:
	cmp	%o4, 0
	bg	%icc, .LL783
	 nop
	cmp	%o4, 0
	bne	%icc, .LL788
	 nop
	cmp	%o5, 0
	bne	%icc, .LL783
	 nop
	cmp	%o5, 0
.LL788:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL785
	 nop
.LL783:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL784:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL785:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE153:
	.size	Mod__Mod_var_i32_LC, .-Mod__Mod_var_i32_LC
	.align 4
	.type	Mod__uMod_param_i32_LC, #function
	.proc	05
Mod__uMod_param_i32_LC:
.LLFB154:
	save	%sp, -104, %sp
.LLCFI154:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE154:
	.size	Mod__uMod_param_i32_LC, .-Mod__uMod_param_i32_LC
	.align 4
	.type	Mod__Mod_param_i32_LC, #function
	.proc	05
Mod__Mod_param_i32_LC:
.LLFB155:
	save	%sp, -104, %sp
.LLCFI155:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l3
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l2
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL792
	 nop
	cmp	%l0, 0
	bne	%icc, .LL796
	 nop
	cmp	%l1, 0
.LL796:
	cmp	%o4, 0
	bl	%icc, .LL791
	 nop
	cmp	%o4, 0
	bne	%icc, .LL797
	 nop
	cmp	%o5, 0
.LL797:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL795
	 nop
.LL791:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL794
	 nop
.LL792:
	cmp	%o4, 0
	bg	%icc, .LL793
	 nop
	cmp	%o4, 0
	bne	%icc, .LL798
	 nop
	cmp	%o5, 0
	bne	%icc, .LL793
	 nop
	cmp	%o5, 0
.LL798:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL795
	 nop
.LL793:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL794:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL795:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE155:
	.size	Mod__Mod_param_i32_LC, .-Mod__Mod_param_i32_LC
	.align 4
	.type	Mod__uMod_var_i32_i32, #function
	.proc	04
Mod__uMod_var_i32_i32:
.LLFB156:
	save	%sp, -96, %sp
.LLCFI156:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE156:
	.size	Mod__uMod_var_i32_i32, .-Mod__uMod_var_i32_i32
	.align 4
	.type	Mod__Mod_var_i32_i32, #function
	.proc	04
Mod__Mod_var_i32_i32:
.LLFB157:
	save	%sp, -96, %sp
.LLCFI157:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL802
	 nop
	cmp	%g1, 0
	bl	%icc, .LL801
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL805
	 nop
.LL801:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL804
	 nop
.LL802:
	cmp	%g1, 0
	bg	%icc, .LL803
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL805
	 nop
.LL803:
	add	%g1, -1, %g1
.LL804:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL805:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE157:
	.size	Mod__Mod_var_i32_i32, .-Mod__Mod_var_i32_i32
	.align 4
	.type	Mod__uMod_param_i32_i32, #function
	.proc	04
Mod__uMod_param_i32_i32:
.LLFB158:
	save	%sp, -96, %sp
.LLCFI158:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE158:
	.size	Mod__uMod_param_i32_i32, .-Mod__uMod_param_i32_i32
	.align 4
	.type	Mod__Mod_param_i32_i32, #function
	.proc	04
Mod__Mod_param_i32_i32:
.LLFB159:
	save	%sp, -96, %sp
.LLCFI159:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL809
	 nop
	cmp	%g1, 0
	bl	%icc, .LL808
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL812
	 nop
.LL808:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL811
	 nop
.LL809:
	cmp	%g1, 0
	bg	%icc, .LL810
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL812
	 nop
.LL810:
	add	%g1, -1, %g1
.LL811:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL812:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE159:
	.size	Mod__Mod_param_i32_i32, .-Mod__Mod_param_i32_i32
	.align 4
	.type	Mod__uMod_var_i32_i16, #function
	.proc	04
Mod__uMod_var_i32_i16:
.LLFB160:
	save	%sp, -96, %sp
.LLCFI160:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE160:
	.size	Mod__uMod_var_i32_i16, .-Mod__uMod_var_i32_i16
	.align 4
	.type	Mod__Mod_var_i32_i16, #function
	.proc	04
Mod__Mod_var_i32_i16:
.LLFB161:
	save	%sp, -96, %sp
.LLCFI161:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL816
	 nop
	cmp	%g1, 0
	bl	%icc, .LL815
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL819
	 nop
.LL815:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL818
	 nop
.LL816:
	cmp	%g1, 0
	bg	%icc, .LL817
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL819
	 nop
.LL817:
	add	%g1, -1, %g1
.LL818:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL819:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE161:
	.size	Mod__Mod_var_i32_i16, .-Mod__Mod_var_i32_i16
	.align 4
	.type	Mod__uMod_param_i32_i16, #function
	.proc	04
Mod__uMod_param_i32_i16:
.LLFB162:
	save	%sp, -96, %sp
.LLCFI162:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE162:
	.size	Mod__uMod_param_i32_i16, .-Mod__uMod_param_i32_i16
	.align 4
	.type	Mod__Mod_param_i32_i16, #function
	.proc	04
Mod__Mod_param_i32_i16:
.LLFB163:
	save	%sp, -96, %sp
.LLCFI163:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL823
	 nop
	cmp	%g1, 0
	bl	%icc, .LL822
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL826
	 nop
.LL822:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL825
	 nop
.LL823:
	cmp	%g1, 0
	bg	%icc, .LL824
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL826
	 nop
.LL824:
	add	%g1, -1, %g1
.LL825:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL826:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE163:
	.size	Mod__Mod_param_i32_i16, .-Mod__Mod_param_i32_i16
	.align 4
	.type	Mod__uMod_var_i32_I, #function
	.proc	04
Mod__uMod_var_i32_I:
.LLFB164:
	save	%sp, -96, %sp
.LLCFI164:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE164:
	.size	Mod__uMod_var_i32_I, .-Mod__uMod_var_i32_I
	.align 4
	.type	Mod__Mod_var_i32_I, #function
	.proc	04
Mod__Mod_var_i32_I:
.LLFB165:
	save	%sp, -96, %sp
.LLCFI165:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL830
	 nop
	cmp	%g1, 0
	bl	%icc, .LL829
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL833
	 nop
.LL829:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL832
	 nop
.LL830:
	cmp	%g1, 0
	bg	%icc, .LL831
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL833
	 nop
.LL831:
	add	%g1, -1, %g1
.LL832:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL833:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE165:
	.size	Mod__Mod_var_i32_I, .-Mod__Mod_var_i32_I
	.align 4
	.type	Mod__uMod_param_i32_I, #function
	.proc	04
Mod__uMod_param_i32_I:
.LLFB166:
	save	%sp, -96, %sp
.LLCFI166:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE166:
	.size	Mod__uMod_param_i32_I, .-Mod__uMod_param_i32_I
	.align 4
	.type	Mod__Mod_param_i32_I, #function
	.proc	04
Mod__Mod_param_i32_I:
.LLFB167:
	save	%sp, -96, %sp
.LLCFI167:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL837
	 nop
	cmp	%g1, 0
	bl	%icc, .LL836
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL840
	 nop
.LL836:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL839
	 nop
.LL837:
	cmp	%g1, 0
	bg	%icc, .LL838
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL840
	 nop
.LL838:
	add	%g1, -1, %g1
.LL839:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL840:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE167:
	.size	Mod__Mod_param_i32_I, .-Mod__Mod_param_i32_I
	.align 4
	.type	Mod__uMod_var_i32_i64, #function
	.proc	05
Mod__uMod_var_i32_i64:
.LLFB168:
	save	%sp, -96, %sp
.LLCFI168:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE168:
	.size	Mod__uMod_var_i32_i64, .-Mod__uMod_var_i32_i64
	.align 4
	.type	Mod__Mod_var_i32_i64, #function
	.proc	05
Mod__Mod_var_i32_i64:
.LLFB169:
	save	%sp, -96, %sp
.LLCFI169:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL844
	 nop
	cmp	%l0, 0
	bne	%icc, .LL848
	 nop
	cmp	%l1, 0
.LL848:
	cmp	%o4, 0
	bl	%icc, .LL843
	 nop
	cmp	%o4, 0
	bne	%icc, .LL849
	 nop
	cmp	%o5, 0
.LL849:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL847
	 nop
.LL843:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL846
	 nop
.LL844:
	cmp	%o4, 0
	bg	%icc, .LL845
	 nop
	cmp	%o4, 0
	bne	%icc, .LL850
	 nop
	cmp	%o5, 0
	bne	%icc, .LL845
	 nop
	cmp	%o5, 0
.LL850:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL847
	 nop
.LL845:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL846:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL847:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE169:
	.size	Mod__Mod_var_i32_i64, .-Mod__Mod_var_i32_i64
	.align 4
	.type	Mod__uMod_param_i32_i64, #function
	.proc	05
Mod__uMod_param_i32_i64:
.LLFB170:
	save	%sp, -104, %sp
.LLCFI170:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE170:
	.size	Mod__uMod_param_i32_i64, .-Mod__uMod_param_i32_i64
	.align 4
	.type	Mod__Mod_param_i32_i64, #function
	.proc	05
Mod__Mod_param_i32_i64:
.LLFB171:
	save	%sp, -104, %sp
.LLCFI171:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL854
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL858
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL858:
	cmp	%o4, 0
	bl	%icc, .LL853
	 nop
	cmp	%o4, 0
	bne	%icc, .LL859
	 nop
	cmp	%o5, 0
.LL859:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL857
	 nop
.LL853:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL856
	 nop
.LL854:
	cmp	%o4, 0
	bg	%icc, .LL855
	 nop
	cmp	%o4, 0
	bne	%icc, .LL860
	 nop
	cmp	%o5, 0
	bne	%icc, .LL855
	 nop
	cmp	%o5, 0
.LL860:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL857
	 nop
.LL855:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL856:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL857:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE171:
	.size	Mod__Mod_param_i32_i64, .-Mod__Mod_param_i32_i64
	.align 4
	.type	Mod__uMod_var_i32_C, #function
	.proc	04
Mod__uMod_var_i32_C:
.LLFB172:
	save	%sp, -96, %sp
.LLCFI172:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE172:
	.size	Mod__uMod_var_i32_C, .-Mod__uMod_var_i32_C
	.align 4
	.type	Mod__Mod_var_i32_C, #function
	.proc	04
Mod__Mod_var_i32_C:
.LLFB173:
	save	%sp, -96, %sp
.LLCFI173:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL864
	 nop
	cmp	%g1, 0
	bl	%icc, .LL863
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL867
	 nop
.LL863:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL866
	 nop
.LL864:
	cmp	%g1, 0
	bg	%icc, .LL865
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL867
	 nop
.LL865:
	add	%g1, -1, %g1
.LL866:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL867:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE173:
	.size	Mod__Mod_var_i32_C, .-Mod__Mod_var_i32_C
	.align 4
	.type	Mod__uMod_param_i32_C, #function
	.proc	04
Mod__uMod_param_i32_C:
.LLFB174:
	save	%sp, -96, %sp
.LLCFI174:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE174:
	.size	Mod__uMod_param_i32_C, .-Mod__uMod_param_i32_C
	.align 4
	.type	Mod__Mod_param_i32_C, #function
	.proc	04
Mod__Mod_param_i32_C:
.LLFB175:
	save	%sp, -96, %sp
.LLCFI175:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL871
	 nop
	cmp	%g1, 0
	bl	%icc, .LL870
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL874
	 nop
.LL870:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL873
	 nop
.LL871:
	cmp	%g1, 0
	bg	%icc, .LL872
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL874
	 nop
.LL872:
	add	%g1, -1, %g1
.LL873:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL874:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE175:
	.size	Mod__Mod_param_i32_C, .-Mod__Mod_param_i32_C
	.align 4
	.type	Mod__uMod_var_i32_u8, #function
	.proc	04
Mod__uMod_var_i32_u8:
.LLFB176:
	save	%sp, -96, %sp
.LLCFI176:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE176:
	.size	Mod__uMod_var_i32_u8, .-Mod__uMod_var_i32_u8
	.align 4
	.type	Mod__Mod_var_i32_u8, #function
	.proc	04
Mod__Mod_var_i32_u8:
.LLFB177:
	save	%sp, -96, %sp
.LLCFI177:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL878
	 nop
	cmp	%g1, 0
	bl	%icc, .LL877
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL881
	 nop
.LL877:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL880
	 nop
.LL878:
	cmp	%g1, 0
	bg	%icc, .LL879
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL881
	 nop
.LL879:
	add	%g1, -1, %g1
.LL880:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL881:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE177:
	.size	Mod__Mod_var_i32_u8, .-Mod__Mod_var_i32_u8
	.align 4
	.type	Mod__uMod_param_i32_u8, #function
	.proc	04
Mod__uMod_param_i32_u8:
.LLFB178:
	save	%sp, -96, %sp
.LLCFI178:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE178:
	.size	Mod__uMod_param_i32_u8, .-Mod__uMod_param_i32_u8
	.align 4
	.type	Mod__Mod_param_i32_u8, #function
	.proc	04
Mod__Mod_param_i32_u8:
.LLFB179:
	save	%sp, -96, %sp
.LLCFI179:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL885
	 nop
	cmp	%g1, 0
	bl	%icc, .LL884
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL888
	 nop
.LL884:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL887
	 nop
.LL885:
	cmp	%g1, 0
	bg	%icc, .LL886
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL888
	 nop
.LL886:
	add	%g1, -1, %g1
.LL887:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL888:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE179:
	.size	Mod__Mod_param_i32_u8, .-Mod__Mod_param_i32_u8
	.align 4
	.type	Mod__uMod_var_i32_L, #function
	.proc	05
Mod__uMod_var_i32_L:
.LLFB180:
	save	%sp, -96, %sp
.LLCFI180:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE180:
	.size	Mod__uMod_var_i32_L, .-Mod__uMod_var_i32_L
	.align 4
	.type	Mod__Mod_var_i32_L, #function
	.proc	05
Mod__Mod_var_i32_L:
.LLFB181:
	save	%sp, -96, %sp
.LLCFI181:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL892
	 nop
	cmp	%l0, 0
	bne	%icc, .LL896
	 nop
	cmp	%l1, 0
.LL896:
	cmp	%o4, 0
	bl	%icc, .LL891
	 nop
	cmp	%o4, 0
	bne	%icc, .LL897
	 nop
	cmp	%o5, 0
.LL897:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL895
	 nop
.LL891:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL894
	 nop
.LL892:
	cmp	%o4, 0
	bg	%icc, .LL893
	 nop
	cmp	%o4, 0
	bne	%icc, .LL898
	 nop
	cmp	%o5, 0
	bne	%icc, .LL893
	 nop
	cmp	%o5, 0
.LL898:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL895
	 nop
.LL893:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL894:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL895:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE181:
	.size	Mod__Mod_var_i32_L, .-Mod__Mod_var_i32_L
	.align 4
	.type	Mod__uMod_param_i32_L, #function
	.proc	05
Mod__uMod_param_i32_L:
.LLFB182:
	save	%sp, -104, %sp
.LLCFI182:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE182:
	.size	Mod__uMod_param_i32_L, .-Mod__uMod_param_i32_L
	.align 4
	.type	Mod__Mod_param_i32_L, #function
	.proc	05
Mod__Mod_param_i32_L:
.LLFB183:
	save	%sp, -104, %sp
.LLCFI183:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL902
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL906
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL906:
	cmp	%o4, 0
	bl	%icc, .LL901
	 nop
	cmp	%o4, 0
	bne	%icc, .LL907
	 nop
	cmp	%o5, 0
.LL907:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL905
	 nop
.LL901:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL904
	 nop
.LL902:
	cmp	%o4, 0
	bg	%icc, .LL903
	 nop
	cmp	%o4, 0
	bne	%icc, .LL908
	 nop
	cmp	%o5, 0
	bne	%icc, .LL903
	 nop
	cmp	%o5, 0
.LL908:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL905
	 nop
.LL903:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL904:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL905:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE183:
	.size	Mod__Mod_param_i32_L, .-Mod__Mod_param_i32_L
	.align 4
	.type	Mod__uMod_var_i32_i8, #function
	.proc	04
Mod__uMod_var_i32_i8:
.LLFB184:
	save	%sp, -96, %sp
.LLCFI184:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE184:
	.size	Mod__uMod_var_i32_i8, .-Mod__uMod_var_i32_i8
	.align 4
	.type	Mod__Mod_var_i32_i8, #function
	.proc	04
Mod__Mod_var_i32_i8:
.LLFB185:
	save	%sp, -96, %sp
.LLCFI185:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL912
	 nop
	cmp	%g1, 0
	bl	%icc, .LL911
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL915
	 nop
.LL911:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL914
	 nop
.LL912:
	cmp	%g1, 0
	bg	%icc, .LL913
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL915
	 nop
.LL913:
	add	%g1, -1, %g1
.LL914:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL915:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE185:
	.size	Mod__Mod_var_i32_i8, .-Mod__Mod_var_i32_i8
	.align 4
	.type	Mod__uMod_param_i32_i8, #function
	.proc	04
Mod__uMod_param_i32_i8:
.LLFB186:
	save	%sp, -96, %sp
.LLCFI186:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE186:
	.size	Mod__uMod_param_i32_i8, .-Mod__uMod_param_i32_i8
	.align 4
	.type	Mod__Mod_param_i32_i8, #function
	.proc	04
Mod__Mod_param_i32_i8:
.LLFB187:
	save	%sp, -96, %sp
.LLCFI187:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL919
	 nop
	cmp	%g1, 0
	bl	%icc, .LL918
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL922
	 nop
.LL918:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL921
	 nop
.LL919:
	cmp	%g1, 0
	bg	%icc, .LL920
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL922
	 nop
.LL920:
	add	%g1, -1, %g1
.LL921:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL922:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE187:
	.size	Mod__Mod_param_i32_i8, .-Mod__Mod_param_i32_i8
	.align 4
	.type	Mod__uMod_var_i32_u32, #function
	.proc	04
Mod__uMod_var_i32_u32:
.LLFB188:
	save	%sp, -96, %sp
.LLCFI188:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE188:
	.size	Mod__uMod_var_i32_u32, .-Mod__uMod_var_i32_u32
	.align 4
	.type	Mod__Mod_var_i32_u32, #function
	.proc	04
Mod__Mod_var_i32_u32:
.LLFB189:
	save	%sp, -96, %sp
.LLCFI189:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL926
	 nop
	cmp	%g1, 0
	bl	%icc, .LL925
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL929
	 nop
.LL925:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL928
	 nop
.LL926:
	cmp	%g1, 0
	bg	%icc, .LL927
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL929
	 nop
.LL927:
	add	%g1, -1, %g1
.LL928:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL929:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE189:
	.size	Mod__Mod_var_i32_u32, .-Mod__Mod_var_i32_u32
	.align 4
	.type	Mod__uMod_param_i32_u32, #function
	.proc	04
Mod__uMod_param_i32_u32:
.LLFB190:
	save	%sp, -96, %sp
.LLCFI190:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE190:
	.size	Mod__uMod_param_i32_u32, .-Mod__uMod_param_i32_u32
	.align 4
	.type	Mod__Mod_param_i32_u32, #function
	.proc	04
Mod__Mod_param_i32_u32:
.LLFB191:
	save	%sp, -96, %sp
.LLCFI191:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL933
	 nop
	cmp	%g1, 0
	bl	%icc, .LL932
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL936
	 nop
.LL932:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL935
	 nop
.LL933:
	cmp	%g1, 0
	bg	%icc, .LL934
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL936
	 nop
.LL934:
	add	%g1, -1, %g1
.LL935:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL936:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE191:
	.size	Mod__Mod_param_i32_u32, .-Mod__Mod_param_i32_u32
	.align 4
	.type	Mod__uMod_var_i16_u16, #function
	.proc	04
Mod__uMod_var_i16_u16:
.LLFB192:
	save	%sp, -96, %sp
.LLCFI192:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE192:
	.size	Mod__uMod_var_i16_u16, .-Mod__uMod_var_i16_u16
	.align 4
	.type	Mod__Mod_var_i16_u16, #function
	.proc	03
Mod__Mod_var_i16_u16:
.LLFB193:
	save	%sp, -96, %sp
.LLCFI193:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL940
	 nop
	cmp	%g1, 0
	bl	%icc, .LL939
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL943
	 nop
.LL939:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL942
	 nop
.LL940:
	cmp	%g1, 0
	bg	%icc, .LL941
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL943
	 nop
.LL941:
	add	%g1, -1, %g1
.LL942:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL943:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL944
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL944:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL946
	 nop
	cmp	%g1, 0
	bl	%icc, .LL945
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL949
	 nop
.LL945:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL948
	 nop
.LL946:
	cmp	%g1, 0
	bg	%icc, .LL947
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL949
	 nop
.LL947:
	add	%g1, -1, %g1
.LL948:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL949:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE193:
	.size	Mod__Mod_var_i16_u16, .-Mod__Mod_var_i16_u16
	.align 4
	.type	Mod__uMod_param_i16_u16, #function
	.proc	04
Mod__uMod_param_i16_u16:
.LLFB194:
	save	%sp, -96, %sp
.LLCFI194:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE194:
	.size	Mod__uMod_param_i16_u16, .-Mod__uMod_param_i16_u16
	.align 4
	.type	Mod__Mod_param_i16_u16, #function
	.proc	03
Mod__Mod_param_i16_u16:
.LLFB195:
	save	%sp, -96, %sp
.LLCFI195:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL953
	 nop
	cmp	%g1, 0
	bl	%icc, .LL952
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL956
	 nop
.LL952:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL955
	 nop
.LL953:
	cmp	%g1, 0
	bg	%icc, .LL954
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL956
	 nop
.LL954:
	add	%g1, -1, %g1
.LL955:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL956:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL957
	 nop
	sethi	%hi(7168), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL957:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL959
	 nop
	cmp	%g1, 0
	bl	%icc, .LL958
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL962
	 nop
.LL958:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL961
	 nop
.LL959:
	cmp	%g1, 0
	bg	%icc, .LL960
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL962
	 nop
.LL960:
	add	%g1, -1, %g1
.LL961:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL962:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE195:
	.size	Mod__Mod_param_i16_u16, .-Mod__Mod_param_i16_u16
	.align 4
	.type	Mod__uMod_var_i16_u64, #function
	.proc	05
Mod__uMod_var_i16_u64:
.LLFB196:
	save	%sp, -96, %sp
.LLCFI196:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE196:
	.size	Mod__uMod_var_i16_u64, .-Mod__uMod_var_i16_u64
	.align 4
	.type	Mod__Mod_var_i16_u64, #function
	.proc	05
Mod__Mod_var_i16_u64:
.LLFB197:
	save	%sp, -96, %sp
.LLCFI197:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL966
	 nop
	cmp	%l0, 0
	bne	%icc, .LL970
	 nop
	cmp	%l1, 0
.LL970:
	cmp	%o4, 0
	bl	%icc, .LL965
	 nop
	cmp	%o4, 0
	bne	%icc, .LL971
	 nop
	cmp	%o5, 0
.LL971:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL969
	 nop
.LL965:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL968
	 nop
.LL966:
	cmp	%o4, 0
	bg	%icc, .LL967
	 nop
	cmp	%o4, 0
	bne	%icc, .LL972
	 nop
	cmp	%o5, 0
	bne	%icc, .LL967
	 nop
	cmp	%o5, 0
.LL972:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL969
	 nop
.LL967:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL968:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL969:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE197:
	.size	Mod__Mod_var_i16_u64, .-Mod__Mod_var_i16_u64
	.align 4
	.type	Mod__uMod_param_i16_u64, #function
	.proc	05
Mod__uMod_param_i16_u64:
.LLFB198:
	save	%sp, -104, %sp
.LLCFI198:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE198:
	.size	Mod__uMod_param_i16_u64, .-Mod__uMod_param_i16_u64
	.align 4
	.type	Mod__Mod_param_i16_u64, #function
	.proc	05
Mod__Mod_param_i16_u64:
.LLFB199:
	save	%sp, -104, %sp
.LLCFI199:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL976
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL980
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL980:
	cmp	%o4, 0
	bl	%icc, .LL975
	 nop
	cmp	%o4, 0
	bne	%icc, .LL981
	 nop
	cmp	%o5, 0
.LL981:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL979
	 nop
.LL975:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL978
	 nop
.LL976:
	cmp	%o4, 0
	bg	%icc, .LL977
	 nop
	cmp	%o4, 0
	bne	%icc, .LL982
	 nop
	cmp	%o5, 0
	bne	%icc, .LL977
	 nop
	cmp	%o5, 0
.LL982:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL979
	 nop
.LL977:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL978:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL979:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE199:
	.size	Mod__Mod_param_i16_u64, .-Mod__Mod_param_i16_u64
	.align 4
	.type	Mod__uMod_var_i16_LC, #function
	.proc	05
Mod__uMod_var_i16_LC:
.LLFB200:
	save	%sp, -96, %sp
.LLCFI200:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE200:
	.size	Mod__uMod_var_i16_LC, .-Mod__uMod_var_i16_LC
	.align 4
	.type	Mod__Mod_var_i16_LC, #function
	.proc	05
Mod__Mod_var_i16_LC:
.LLFB201:
	save	%sp, -96, %sp
.LLCFI201:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL986
	 nop
	cmp	%l0, 0
	bne	%icc, .LL990
	 nop
	cmp	%l1, 0
.LL990:
	cmp	%o4, 0
	bl	%icc, .LL985
	 nop
	cmp	%o4, 0
	bne	%icc, .LL991
	 nop
	cmp	%o5, 0
.LL991:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL989
	 nop
.LL985:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL988
	 nop
.LL986:
	cmp	%o4, 0
	bg	%icc, .LL987
	 nop
	cmp	%o4, 0
	bne	%icc, .LL992
	 nop
	cmp	%o5, 0
	bne	%icc, .LL987
	 nop
	cmp	%o5, 0
.LL992:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL989
	 nop
.LL987:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL988:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL989:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE201:
	.size	Mod__Mod_var_i16_LC, .-Mod__Mod_var_i16_LC
	.align 4
	.type	Mod__uMod_param_i16_LC, #function
	.proc	05
Mod__uMod_param_i16_LC:
.LLFB202:
	save	%sp, -104, %sp
.LLCFI202:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE202:
	.size	Mod__uMod_param_i16_LC, .-Mod__uMod_param_i16_LC
	.align 4
	.type	Mod__Mod_param_i16_LC, #function
	.proc	05
Mod__Mod_param_i16_LC:
.LLFB203:
	save	%sp, -104, %sp
.LLCFI203:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL996
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1000
	 nop
	cmp	%l1, 0
.LL1000:
	cmp	%o4, 0
	bl	%icc, .LL995
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1001
	 nop
	cmp	%o5, 0
.LL1001:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL999
	 nop
.LL995:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL998
	 nop
.LL996:
	cmp	%o4, 0
	bg	%icc, .LL997
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1002
	 nop
	cmp	%o5, 0
	bne	%icc, .LL997
	 nop
	cmp	%o5, 0
.LL1002:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL999
	 nop
.LL997:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL998:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL999:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE203:
	.size	Mod__Mod_param_i16_LC, .-Mod__Mod_param_i16_LC
	.align 4
	.type	Mod__uMod_var_i16_i32, #function
	.proc	04
Mod__uMod_var_i16_i32:
.LLFB204:
	save	%sp, -96, %sp
.LLCFI204:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE204:
	.size	Mod__uMod_var_i16_i32, .-Mod__uMod_var_i16_i32
	.align 4
	.type	Mod__Mod_var_i16_i32, #function
	.proc	03
Mod__Mod_var_i16_i32:
.LLFB205:
	save	%sp, -96, %sp
.LLCFI205:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1006
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1005
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1009
	 nop
.LL1005:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1008
	 nop
.LL1006:
	cmp	%g1, 0
	bg	%icc, .LL1007
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1009
	 nop
.LL1007:
	add	%g1, -1, %g1
.LL1008:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1009:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1010
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1012
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1011
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1015
	 nop
.LL1011:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1014
	 nop
.LL1012:
	cmp	%g1, 0
	bg	%icc, .LL1013
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1015
	 nop
.LL1013:
	add	%g1, -1, %g1
.LL1014:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1015:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1016
	 nop
.LL1010:
	sethi	%hi(8192), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1016:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1018
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1017
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1021
	 nop
.LL1017:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1020
	 nop
.LL1018:
	cmp	%g1, 0
	bg	%icc, .LL1019
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1021
	 nop
.LL1019:
	add	%g1, -1, %g1
.LL1020:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1021:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE205:
	.size	Mod__Mod_var_i16_i32, .-Mod__Mod_var_i16_i32
	.align 4
	.type	Mod__uMod_param_i16_i32, #function
	.proc	04
Mod__uMod_param_i16_i32:
.LLFB206:
	save	%sp, -96, %sp
.LLCFI206:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE206:
	.size	Mod__uMod_param_i16_i32, .-Mod__uMod_param_i16_i32
	.align 4
	.type	Mod__Mod_param_i16_i32, #function
	.proc	03
Mod__Mod_param_i16_i32:
.LLFB207:
	save	%sp, -96, %sp
.LLCFI207:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1025
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1024
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1028
	 nop
.LL1024:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1027
	 nop
.LL1025:
	cmp	%g1, 0
	bg	%icc, .LL1026
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1028
	 nop
.LL1026:
	add	%g1, -1, %g1
.LL1027:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1028:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1029
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1031
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1030
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1034
	 nop
.LL1030:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1033
	 nop
.LL1031:
	cmp	%g1, 0
	bg	%icc, .LL1032
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1034
	 nop
.LL1032:
	add	%g1, -1, %g1
.LL1033:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1034:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1035
	 nop
.LL1029:
	sethi	%hi(8192), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1035:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1037
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1036
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1040
	 nop
.LL1036:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1039
	 nop
.LL1037:
	cmp	%g1, 0
	bg	%icc, .LL1038
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1040
	 nop
.LL1038:
	add	%g1, -1, %g1
.LL1039:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1040:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE207:
	.size	Mod__Mod_param_i16_i32, .-Mod__Mod_param_i16_i32
	.align 4
	.type	Mod__uMod_var_i16_i16, #function
	.proc	04
Mod__uMod_var_i16_i16:
.LLFB208:
	save	%sp, -96, %sp
.LLCFI208:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE208:
	.size	Mod__uMod_var_i16_i16, .-Mod__uMod_var_i16_i16
	.align 4
	.type	Mod__Mod_var_i16_i16, #function
	.proc	03
Mod__Mod_var_i16_i16:
.LLFB209:
	save	%sp, -96, %sp
.LLCFI209:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1044
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1043
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1047
	 nop
.LL1043:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1046
	 nop
.LL1044:
	cmp	%g1, 0
	bg	%icc, .LL1045
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1047
	 nop
.LL1045:
	add	%g1, -1, %g1
.LL1046:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1047:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1048
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1050
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1049
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1053
	 nop
.LL1049:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1052
	 nop
.LL1050:
	cmp	%g1, 0
	bg	%icc, .LL1051
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1053
	 nop
.LL1051:
	add	%g1, -1, %g1
.LL1052:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1053:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1054
	 nop
.LL1048:
	sethi	%hi(8192), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL1054:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1056
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1055
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1059
	 nop
.LL1055:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1058
	 nop
.LL1056:
	cmp	%g1, 0
	bg	%icc, .LL1057
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1059
	 nop
.LL1057:
	add	%g1, -1, %g1
.LL1058:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1059:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE209:
	.size	Mod__Mod_var_i16_i16, .-Mod__Mod_var_i16_i16
	.align 4
	.type	Mod__uMod_param_i16_i16, #function
	.proc	04
Mod__uMod_param_i16_i16:
.LLFB210:
	save	%sp, -96, %sp
.LLCFI210:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE210:
	.size	Mod__uMod_param_i16_i16, .-Mod__uMod_param_i16_i16
	.align 4
	.type	Mod__Mod_param_i16_i16, #function
	.proc	03
Mod__Mod_param_i16_i16:
.LLFB211:
	save	%sp, -96, %sp
.LLCFI211:
	sth	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1063
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1062
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1066
	 nop
.LL1062:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1065
	 nop
.LL1063:
	cmp	%g1, 0
	bg	%icc, .LL1064
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1066
	 nop
.LL1064:
	add	%g1, -1, %g1
.LL1065:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1066:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1067
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1069
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1068
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1072
	 nop
.LL1068:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1071
	 nop
.LL1069:
	cmp	%g1, 0
	bg	%icc, .LL1070
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1072
	 nop
.LL1070:
	add	%g1, -1, %g1
.LL1071:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1072:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1073
	 nop
.LL1067:
	sethi	%hi(8192), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL1073:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1075
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1074
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1078
	 nop
.LL1074:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1077
	 nop
.LL1075:
	cmp	%g1, 0
	bg	%icc, .LL1076
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1078
	 nop
.LL1076:
	add	%g1, -1, %g1
.LL1077:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1078:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE211:
	.size	Mod__Mod_param_i16_i16, .-Mod__Mod_param_i16_i16
	.align 4
	.type	Mod__uMod_var_i16_I, #function
	.proc	04
Mod__uMod_var_i16_I:
.LLFB212:
	save	%sp, -96, %sp
.LLCFI212:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE212:
	.size	Mod__uMod_var_i16_I, .-Mod__uMod_var_i16_I
	.align 4
	.type	Mod__Mod_var_i16_I, #function
	.proc	03
Mod__Mod_var_i16_I:
.LLFB213:
	save	%sp, -96, %sp
.LLCFI213:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1082
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1081
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1085
	 nop
.LL1081:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1084
	 nop
.LL1082:
	cmp	%g1, 0
	bg	%icc, .LL1083
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1085
	 nop
.LL1083:
	add	%g1, -1, %g1
.LL1084:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1085:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1086
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1088
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1087
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1091
	 nop
.LL1087:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1090
	 nop
.LL1088:
	cmp	%g1, 0
	bg	%icc, .LL1089
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1091
	 nop
.LL1089:
	add	%g1, -1, %g1
.LL1090:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1091:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1092
	 nop
.LL1086:
	sethi	%hi(8192), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL1092:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1094
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1093
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1097
	 nop
.LL1093:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1096
	 nop
.LL1094:
	cmp	%g1, 0
	bg	%icc, .LL1095
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1097
	 nop
.LL1095:
	add	%g1, -1, %g1
.LL1096:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1097:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE213:
	.size	Mod__Mod_var_i16_I, .-Mod__Mod_var_i16_I
	.align 4
	.type	Mod__uMod_param_i16_I, #function
	.proc	04
Mod__uMod_param_i16_I:
.LLFB214:
	save	%sp, -96, %sp
.LLCFI214:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE214:
	.size	Mod__uMod_param_i16_I, .-Mod__uMod_param_i16_I
	.align 4
	.type	Mod__Mod_param_i16_I, #function
	.proc	03
Mod__Mod_param_i16_I:
.LLFB215:
	save	%sp, -96, %sp
.LLCFI215:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1101
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1100
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1104
	 nop
.LL1100:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1103
	 nop
.LL1101:
	cmp	%g1, 0
	bg	%icc, .LL1102
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1104
	 nop
.LL1102:
	add	%g1, -1, %g1
.LL1103:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1104:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1105
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1107
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1106
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1110
	 nop
.LL1106:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1109
	 nop
.LL1107:
	cmp	%g1, 0
	bg	%icc, .LL1108
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1110
	 nop
.LL1108:
	add	%g1, -1, %g1
.LL1109:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1110:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1111
	 nop
.LL1105:
	sethi	%hi(8192), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL1111:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1113
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1112
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1116
	 nop
.LL1112:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1115
	 nop
.LL1113:
	cmp	%g1, 0
	bg	%icc, .LL1114
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1116
	 nop
.LL1114:
	add	%g1, -1, %g1
.LL1115:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1116:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE215:
	.size	Mod__Mod_param_i16_I, .-Mod__Mod_param_i16_I
	.align 4
	.type	Mod__uMod_var_i16_i64, #function
	.proc	05
Mod__uMod_var_i16_i64:
.LLFB216:
	save	%sp, -96, %sp
.LLCFI216:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE216:
	.size	Mod__uMod_var_i16_i64, .-Mod__uMod_var_i16_i64
	.align 4
	.type	Mod__Mod_var_i16_i64, #function
	.proc	05
Mod__Mod_var_i16_i64:
.LLFB217:
	save	%sp, -96, %sp
.LLCFI217:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1120
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1124
	 nop
	cmp	%l1, 0
.LL1124:
	cmp	%o4, 0
	bl	%icc, .LL1119
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1125
	 nop
	cmp	%o5, 0
.LL1125:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1123
	 nop
.LL1119:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1122
	 nop
.LL1120:
	cmp	%o4, 0
	bg	%icc, .LL1121
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1126
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1121
	 nop
	cmp	%o5, 0
.LL1126:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1123
	 nop
.LL1121:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1122:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1123:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE217:
	.size	Mod__Mod_var_i16_i64, .-Mod__Mod_var_i16_i64
	.align 4
	.type	Mod__uMod_param_i16_i64, #function
	.proc	05
Mod__uMod_param_i16_i64:
.LLFB218:
	save	%sp, -104, %sp
.LLCFI218:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE218:
	.size	Mod__uMod_param_i16_i64, .-Mod__uMod_param_i16_i64
	.align 4
	.type	Mod__Mod_param_i16_i64, #function
	.proc	05
Mod__Mod_param_i16_i64:
.LLFB219:
	save	%sp, -104, %sp
.LLCFI219:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1130
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1134
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1134:
	cmp	%o4, 0
	bl	%icc, .LL1129
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1135
	 nop
	cmp	%o5, 0
.LL1135:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1133
	 nop
.LL1129:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1132
	 nop
.LL1130:
	cmp	%o4, 0
	bg	%icc, .LL1131
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1136
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1131
	 nop
	cmp	%o5, 0
.LL1136:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1133
	 nop
.LL1131:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1132:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1133:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE219:
	.size	Mod__Mod_param_i16_i64, .-Mod__Mod_param_i16_i64
	.align 4
	.type	Mod__uMod_var_i16_C, #function
	.proc	04
Mod__uMod_var_i16_C:
.LLFB220:
	save	%sp, -96, %sp
.LLCFI220:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE220:
	.size	Mod__uMod_var_i16_C, .-Mod__uMod_var_i16_C
	.align 4
	.type	Mod__Mod_var_i16_C, #function
	.proc	03
Mod__Mod_var_i16_C:
.LLFB221:
	save	%sp, -96, %sp
.LLCFI221:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1140
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1139
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1143
	 nop
.LL1139:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1142
	 nop
.LL1140:
	cmp	%g1, 0
	bg	%icc, .LL1141
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1143
	 nop
.LL1141:
	add	%g1, -1, %g1
.LL1142:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1143:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1144
	 nop
	sethi	%hi(8192), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1144:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1146
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1145
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1149
	 nop
.LL1145:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1148
	 nop
.LL1146:
	cmp	%g1, 0
	bg	%icc, .LL1147
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1149
	 nop
.LL1147:
	add	%g1, -1, %g1
.LL1148:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1149:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE221:
	.size	Mod__Mod_var_i16_C, .-Mod__Mod_var_i16_C
	.align 4
	.type	Mod__uMod_param_i16_C, #function
	.proc	04
Mod__uMod_param_i16_C:
.LLFB222:
	save	%sp, -96, %sp
.LLCFI222:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE222:
	.size	Mod__uMod_param_i16_C, .-Mod__uMod_param_i16_C
	.align 4
	.type	Mod__Mod_param_i16_C, #function
	.proc	03
Mod__Mod_param_i16_C:
.LLFB223:
	save	%sp, -96, %sp
.LLCFI223:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1153
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1152
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1156
	 nop
.LL1152:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1155
	 nop
.LL1153:
	cmp	%g1, 0
	bg	%icc, .LL1154
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1156
	 nop
.LL1154:
	add	%g1, -1, %g1
.LL1155:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1156:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1157
	 nop
	sethi	%hi(8192), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1157:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1159
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1158
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1162
	 nop
.LL1158:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1161
	 nop
.LL1159:
	cmp	%g1, 0
	bg	%icc, .LL1160
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1162
	 nop
.LL1160:
	add	%g1, -1, %g1
.LL1161:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1162:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE223:
	.size	Mod__Mod_param_i16_C, .-Mod__Mod_param_i16_C
	.align 4
	.type	Mod__uMod_var_i16_u8, #function
	.proc	04
Mod__uMod_var_i16_u8:
.LLFB224:
	save	%sp, -96, %sp
.LLCFI224:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE224:
	.size	Mod__uMod_var_i16_u8, .-Mod__uMod_var_i16_u8
	.align 4
	.type	Mod__Mod_var_i16_u8, #function
	.proc	03
Mod__Mod_var_i16_u8:
.LLFB225:
	save	%sp, -96, %sp
.LLCFI225:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1166
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1165
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1169
	 nop
.LL1165:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1168
	 nop
.LL1166:
	cmp	%g1, 0
	bg	%icc, .LL1167
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1169
	 nop
.LL1167:
	add	%g1, -1, %g1
.LL1168:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1169:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE225:
	.size	Mod__Mod_var_i16_u8, .-Mod__Mod_var_i16_u8
	.align 4
	.type	Mod__uMod_param_i16_u8, #function
	.proc	04
Mod__uMod_param_i16_u8:
.LLFB226:
	save	%sp, -96, %sp
.LLCFI226:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE226:
	.size	Mod__uMod_param_i16_u8, .-Mod__uMod_param_i16_u8
	.align 4
	.type	Mod__Mod_param_i16_u8, #function
	.proc	03
Mod__Mod_param_i16_u8:
.LLFB227:
	save	%sp, -96, %sp
.LLCFI227:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1173
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1172
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1176
	 nop
.LL1172:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1175
	 nop
.LL1173:
	cmp	%g1, 0
	bg	%icc, .LL1174
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1176
	 nop
.LL1174:
	add	%g1, -1, %g1
.LL1175:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1176:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE227:
	.size	Mod__Mod_param_i16_u8, .-Mod__Mod_param_i16_u8
	.align 4
	.type	Mod__uMod_var_i16_L, #function
	.proc	05
Mod__uMod_var_i16_L:
.LLFB228:
	save	%sp, -96, %sp
.LLCFI228:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE228:
	.size	Mod__uMod_var_i16_L, .-Mod__uMod_var_i16_L
	.align 4
	.type	Mod__Mod_var_i16_L, #function
	.proc	05
Mod__Mod_var_i16_L:
.LLFB229:
	save	%sp, -96, %sp
.LLCFI229:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1180
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1184
	 nop
	cmp	%l1, 0
.LL1184:
	cmp	%o4, 0
	bl	%icc, .LL1179
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1185
	 nop
	cmp	%o5, 0
.LL1185:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1183
	 nop
.LL1179:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1182
	 nop
.LL1180:
	cmp	%o4, 0
	bg	%icc, .LL1181
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1186
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1181
	 nop
	cmp	%o5, 0
.LL1186:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1183
	 nop
.LL1181:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1182:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1183:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE229:
	.size	Mod__Mod_var_i16_L, .-Mod__Mod_var_i16_L
	.align 4
	.type	Mod__uMod_param_i16_L, #function
	.proc	05
Mod__uMod_param_i16_L:
.LLFB230:
	save	%sp, -104, %sp
.LLCFI230:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE230:
	.size	Mod__uMod_param_i16_L, .-Mod__uMod_param_i16_L
	.align 4
	.type	Mod__Mod_param_i16_L, #function
	.proc	05
Mod__Mod_param_i16_L:
.LLFB231:
	save	%sp, -104, %sp
.LLCFI231:
	sth	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1190
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1194
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1194:
	cmp	%o4, 0
	bl	%icc, .LL1189
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1195
	 nop
	cmp	%o5, 0
.LL1195:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1193
	 nop
.LL1189:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1192
	 nop
.LL1190:
	cmp	%o4, 0
	bg	%icc, .LL1191
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1196
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1191
	 nop
	cmp	%o5, 0
.LL1196:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1193
	 nop
.LL1191:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1192:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1193:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE231:
	.size	Mod__Mod_param_i16_L, .-Mod__Mod_param_i16_L
	.align 4
	.type	Mod__uMod_var_i16_i8, #function
	.proc	04
Mod__uMod_var_i16_i8:
.LLFB232:
	save	%sp, -96, %sp
.LLCFI232:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE232:
	.size	Mod__uMod_var_i16_i8, .-Mod__uMod_var_i16_i8
	.align 4
	.type	Mod__Mod_var_i16_i8, #function
	.proc	03
Mod__Mod_var_i16_i8:
.LLFB233:
	save	%sp, -96, %sp
.LLCFI233:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1200
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1199
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1203
	 nop
.LL1199:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1202
	 nop
.LL1200:
	cmp	%g1, 0
	bg	%icc, .LL1201
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1203
	 nop
.LL1201:
	add	%g1, -1, %g1
.LL1202:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1203:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1204
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1206
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1205
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1209
	 nop
.LL1205:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1208
	 nop
.LL1206:
	cmp	%g1, 0
	bg	%icc, .LL1207
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1209
	 nop
.LL1207:
	add	%g1, -1, %g1
.LL1208:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1209:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1210
	 nop
.LL1204:
	sethi	%hi(8192), %g1
	or	%g1, 993, %o0
	call	_m3_fault, 0
	 nop
.LL1210:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1212
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1211
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1215
	 nop
.LL1211:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1214
	 nop
.LL1212:
	cmp	%g1, 0
	bg	%icc, .LL1213
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1215
	 nop
.LL1213:
	add	%g1, -1, %g1
.LL1214:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1215:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE233:
	.size	Mod__Mod_var_i16_i8, .-Mod__Mod_var_i16_i8
	.align 4
	.type	Mod__uMod_param_i16_i8, #function
	.proc	04
Mod__uMod_param_i16_i8:
.LLFB234:
	save	%sp, -96, %sp
.LLCFI234:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE234:
	.size	Mod__uMod_param_i16_i8, .-Mod__uMod_param_i16_i8
	.align 4
	.type	Mod__Mod_param_i16_i8, #function
	.proc	03
Mod__Mod_param_i16_i8:
.LLFB235:
	save	%sp, -96, %sp
.LLCFI235:
	sth	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1219
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1218
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1222
	 nop
.LL1218:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1221
	 nop
.LL1219:
	cmp	%g1, 0
	bg	%icc, .LL1220
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1222
	 nop
.LL1220:
	add	%g1, -1, %g1
.LL1221:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1222:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1223
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1225
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1224
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1228
	 nop
.LL1224:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1227
	 nop
.LL1225:
	cmp	%g1, 0
	bg	%icc, .LL1226
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1228
	 nop
.LL1226:
	add	%g1, -1, %g1
.LL1227:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1228:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1229
	 nop
.LL1223:
	sethi	%hi(9216), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL1229:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1231
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1230
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1234
	 nop
.LL1230:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1233
	 nop
.LL1231:
	cmp	%g1, 0
	bg	%icc, .LL1232
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1234
	 nop
.LL1232:
	add	%g1, -1, %g1
.LL1233:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1234:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE235:
	.size	Mod__Mod_param_i16_i8, .-Mod__Mod_param_i16_i8
	.align 4
	.type	Mod__uMod_var_i16_u32, #function
	.proc	04
Mod__uMod_var_i16_u32:
.LLFB236:
	save	%sp, -96, %sp
.LLCFI236:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE236:
	.size	Mod__uMod_var_i16_u32, .-Mod__uMod_var_i16_u32
	.align 4
	.type	Mod__Mod_var_i16_u32, #function
	.proc	03
Mod__Mod_var_i16_u32:
.LLFB237:
	save	%sp, -96, %sp
.LLCFI237:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1238
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1237
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1241
	 nop
.LL1237:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1240
	 nop
.LL1238:
	cmp	%g1, 0
	bg	%icc, .LL1239
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1241
	 nop
.LL1239:
	add	%g1, -1, %g1
.LL1240:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1241:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1242
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1244
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1243
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1247
	 nop
.LL1243:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1246
	 nop
.LL1244:
	cmp	%g1, 0
	bg	%icc, .LL1245
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1247
	 nop
.LL1245:
	add	%g1, -1, %g1
.LL1246:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1247:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1248
	 nop
.LL1242:
	sethi	%hi(9216), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL1248:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1250
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1249
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1253
	 nop
.LL1249:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1252
	 nop
.LL1250:
	cmp	%g1, 0
	bg	%icc, .LL1251
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1253
	 nop
.LL1251:
	add	%g1, -1, %g1
.LL1252:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1253:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE237:
	.size	Mod__Mod_var_i16_u32, .-Mod__Mod_var_i16_u32
	.align 4
	.type	Mod__uMod_param_i16_u32, #function
	.proc	04
Mod__uMod_param_i16_u32:
.LLFB238:
	save	%sp, -96, %sp
.LLCFI238:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE238:
	.size	Mod__uMod_param_i16_u32, .-Mod__uMod_param_i16_u32
	.align 4
	.type	Mod__Mod_param_i16_u32, #function
	.proc	03
Mod__Mod_param_i16_u32:
.LLFB239:
	save	%sp, -96, %sp
.LLCFI239:
	sth	%i0, [%fp+68]
	st	%i1, [%fp+72]
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1257
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1256
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1260
	 nop
.LL1256:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1259
	 nop
.LL1257:
	cmp	%g1, 0
	bg	%icc, .LL1258
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1260
	 nop
.LL1258:
	add	%g1, -1, %g1
.LL1259:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1260:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(-32768), %g1
	cmp	%o5, %g1
	bl	%icc, .LL1261
	 nop
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1263
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1262
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1266
	 nop
.LL1262:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1265
	 nop
.LL1263:
	cmp	%g1, 0
	bg	%icc, .LL1264
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1266
	 nop
.LL1264:
	add	%g1, -1, %g1
.LL1265:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1266:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %o5
	sethi	%hi(31744), %g1
	or	%g1, 1023, %g1
	cmp	%o5, %g1
	ble	%icc, .LL1267
	 nop
.LL1261:
	sethi	%hi(9216), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL1267:
	lduh	[%fp+68], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1269
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1268
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1272
	 nop
.LL1268:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1271
	 nop
.LL1269:
	cmp	%g1, 0
	bg	%icc, .LL1270
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1272
	 nop
.LL1270:
	add	%g1, -1, %g1
.LL1271:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1272:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE239:
	.size	Mod__Mod_param_i16_u32, .-Mod__Mod_param_i16_u32
	.align 4
	.type	Mod__uMod_var_I_u16, #function
	.proc	04
Mod__uMod_var_I_u16:
.LLFB240:
	save	%sp, -96, %sp
.LLCFI240:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE240:
	.size	Mod__uMod_var_I_u16, .-Mod__uMod_var_I_u16
	.align 4
	.type	Mod__Mod_var_I_u16, #function
	.proc	04
Mod__Mod_var_I_u16:
.LLFB241:
	save	%sp, -96, %sp
.LLCFI241:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1276
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1275
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1279
	 nop
.LL1275:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1278
	 nop
.LL1276:
	cmp	%g1, 0
	bg	%icc, .LL1277
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1279
	 nop
.LL1277:
	add	%g1, -1, %g1
.LL1278:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1279:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE241:
	.size	Mod__Mod_var_I_u16, .-Mod__Mod_var_I_u16
	.align 4
	.type	Mod__uMod_param_I_u16, #function
	.proc	04
Mod__uMod_param_I_u16:
.LLFB242:
	save	%sp, -96, %sp
.LLCFI242:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE242:
	.size	Mod__uMod_param_I_u16, .-Mod__uMod_param_I_u16
	.align 4
	.type	Mod__Mod_param_I_u16, #function
	.proc	04
Mod__Mod_param_I_u16:
.LLFB243:
	save	%sp, -96, %sp
.LLCFI243:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1283
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1282
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1286
	 nop
.LL1282:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1285
	 nop
.LL1283:
	cmp	%g1, 0
	bg	%icc, .LL1284
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1286
	 nop
.LL1284:
	add	%g1, -1, %g1
.LL1285:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1286:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE243:
	.size	Mod__Mod_param_I_u16, .-Mod__Mod_param_I_u16
	.align 4
	.type	Mod__uMod_var_I_u64, #function
	.proc	05
Mod__uMod_var_I_u64:
.LLFB244:
	save	%sp, -96, %sp
.LLCFI244:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE244:
	.size	Mod__uMod_var_I_u64, .-Mod__uMod_var_I_u64
	.align 4
	.type	Mod__Mod_var_I_u64, #function
	.proc	05
Mod__Mod_var_I_u64:
.LLFB245:
	save	%sp, -96, %sp
.LLCFI245:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1290
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1294
	 nop
	cmp	%l1, 0
.LL1294:
	cmp	%o4, 0
	bl	%icc, .LL1289
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1295
	 nop
	cmp	%o5, 0
.LL1295:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1293
	 nop
.LL1289:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1292
	 nop
.LL1290:
	cmp	%o4, 0
	bg	%icc, .LL1291
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1296
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1291
	 nop
	cmp	%o5, 0
.LL1296:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1293
	 nop
.LL1291:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1292:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1293:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE245:
	.size	Mod__Mod_var_I_u64, .-Mod__Mod_var_I_u64
	.align 4
	.type	Mod__uMod_param_I_u64, #function
	.proc	05
Mod__uMod_param_I_u64:
.LLFB246:
	save	%sp, -104, %sp
.LLCFI246:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE246:
	.size	Mod__uMod_param_I_u64, .-Mod__uMod_param_I_u64
	.align 4
	.type	Mod__Mod_param_I_u64, #function
	.proc	05
Mod__Mod_param_I_u64:
.LLFB247:
	save	%sp, -104, %sp
.LLCFI247:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1300
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1304
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1304:
	cmp	%o4, 0
	bl	%icc, .LL1299
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1305
	 nop
	cmp	%o5, 0
.LL1305:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1303
	 nop
.LL1299:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1302
	 nop
.LL1300:
	cmp	%o4, 0
	bg	%icc, .LL1301
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1306
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1301
	 nop
	cmp	%o5, 0
.LL1306:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1303
	 nop
.LL1301:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1302:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1303:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE247:
	.size	Mod__Mod_param_I_u64, .-Mod__Mod_param_I_u64
	.align 4
	.type	Mod__uMod_var_I_LC, #function
	.proc	05
Mod__uMod_var_I_LC:
.LLFB248:
	save	%sp, -96, %sp
.LLCFI248:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE248:
	.size	Mod__uMod_var_I_LC, .-Mod__uMod_var_I_LC
	.align 4
	.type	Mod__Mod_var_I_LC, #function
	.proc	05
Mod__Mod_var_I_LC:
.LLFB249:
	save	%sp, -96, %sp
.LLCFI249:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1310
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1314
	 nop
	cmp	%l1, 0
.LL1314:
	cmp	%o4, 0
	bl	%icc, .LL1309
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1315
	 nop
	cmp	%o5, 0
.LL1315:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1313
	 nop
.LL1309:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1312
	 nop
.LL1310:
	cmp	%o4, 0
	bg	%icc, .LL1311
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1316
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1311
	 nop
	cmp	%o5, 0
.LL1316:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1313
	 nop
.LL1311:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1312:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1313:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE249:
	.size	Mod__Mod_var_I_LC, .-Mod__Mod_var_I_LC
	.align 4
	.type	Mod__uMod_param_I_LC, #function
	.proc	05
Mod__uMod_param_I_LC:
.LLFB250:
	save	%sp, -104, %sp
.LLCFI250:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE250:
	.size	Mod__uMod_param_I_LC, .-Mod__uMod_param_I_LC
	.align 4
	.type	Mod__Mod_param_I_LC, #function
	.proc	05
Mod__Mod_param_I_LC:
.LLFB251:
	save	%sp, -104, %sp
.LLCFI251:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l3
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l2
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1320
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1324
	 nop
	cmp	%l1, 0
.LL1324:
	cmp	%o4, 0
	bl	%icc, .LL1319
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1325
	 nop
	cmp	%o5, 0
.LL1325:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1323
	 nop
.LL1319:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1322
	 nop
.LL1320:
	cmp	%o4, 0
	bg	%icc, .LL1321
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1326
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1321
	 nop
	cmp	%o5, 0
.LL1326:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1323
	 nop
.LL1321:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1322:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1323:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE251:
	.size	Mod__Mod_param_I_LC, .-Mod__Mod_param_I_LC
	.align 4
	.type	Mod__uMod_var_I_i32, #function
	.proc	04
Mod__uMod_var_I_i32:
.LLFB252:
	save	%sp, -96, %sp
.LLCFI252:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE252:
	.size	Mod__uMod_var_I_i32, .-Mod__uMod_var_I_i32
	.align 4
	.type	Mod__Mod_var_I_i32, #function
	.proc	04
Mod__Mod_var_I_i32:
.LLFB253:
	save	%sp, -96, %sp
.LLCFI253:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1330
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1329
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1333
	 nop
.LL1329:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1332
	 nop
.LL1330:
	cmp	%g1, 0
	bg	%icc, .LL1331
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1333
	 nop
.LL1331:
	add	%g1, -1, %g1
.LL1332:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1333:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE253:
	.size	Mod__Mod_var_I_i32, .-Mod__Mod_var_I_i32
	.align 4
	.type	Mod__uMod_param_I_i32, #function
	.proc	04
Mod__uMod_param_I_i32:
.LLFB254:
	save	%sp, -96, %sp
.LLCFI254:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE254:
	.size	Mod__uMod_param_I_i32, .-Mod__uMod_param_I_i32
	.align 4
	.type	Mod__Mod_param_I_i32, #function
	.proc	04
Mod__Mod_param_I_i32:
.LLFB255:
	save	%sp, -96, %sp
.LLCFI255:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1337
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1336
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1340
	 nop
.LL1336:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1339
	 nop
.LL1337:
	cmp	%g1, 0
	bg	%icc, .LL1338
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1340
	 nop
.LL1338:
	add	%g1, -1, %g1
.LL1339:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1340:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE255:
	.size	Mod__Mod_param_I_i32, .-Mod__Mod_param_I_i32
	.align 4
	.type	Mod__uMod_var_I_i16, #function
	.proc	04
Mod__uMod_var_I_i16:
.LLFB256:
	save	%sp, -96, %sp
.LLCFI256:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE256:
	.size	Mod__uMod_var_I_i16, .-Mod__uMod_var_I_i16
	.align 4
	.type	Mod__Mod_var_I_i16, #function
	.proc	04
Mod__Mod_var_I_i16:
.LLFB257:
	save	%sp, -96, %sp
.LLCFI257:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1344
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1343
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1347
	 nop
.LL1343:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1346
	 nop
.LL1344:
	cmp	%g1, 0
	bg	%icc, .LL1345
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1347
	 nop
.LL1345:
	add	%g1, -1, %g1
.LL1346:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1347:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE257:
	.size	Mod__Mod_var_I_i16, .-Mod__Mod_var_I_i16
	.align 4
	.type	Mod__uMod_param_I_i16, #function
	.proc	04
Mod__uMod_param_I_i16:
.LLFB258:
	save	%sp, -96, %sp
.LLCFI258:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE258:
	.size	Mod__uMod_param_I_i16, .-Mod__uMod_param_I_i16
	.align 4
	.type	Mod__Mod_param_I_i16, #function
	.proc	04
Mod__Mod_param_I_i16:
.LLFB259:
	save	%sp, -96, %sp
.LLCFI259:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1351
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1350
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1354
	 nop
.LL1350:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1353
	 nop
.LL1351:
	cmp	%g1, 0
	bg	%icc, .LL1352
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1354
	 nop
.LL1352:
	add	%g1, -1, %g1
.LL1353:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1354:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE259:
	.size	Mod__Mod_param_I_i16, .-Mod__Mod_param_I_i16
	.align 4
	.type	Mod__uMod_var_I_I, #function
	.proc	04
Mod__uMod_var_I_I:
.LLFB260:
	save	%sp, -96, %sp
.LLCFI260:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE260:
	.size	Mod__uMod_var_I_I, .-Mod__uMod_var_I_I
	.align 4
	.type	Mod__Mod_var_I_I, #function
	.proc	04
Mod__Mod_var_I_I:
.LLFB261:
	save	%sp, -96, %sp
.LLCFI261:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1358
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1357
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1361
	 nop
.LL1357:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1360
	 nop
.LL1358:
	cmp	%g1, 0
	bg	%icc, .LL1359
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1361
	 nop
.LL1359:
	add	%g1, -1, %g1
.LL1360:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1361:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE261:
	.size	Mod__Mod_var_I_I, .-Mod__Mod_var_I_I
	.align 4
	.type	Mod__uMod_param_I_I, #function
	.proc	04
Mod__uMod_param_I_I:
.LLFB262:
	save	%sp, -96, %sp
.LLCFI262:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE262:
	.size	Mod__uMod_param_I_I, .-Mod__uMod_param_I_I
	.align 4
	.type	Mod__Mod_param_I_I, #function
	.proc	04
Mod__Mod_param_I_I:
.LLFB263:
	save	%sp, -96, %sp
.LLCFI263:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1365
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1364
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1368
	 nop
.LL1364:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1367
	 nop
.LL1365:
	cmp	%g1, 0
	bg	%icc, .LL1366
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1368
	 nop
.LL1366:
	add	%g1, -1, %g1
.LL1367:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1368:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE263:
	.size	Mod__Mod_param_I_I, .-Mod__Mod_param_I_I
	.align 4
	.type	Mod__uMod_var_I_i64, #function
	.proc	05
Mod__uMod_var_I_i64:
.LLFB264:
	save	%sp, -96, %sp
.LLCFI264:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE264:
	.size	Mod__uMod_var_I_i64, .-Mod__uMod_var_I_i64
	.align 4
	.type	Mod__Mod_var_I_i64, #function
	.proc	05
Mod__Mod_var_I_i64:
.LLFB265:
	save	%sp, -96, %sp
.LLCFI265:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1372
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1376
	 nop
	cmp	%l1, 0
.LL1376:
	cmp	%o4, 0
	bl	%icc, .LL1371
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1377
	 nop
	cmp	%o5, 0
.LL1377:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1375
	 nop
.LL1371:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1374
	 nop
.LL1372:
	cmp	%o4, 0
	bg	%icc, .LL1373
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1378
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1373
	 nop
	cmp	%o5, 0
.LL1378:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1375
	 nop
.LL1373:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1374:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1375:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE265:
	.size	Mod__Mod_var_I_i64, .-Mod__Mod_var_I_i64
	.align 4
	.type	Mod__uMod_param_I_i64, #function
	.proc	05
Mod__uMod_param_I_i64:
.LLFB266:
	save	%sp, -104, %sp
.LLCFI266:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE266:
	.size	Mod__uMod_param_I_i64, .-Mod__uMod_param_I_i64
	.align 4
	.type	Mod__Mod_param_I_i64, #function
	.proc	05
Mod__Mod_param_I_i64:
.LLFB267:
	save	%sp, -104, %sp
.LLCFI267:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1382
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1386
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1386:
	cmp	%o4, 0
	bl	%icc, .LL1381
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1387
	 nop
	cmp	%o5, 0
.LL1387:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1385
	 nop
.LL1381:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1384
	 nop
.LL1382:
	cmp	%o4, 0
	bg	%icc, .LL1383
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1388
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1383
	 nop
	cmp	%o5, 0
.LL1388:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1385
	 nop
.LL1383:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1384:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1385:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE267:
	.size	Mod__Mod_param_I_i64, .-Mod__Mod_param_I_i64
	.align 4
	.type	Mod__uMod_var_I_C, #function
	.proc	04
Mod__uMod_var_I_C:
.LLFB268:
	save	%sp, -96, %sp
.LLCFI268:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE268:
	.size	Mod__uMod_var_I_C, .-Mod__uMod_var_I_C
	.align 4
	.type	Mod__Mod_var_I_C, #function
	.proc	04
Mod__Mod_var_I_C:
.LLFB269:
	save	%sp, -96, %sp
.LLCFI269:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1392
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1391
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1395
	 nop
.LL1391:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1394
	 nop
.LL1392:
	cmp	%g1, 0
	bg	%icc, .LL1393
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1395
	 nop
.LL1393:
	add	%g1, -1, %g1
.LL1394:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1395:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE269:
	.size	Mod__Mod_var_I_C, .-Mod__Mod_var_I_C
	.align 4
	.type	Mod__uMod_param_I_C, #function
	.proc	04
Mod__uMod_param_I_C:
.LLFB270:
	save	%sp, -96, %sp
.LLCFI270:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE270:
	.size	Mod__uMod_param_I_C, .-Mod__uMod_param_I_C
	.align 4
	.type	Mod__Mod_param_I_C, #function
	.proc	04
Mod__Mod_param_I_C:
.LLFB271:
	save	%sp, -96, %sp
.LLCFI271:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1399
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1398
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1402
	 nop
.LL1398:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1401
	 nop
.LL1399:
	cmp	%g1, 0
	bg	%icc, .LL1400
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1402
	 nop
.LL1400:
	add	%g1, -1, %g1
.LL1401:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1402:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE271:
	.size	Mod__Mod_param_I_C, .-Mod__Mod_param_I_C
	.align 4
	.type	Mod__uMod_var_I_u8, #function
	.proc	04
Mod__uMod_var_I_u8:
.LLFB272:
	save	%sp, -96, %sp
.LLCFI272:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE272:
	.size	Mod__uMod_var_I_u8, .-Mod__uMod_var_I_u8
	.align 4
	.type	Mod__Mod_var_I_u8, #function
	.proc	04
Mod__Mod_var_I_u8:
.LLFB273:
	save	%sp, -96, %sp
.LLCFI273:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1406
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1405
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1409
	 nop
.LL1405:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1408
	 nop
.LL1406:
	cmp	%g1, 0
	bg	%icc, .LL1407
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1409
	 nop
.LL1407:
	add	%g1, -1, %g1
.LL1408:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1409:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE273:
	.size	Mod__Mod_var_I_u8, .-Mod__Mod_var_I_u8
	.align 4
	.type	Mod__uMod_param_I_u8, #function
	.proc	04
Mod__uMod_param_I_u8:
.LLFB274:
	save	%sp, -96, %sp
.LLCFI274:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE274:
	.size	Mod__uMod_param_I_u8, .-Mod__uMod_param_I_u8
	.align 4
	.type	Mod__Mod_param_I_u8, #function
	.proc	04
Mod__Mod_param_I_u8:
.LLFB275:
	save	%sp, -96, %sp
.LLCFI275:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1413
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1412
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1416
	 nop
.LL1412:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1415
	 nop
.LL1413:
	cmp	%g1, 0
	bg	%icc, .LL1414
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1416
	 nop
.LL1414:
	add	%g1, -1, %g1
.LL1415:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1416:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE275:
	.size	Mod__Mod_param_I_u8, .-Mod__Mod_param_I_u8
	.align 4
	.type	Mod__uMod_var_I_L, #function
	.proc	05
Mod__uMod_var_I_L:
.LLFB276:
	save	%sp, -96, %sp
.LLCFI276:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE276:
	.size	Mod__uMod_var_I_L, .-Mod__uMod_var_I_L
	.align 4
	.type	Mod__Mod_var_I_L, #function
	.proc	05
Mod__Mod_var_I_L:
.LLFB277:
	save	%sp, -96, %sp
.LLCFI277:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1420
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1424
	 nop
	cmp	%l1, 0
.LL1424:
	cmp	%o4, 0
	bl	%icc, .LL1419
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1425
	 nop
	cmp	%o5, 0
.LL1425:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1423
	 nop
.LL1419:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1422
	 nop
.LL1420:
	cmp	%o4, 0
	bg	%icc, .LL1421
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1426
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1421
	 nop
	cmp	%o5, 0
.LL1426:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1423
	 nop
.LL1421:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1422:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1423:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE277:
	.size	Mod__Mod_var_I_L, .-Mod__Mod_var_I_L
	.align 4
	.type	Mod__uMod_param_I_L, #function
	.proc	05
Mod__uMod_param_I_L:
.LLFB278:
	save	%sp, -104, %sp
.LLCFI278:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE278:
	.size	Mod__uMod_param_I_L, .-Mod__uMod_param_I_L
	.align 4
	.type	Mod__Mod_param_I_L, #function
	.proc	05
Mod__Mod_param_I_L:
.LLFB279:
	save	%sp, -104, %sp
.LLCFI279:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1430
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1434
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1434:
	cmp	%o4, 0
	bl	%icc, .LL1429
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1435
	 nop
	cmp	%o5, 0
.LL1435:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1433
	 nop
.LL1429:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1432
	 nop
.LL1430:
	cmp	%o4, 0
	bg	%icc, .LL1431
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1436
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1431
	 nop
	cmp	%o5, 0
.LL1436:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1433
	 nop
.LL1431:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1432:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1433:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE279:
	.size	Mod__Mod_param_I_L, .-Mod__Mod_param_I_L
	.align 4
	.type	Mod__uMod_var_I_i8, #function
	.proc	04
Mod__uMod_var_I_i8:
.LLFB280:
	save	%sp, -96, %sp
.LLCFI280:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE280:
	.size	Mod__uMod_var_I_i8, .-Mod__uMod_var_I_i8
	.align 4
	.type	Mod__Mod_var_I_i8, #function
	.proc	04
Mod__Mod_var_I_i8:
.LLFB281:
	save	%sp, -96, %sp
.LLCFI281:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1440
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1439
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1443
	 nop
.LL1439:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1442
	 nop
.LL1440:
	cmp	%g1, 0
	bg	%icc, .LL1441
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1443
	 nop
.LL1441:
	add	%g1, -1, %g1
.LL1442:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1443:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE281:
	.size	Mod__Mod_var_I_i8, .-Mod__Mod_var_I_i8
	.align 4
	.type	Mod__uMod_param_I_i8, #function
	.proc	04
Mod__uMod_param_I_i8:
.LLFB282:
	save	%sp, -96, %sp
.LLCFI282:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE282:
	.size	Mod__uMod_param_I_i8, .-Mod__uMod_param_I_i8
	.align 4
	.type	Mod__Mod_param_I_i8, #function
	.proc	04
Mod__Mod_param_I_i8:
.LLFB283:
	save	%sp, -96, %sp
.LLCFI283:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1447
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1446
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1450
	 nop
.LL1446:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1449
	 nop
.LL1447:
	cmp	%g1, 0
	bg	%icc, .LL1448
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1450
	 nop
.LL1448:
	add	%g1, -1, %g1
.LL1449:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1450:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE283:
	.size	Mod__Mod_param_I_i8, .-Mod__Mod_param_I_i8
	.align 4
	.type	Mod__uMod_var_I_u32, #function
	.proc	04
Mod__uMod_var_I_u32:
.LLFB284:
	save	%sp, -96, %sp
.LLCFI284:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE284:
	.size	Mod__uMod_var_I_u32, .-Mod__uMod_var_I_u32
	.align 4
	.type	Mod__Mod_var_I_u32, #function
	.proc	04
Mod__Mod_var_I_u32:
.LLFB285:
	save	%sp, -96, %sp
.LLCFI285:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1454
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1453
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1457
	 nop
.LL1453:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1456
	 nop
.LL1454:
	cmp	%g1, 0
	bg	%icc, .LL1455
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1457
	 nop
.LL1455:
	add	%g1, -1, %g1
.LL1456:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1457:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE285:
	.size	Mod__Mod_var_I_u32, .-Mod__Mod_var_I_u32
	.align 4
	.type	Mod__uMod_param_I_u32, #function
	.proc	04
Mod__uMod_param_I_u32:
.LLFB286:
	save	%sp, -96, %sp
.LLCFI286:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE286:
	.size	Mod__uMod_param_I_u32, .-Mod__uMod_param_I_u32
	.align 4
	.type	Mod__Mod_param_I_u32, #function
	.proc	04
Mod__Mod_param_I_u32:
.LLFB287:
	save	%sp, -96, %sp
.LLCFI287:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1461
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1460
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1464
	 nop
.LL1460:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1463
	 nop
.LL1461:
	cmp	%g1, 0
	bg	%icc, .LL1462
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1464
	 nop
.LL1462:
	add	%g1, -1, %g1
.LL1463:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1464:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE287:
	.size	Mod__Mod_param_I_u32, .-Mod__Mod_param_I_u32
	.align 4
	.type	Mod__uMod_var_i64_u16, #function
	.proc	05
Mod__uMod_var_i64_u16:
.LLFB288:
	save	%sp, -96, %sp
.LLCFI288:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE288:
	.size	Mod__uMod_var_i64_u16, .-Mod__uMod_var_i64_u16
	.align 4
	.type	Mod__Mod_var_i64_u16, #function
	.proc	05
Mod__Mod_var_i64_u16:
.LLFB289:
	save	%sp, -96, %sp
.LLCFI289:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1468
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1472
	 nop
	cmp	%l1, 0
.LL1472:
	cmp	%o4, 0
	bl	%icc, .LL1467
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1473
	 nop
	cmp	%o5, 0
.LL1473:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1471
	 nop
.LL1467:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1470
	 nop
.LL1468:
	cmp	%o4, 0
	bg	%icc, .LL1469
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1474
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1469
	 nop
	cmp	%o5, 0
.LL1474:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1471
	 nop
.LL1469:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1470:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1471:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE289:
	.size	Mod__Mod_var_i64_u16, .-Mod__Mod_var_i64_u16
	.align 4
	.type	Mod__uMod_param_i64_u16, #function
	.proc	05
Mod__uMod_param_i64_u16:
.LLFB290:
	save	%sp, -104, %sp
.LLCFI290:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE290:
	.size	Mod__uMod_param_i64_u16, .-Mod__uMod_param_i64_u16
	.align 4
	.type	Mod__Mod_param_i64_u16, #function
	.proc	05
Mod__Mod_param_i64_u16:
.LLFB291:
	save	%sp, -104, %sp
.LLCFI291:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1478
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1482
	 nop
	cmp	%l1, 0
.LL1482:
	cmp	%o4, 0
	bl	%icc, .LL1477
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1483
	 nop
	cmp	%o5, 0
.LL1483:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1481
	 nop
.LL1477:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1480
	 nop
.LL1478:
	cmp	%o4, 0
	bg	%icc, .LL1479
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1484
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1479
	 nop
	cmp	%o5, 0
.LL1484:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1481
	 nop
.LL1479:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1480:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1481:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE291:
	.size	Mod__Mod_param_i64_u16, .-Mod__Mod_param_i64_u16
	.align 4
	.type	Mod__uMod_var_i64_u64, #function
	.proc	05
Mod__uMod_var_i64_u64:
.LLFB292:
	save	%sp, -96, %sp
.LLCFI292:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE292:
	.size	Mod__uMod_var_i64_u64, .-Mod__uMod_var_i64_u64
	.align 4
	.type	Mod__Mod_var_i64_u64, #function
	.proc	05
Mod__Mod_var_i64_u64:
.LLFB293:
	save	%sp, -96, %sp
.LLCFI293:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1488
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1492
	 nop
	cmp	%l1, 0
.LL1492:
	cmp	%o4, 0
	bl	%icc, .LL1487
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1493
	 nop
	cmp	%o5, 0
.LL1493:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1491
	 nop
.LL1487:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1490
	 nop
.LL1488:
	cmp	%o4, 0
	bg	%icc, .LL1489
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1494
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1489
	 nop
	cmp	%o5, 0
.LL1494:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1491
	 nop
.LL1489:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1490:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1491:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE293:
	.size	Mod__Mod_var_i64_u64, .-Mod__Mod_var_i64_u64
	.align 4
	.type	Mod__uMod_param_i64_u64, #function
	.proc	05
Mod__uMod_param_i64_u64:
.LLFB294:
	save	%sp, -112, %sp
.LLCFI294:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE294:
	.size	Mod__uMod_param_i64_u64, .-Mod__uMod_param_i64_u64
	.align 4
	.type	Mod__Mod_param_i64_u64, #function
	.proc	05
Mod__Mod_param_i64_u64:
.LLFB295:
	save	%sp, -112, %sp
.LLCFI295:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL1498
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL1502
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL1502:
	cmp	%o4, 0
	bl	%icc, .LL1497
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1503
	 nop
	cmp	%o5, 0
.LL1503:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1501
	 nop
.LL1497:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1500
	 nop
.LL1498:
	cmp	%o4, 0
	bg	%icc, .LL1499
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1504
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1499
	 nop
	cmp	%o5, 0
.LL1504:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1501
	 nop
.LL1499:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1500:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1501:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE295:
	.size	Mod__Mod_param_i64_u64, .-Mod__Mod_param_i64_u64
	.align 4
	.type	Mod__uMod_var_i64_LC, #function
	.proc	05
Mod__uMod_var_i64_LC:
.LLFB296:
	save	%sp, -96, %sp
.LLCFI296:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE296:
	.size	Mod__uMod_var_i64_LC, .-Mod__uMod_var_i64_LC
	.align 4
	.type	Mod__Mod_var_i64_LC, #function
	.proc	05
Mod__Mod_var_i64_LC:
.LLFB297:
	save	%sp, -96, %sp
.LLCFI297:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1508
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1512
	 nop
	cmp	%l1, 0
.LL1512:
	cmp	%o4, 0
	bl	%icc, .LL1507
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1513
	 nop
	cmp	%o5, 0
.LL1513:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1511
	 nop
.LL1507:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1510
	 nop
.LL1508:
	cmp	%o4, 0
	bg	%icc, .LL1509
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1514
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1509
	 nop
	cmp	%o5, 0
.LL1514:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1511
	 nop
.LL1509:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1510:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1511:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE297:
	.size	Mod__Mod_var_i64_LC, .-Mod__Mod_var_i64_LC
	.align 4
	.type	Mod__uMod_param_i64_LC, #function
	.proc	05
Mod__uMod_param_i64_LC:
.LLFB298:
	save	%sp, -112, %sp
.LLCFI298:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE298:
	.size	Mod__uMod_param_i64_LC, .-Mod__uMod_param_i64_LC
	.align 4
	.type	Mod__Mod_param_i64_LC, #function
	.proc	05
Mod__Mod_param_i64_LC:
.LLFB299:
	save	%sp, -112, %sp
.LLCFI299:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1518
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1522
	 nop
	cmp	%l1, 0
.LL1522:
	cmp	%o4, 0
	bl	%icc, .LL1517
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1523
	 nop
	cmp	%o5, 0
.LL1523:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1521
	 nop
.LL1517:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1520
	 nop
.LL1518:
	cmp	%o4, 0
	bg	%icc, .LL1519
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1524
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1519
	 nop
	cmp	%o5, 0
.LL1524:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1521
	 nop
.LL1519:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1520:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1521:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE299:
	.size	Mod__Mod_param_i64_LC, .-Mod__Mod_param_i64_LC
	.align 4
	.type	Mod__uMod_var_i64_i32, #function
	.proc	05
Mod__uMod_var_i64_i32:
.LLFB300:
	save	%sp, -96, %sp
.LLCFI300:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE300:
	.size	Mod__uMod_var_i64_i32, .-Mod__uMod_var_i64_i32
	.align 4
	.type	Mod__Mod_var_i64_i32, #function
	.proc	05
Mod__Mod_var_i64_i32:
.LLFB301:
	save	%sp, -96, %sp
.LLCFI301:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1528
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1532
	 nop
	cmp	%l1, 0
.LL1532:
	cmp	%o4, 0
	bl	%icc, .LL1527
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1533
	 nop
	cmp	%o5, 0
.LL1533:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1531
	 nop
.LL1527:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1530
	 nop
.LL1528:
	cmp	%o4, 0
	bg	%icc, .LL1529
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1534
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1529
	 nop
	cmp	%o5, 0
.LL1534:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1531
	 nop
.LL1529:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1530:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1531:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE301:
	.size	Mod__Mod_var_i64_i32, .-Mod__Mod_var_i64_i32
	.align 4
	.type	Mod__uMod_param_i64_i32, #function
	.proc	05
Mod__uMod_param_i64_i32:
.LLFB302:
	save	%sp, -104, %sp
.LLCFI302:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE302:
	.size	Mod__uMod_param_i64_i32, .-Mod__uMod_param_i64_i32
	.align 4
	.type	Mod__Mod_param_i64_i32, #function
	.proc	05
Mod__Mod_param_i64_i32:
.LLFB303:
	save	%sp, -104, %sp
.LLCFI303:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1538
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1542
	 nop
	cmp	%l1, 0
.LL1542:
	cmp	%o4, 0
	bl	%icc, .LL1537
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1543
	 nop
	cmp	%o5, 0
.LL1543:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1541
	 nop
.LL1537:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1540
	 nop
.LL1538:
	cmp	%o4, 0
	bg	%icc, .LL1539
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1544
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1539
	 nop
	cmp	%o5, 0
.LL1544:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1541
	 nop
.LL1539:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1540:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1541:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE303:
	.size	Mod__Mod_param_i64_i32, .-Mod__Mod_param_i64_i32
	.align 4
	.type	Mod__uMod_var_i64_i16, #function
	.proc	05
Mod__uMod_var_i64_i16:
.LLFB304:
	save	%sp, -96, %sp
.LLCFI304:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE304:
	.size	Mod__uMod_var_i64_i16, .-Mod__uMod_var_i64_i16
	.align 4
	.type	Mod__Mod_var_i64_i16, #function
	.proc	05
Mod__Mod_var_i64_i16:
.LLFB305:
	save	%sp, -96, %sp
.LLCFI305:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1548
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1552
	 nop
	cmp	%l1, 0
.LL1552:
	cmp	%o4, 0
	bl	%icc, .LL1547
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1553
	 nop
	cmp	%o5, 0
.LL1553:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1551
	 nop
.LL1547:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1550
	 nop
.LL1548:
	cmp	%o4, 0
	bg	%icc, .LL1549
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1554
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1549
	 nop
	cmp	%o5, 0
.LL1554:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1551
	 nop
.LL1549:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1550:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1551:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE305:
	.size	Mod__Mod_var_i64_i16, .-Mod__Mod_var_i64_i16
	.align 4
	.type	Mod__uMod_param_i64_i16, #function
	.proc	05
Mod__uMod_param_i64_i16:
.LLFB306:
	save	%sp, -104, %sp
.LLCFI306:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE306:
	.size	Mod__uMod_param_i64_i16, .-Mod__uMod_param_i64_i16
	.align 4
	.type	Mod__Mod_param_i64_i16, #function
	.proc	05
Mod__Mod_param_i64_i16:
.LLFB307:
	save	%sp, -104, %sp
.LLCFI307:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1558
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1562
	 nop
	cmp	%l1, 0
.LL1562:
	cmp	%o4, 0
	bl	%icc, .LL1557
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1563
	 nop
	cmp	%o5, 0
.LL1563:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1561
	 nop
.LL1557:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1560
	 nop
.LL1558:
	cmp	%o4, 0
	bg	%icc, .LL1559
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1564
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1559
	 nop
	cmp	%o5, 0
.LL1564:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1561
	 nop
.LL1559:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1560:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1561:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE307:
	.size	Mod__Mod_param_i64_i16, .-Mod__Mod_param_i64_i16
	.align 4
	.type	Mod__uMod_var_i64_I, #function
	.proc	05
Mod__uMod_var_i64_I:
.LLFB308:
	save	%sp, -96, %sp
.LLCFI308:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE308:
	.size	Mod__uMod_var_i64_I, .-Mod__uMod_var_i64_I
	.align 4
	.type	Mod__Mod_var_i64_I, #function
	.proc	05
Mod__Mod_var_i64_I:
.LLFB309:
	save	%sp, -96, %sp
.LLCFI309:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1568
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1572
	 nop
	cmp	%l1, 0
.LL1572:
	cmp	%o4, 0
	bl	%icc, .LL1567
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1573
	 nop
	cmp	%o5, 0
.LL1573:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1571
	 nop
.LL1567:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1570
	 nop
.LL1568:
	cmp	%o4, 0
	bg	%icc, .LL1569
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1574
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1569
	 nop
	cmp	%o5, 0
.LL1574:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1571
	 nop
.LL1569:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1570:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1571:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE309:
	.size	Mod__Mod_var_i64_I, .-Mod__Mod_var_i64_I
	.align 4
	.type	Mod__uMod_param_i64_I, #function
	.proc	05
Mod__uMod_param_i64_I:
.LLFB310:
	save	%sp, -104, %sp
.LLCFI310:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE310:
	.size	Mod__uMod_param_i64_I, .-Mod__uMod_param_i64_I
	.align 4
	.type	Mod__Mod_param_i64_I, #function
	.proc	05
Mod__Mod_param_i64_I:
.LLFB311:
	save	%sp, -104, %sp
.LLCFI311:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1578
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1582
	 nop
	cmp	%l1, 0
.LL1582:
	cmp	%o4, 0
	bl	%icc, .LL1577
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1583
	 nop
	cmp	%o5, 0
.LL1583:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1581
	 nop
.LL1577:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1580
	 nop
.LL1578:
	cmp	%o4, 0
	bg	%icc, .LL1579
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1584
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1579
	 nop
	cmp	%o5, 0
.LL1584:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1581
	 nop
.LL1579:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1580:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1581:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE311:
	.size	Mod__Mod_param_i64_I, .-Mod__Mod_param_i64_I
	.align 4
	.type	Mod__uMod_var_i64_i64, #function
	.proc	05
Mod__uMod_var_i64_i64:
.LLFB312:
	save	%sp, -96, %sp
.LLCFI312:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE312:
	.size	Mod__uMod_var_i64_i64, .-Mod__uMod_var_i64_i64
	.align 4
	.type	Mod__Mod_var_i64_i64, #function
	.proc	05
Mod__Mod_var_i64_i64:
.LLFB313:
	save	%sp, -96, %sp
.LLCFI313:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1588
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1592
	 nop
	cmp	%l1, 0
.LL1592:
	cmp	%o4, 0
	bl	%icc, .LL1587
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1593
	 nop
	cmp	%o5, 0
.LL1593:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1591
	 nop
.LL1587:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1590
	 nop
.LL1588:
	cmp	%o4, 0
	bg	%icc, .LL1589
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1594
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1589
	 nop
	cmp	%o5, 0
.LL1594:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1591
	 nop
.LL1589:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1590:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1591:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE313:
	.size	Mod__Mod_var_i64_i64, .-Mod__Mod_var_i64_i64
	.align 4
	.type	Mod__uMod_param_i64_i64, #function
	.proc	05
Mod__uMod_param_i64_i64:
.LLFB314:
	save	%sp, -112, %sp
.LLCFI314:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE314:
	.size	Mod__uMod_param_i64_i64, .-Mod__uMod_param_i64_i64
	.align 4
	.type	Mod__Mod_param_i64_i64, #function
	.proc	05
Mod__Mod_param_i64_i64:
.LLFB315:
	save	%sp, -112, %sp
.LLCFI315:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL1598
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL1602
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL1602:
	cmp	%o4, 0
	bl	%icc, .LL1597
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1603
	 nop
	cmp	%o5, 0
.LL1603:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1601
	 nop
.LL1597:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1600
	 nop
.LL1598:
	cmp	%o4, 0
	bg	%icc, .LL1599
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1604
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1599
	 nop
	cmp	%o5, 0
.LL1604:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1601
	 nop
.LL1599:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1600:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1601:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE315:
	.size	Mod__Mod_param_i64_i64, .-Mod__Mod_param_i64_i64
	.align 4
	.type	Mod__uMod_var_i64_C, #function
	.proc	05
Mod__uMod_var_i64_C:
.LLFB316:
	save	%sp, -96, %sp
.LLCFI316:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE316:
	.size	Mod__uMod_var_i64_C, .-Mod__uMod_var_i64_C
	.align 4
	.type	Mod__Mod_var_i64_C, #function
	.proc	05
Mod__Mod_var_i64_C:
.LLFB317:
	save	%sp, -96, %sp
.LLCFI317:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1608
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1612
	 nop
	cmp	%l1, 0
.LL1612:
	cmp	%o4, 0
	bl	%icc, .LL1607
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1613
	 nop
	cmp	%o5, 0
.LL1613:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1611
	 nop
.LL1607:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1610
	 nop
.LL1608:
	cmp	%o4, 0
	bg	%icc, .LL1609
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1614
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1609
	 nop
	cmp	%o5, 0
.LL1614:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1611
	 nop
.LL1609:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1610:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1611:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE317:
	.size	Mod__Mod_var_i64_C, .-Mod__Mod_var_i64_C
	.align 4
	.type	Mod__uMod_param_i64_C, #function
	.proc	05
Mod__uMod_param_i64_C:
.LLFB318:
	save	%sp, -104, %sp
.LLCFI318:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE318:
	.size	Mod__uMod_param_i64_C, .-Mod__uMod_param_i64_C
	.align 4
	.type	Mod__Mod_param_i64_C, #function
	.proc	05
Mod__Mod_param_i64_C:
.LLFB319:
	save	%sp, -104, %sp
.LLCFI319:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1618
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1622
	 nop
	cmp	%l1, 0
.LL1622:
	cmp	%o4, 0
	bl	%icc, .LL1617
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1623
	 nop
	cmp	%o5, 0
.LL1623:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1621
	 nop
.LL1617:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1620
	 nop
.LL1618:
	cmp	%o4, 0
	bg	%icc, .LL1619
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1624
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1619
	 nop
	cmp	%o5, 0
.LL1624:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1621
	 nop
.LL1619:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1620:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1621:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE319:
	.size	Mod__Mod_param_i64_C, .-Mod__Mod_param_i64_C
	.align 4
	.type	Mod__uMod_var_i64_u8, #function
	.proc	05
Mod__uMod_var_i64_u8:
.LLFB320:
	save	%sp, -96, %sp
.LLCFI320:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE320:
	.size	Mod__uMod_var_i64_u8, .-Mod__uMod_var_i64_u8
	.align 4
	.type	Mod__Mod_var_i64_u8, #function
	.proc	05
Mod__Mod_var_i64_u8:
.LLFB321:
	save	%sp, -96, %sp
.LLCFI321:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1628
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1632
	 nop
	cmp	%l1, 0
.LL1632:
	cmp	%o4, 0
	bl	%icc, .LL1627
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1633
	 nop
	cmp	%o5, 0
.LL1633:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1631
	 nop
.LL1627:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1630
	 nop
.LL1628:
	cmp	%o4, 0
	bg	%icc, .LL1629
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1634
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1629
	 nop
	cmp	%o5, 0
.LL1634:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1631
	 nop
.LL1629:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1630:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1631:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE321:
	.size	Mod__Mod_var_i64_u8, .-Mod__Mod_var_i64_u8
	.align 4
	.type	Mod__uMod_param_i64_u8, #function
	.proc	05
Mod__uMod_param_i64_u8:
.LLFB322:
	save	%sp, -104, %sp
.LLCFI322:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE322:
	.size	Mod__uMod_param_i64_u8, .-Mod__uMod_param_i64_u8
	.align 4
	.type	Mod__Mod_param_i64_u8, #function
	.proc	05
Mod__Mod_param_i64_u8:
.LLFB323:
	save	%sp, -104, %sp
.LLCFI323:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1638
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1642
	 nop
	cmp	%l1, 0
.LL1642:
	cmp	%o4, 0
	bl	%icc, .LL1637
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1643
	 nop
	cmp	%o5, 0
.LL1643:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1641
	 nop
.LL1637:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1640
	 nop
.LL1638:
	cmp	%o4, 0
	bg	%icc, .LL1639
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1644
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1639
	 nop
	cmp	%o5, 0
.LL1644:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1641
	 nop
.LL1639:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1640:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1641:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE323:
	.size	Mod__Mod_param_i64_u8, .-Mod__Mod_param_i64_u8
	.align 4
	.type	Mod__uMod_var_i64_L, #function
	.proc	05
Mod__uMod_var_i64_L:
.LLFB324:
	save	%sp, -96, %sp
.LLCFI324:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE324:
	.size	Mod__uMod_var_i64_L, .-Mod__uMod_var_i64_L
	.align 4
	.type	Mod__Mod_var_i64_L, #function
	.proc	05
Mod__Mod_var_i64_L:
.LLFB325:
	save	%sp, -96, %sp
.LLCFI325:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1648
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1652
	 nop
	cmp	%l1, 0
.LL1652:
	cmp	%o4, 0
	bl	%icc, .LL1647
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1653
	 nop
	cmp	%o5, 0
.LL1653:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1651
	 nop
.LL1647:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1650
	 nop
.LL1648:
	cmp	%o4, 0
	bg	%icc, .LL1649
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1654
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1649
	 nop
	cmp	%o5, 0
.LL1654:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1651
	 nop
.LL1649:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1650:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1651:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE325:
	.size	Mod__Mod_var_i64_L, .-Mod__Mod_var_i64_L
	.align 4
	.type	Mod__uMod_param_i64_L, #function
	.proc	05
Mod__uMod_param_i64_L:
.LLFB326:
	save	%sp, -112, %sp
.LLCFI326:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE326:
	.size	Mod__uMod_param_i64_L, .-Mod__uMod_param_i64_L
	.align 4
	.type	Mod__Mod_param_i64_L, #function
	.proc	05
Mod__Mod_param_i64_L:
.LLFB327:
	save	%sp, -112, %sp
.LLCFI327:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL1658
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL1662
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL1662:
	cmp	%o4, 0
	bl	%icc, .LL1657
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1663
	 nop
	cmp	%o5, 0
.LL1663:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1661
	 nop
.LL1657:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1660
	 nop
.LL1658:
	cmp	%o4, 0
	bg	%icc, .LL1659
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1664
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1659
	 nop
	cmp	%o5, 0
.LL1664:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1661
	 nop
.LL1659:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1660:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1661:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE327:
	.size	Mod__Mod_param_i64_L, .-Mod__Mod_param_i64_L
	.align 4
	.type	Mod__uMod_var_i64_i8, #function
	.proc	05
Mod__uMod_var_i64_i8:
.LLFB328:
	save	%sp, -96, %sp
.LLCFI328:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE328:
	.size	Mod__uMod_var_i64_i8, .-Mod__uMod_var_i64_i8
	.align 4
	.type	Mod__Mod_var_i64_i8, #function
	.proc	05
Mod__Mod_var_i64_i8:
.LLFB329:
	save	%sp, -96, %sp
.LLCFI329:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1668
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1672
	 nop
	cmp	%l1, 0
.LL1672:
	cmp	%o4, 0
	bl	%icc, .LL1667
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1673
	 nop
	cmp	%o5, 0
.LL1673:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1671
	 nop
.LL1667:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1670
	 nop
.LL1668:
	cmp	%o4, 0
	bg	%icc, .LL1669
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1674
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1669
	 nop
	cmp	%o5, 0
.LL1674:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1671
	 nop
.LL1669:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1670:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1671:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE329:
	.size	Mod__Mod_var_i64_i8, .-Mod__Mod_var_i64_i8
	.align 4
	.type	Mod__uMod_param_i64_i8, #function
	.proc	05
Mod__uMod_param_i64_i8:
.LLFB330:
	save	%sp, -104, %sp
.LLCFI330:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE330:
	.size	Mod__uMod_param_i64_i8, .-Mod__uMod_param_i64_i8
	.align 4
	.type	Mod__Mod_param_i64_i8, #function
	.proc	05
Mod__Mod_param_i64_i8:
.LLFB331:
	save	%sp, -104, %sp
.LLCFI331:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1678
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1682
	 nop
	cmp	%l1, 0
.LL1682:
	cmp	%o4, 0
	bl	%icc, .LL1677
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1683
	 nop
	cmp	%o5, 0
.LL1683:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1681
	 nop
.LL1677:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1680
	 nop
.LL1678:
	cmp	%o4, 0
	bg	%icc, .LL1679
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1684
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1679
	 nop
	cmp	%o5, 0
.LL1684:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1681
	 nop
.LL1679:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1680:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1681:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE331:
	.size	Mod__Mod_param_i64_i8, .-Mod__Mod_param_i64_i8
	.align 4
	.type	Mod__uMod_var_i64_u32, #function
	.proc	05
Mod__uMod_var_i64_u32:
.LLFB332:
	save	%sp, -96, %sp
.LLCFI332:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE332:
	.size	Mod__uMod_var_i64_u32, .-Mod__uMod_var_i64_u32
	.align 4
	.type	Mod__Mod_var_i64_u32, #function
	.proc	05
Mod__Mod_var_i64_u32:
.LLFB333:
	save	%sp, -96, %sp
.LLCFI333:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1688
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1692
	 nop
	cmp	%l1, 0
.LL1692:
	cmp	%o4, 0
	bl	%icc, .LL1687
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1693
	 nop
	cmp	%o5, 0
.LL1693:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1691
	 nop
.LL1687:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1690
	 nop
.LL1688:
	cmp	%o4, 0
	bg	%icc, .LL1689
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1694
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1689
	 nop
	cmp	%o5, 0
.LL1694:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1691
	 nop
.LL1689:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1690:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1691:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE333:
	.size	Mod__Mod_var_i64_u32, .-Mod__Mod_var_i64_u32
	.align 4
	.type	Mod__uMod_param_i64_u32, #function
	.proc	05
Mod__uMod_param_i64_u32:
.LLFB334:
	save	%sp, -104, %sp
.LLCFI334:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE334:
	.size	Mod__uMod_param_i64_u32, .-Mod__uMod_param_i64_u32
	.align 4
	.type	Mod__Mod_param_i64_u32, #function
	.proc	05
Mod__Mod_param_i64_u32:
.LLFB335:
	save	%sp, -104, %sp
.LLCFI335:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1698
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1702
	 nop
	cmp	%l1, 0
.LL1702:
	cmp	%o4, 0
	bl	%icc, .LL1697
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1703
	 nop
	cmp	%o5, 0
.LL1703:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1701
	 nop
.LL1697:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1700
	 nop
.LL1698:
	cmp	%o4, 0
	bg	%icc, .LL1699
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1704
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1699
	 nop
	cmp	%o5, 0
.LL1704:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1701
	 nop
.LL1699:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1700:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1701:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE335:
	.size	Mod__Mod_param_i64_u32, .-Mod__Mod_param_i64_u32
	.align 4
	.type	Mod__uMod_var_C_u16, #function
	.proc	04
Mod__uMod_var_C_u16:
.LLFB336:
	save	%sp, -96, %sp
.LLCFI336:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE336:
	.size	Mod__uMod_var_C_u16, .-Mod__uMod_var_C_u16
	.align 4
	.type	Mod__Mod_var_C_u16, #function
	.proc	016
Mod__Mod_var_C_u16:
.LLFB337:
	save	%sp, -96, %sp
.LLCFI337:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1708
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1707
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1711
	 nop
.LL1707:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1710
	 nop
.LL1708:
	cmp	%g1, 0
	bg	%icc, .LL1709
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1711
	 nop
.LL1709:
	add	%g1, -1, %g1
.LL1710:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1711:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE337:
	.size	Mod__Mod_var_C_u16, .-Mod__Mod_var_C_u16
	.align 4
	.type	Mod__uMod_param_C_u16, #function
	.proc	04
Mod__uMod_param_C_u16:
.LLFB338:
	save	%sp, -96, %sp
.LLCFI338:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE338:
	.size	Mod__uMod_param_C_u16, .-Mod__uMod_param_C_u16
	.align 4
	.type	Mod__Mod_param_C_u16, #function
	.proc	016
Mod__Mod_param_C_u16:
.LLFB339:
	save	%sp, -96, %sp
.LLCFI339:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1715
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1714
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1718
	 nop
.LL1714:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1717
	 nop
.LL1715:
	cmp	%g1, 0
	bg	%icc, .LL1716
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1718
	 nop
.LL1716:
	add	%g1, -1, %g1
.LL1717:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1718:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE339:
	.size	Mod__Mod_param_C_u16, .-Mod__Mod_param_C_u16
	.align 4
	.type	Mod__uMod_var_C_u64, #function
	.proc	05
Mod__uMod_var_C_u64:
.LLFB340:
	save	%sp, -96, %sp
.LLCFI340:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE340:
	.size	Mod__uMod_var_C_u64, .-Mod__uMod_var_C_u64
	.align 4
	.type	Mod__Mod_var_C_u64, #function
	.proc	05
Mod__Mod_var_C_u64:
.LLFB341:
	save	%sp, -96, %sp
.LLCFI341:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1722
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1726
	 nop
	cmp	%l1, 0
.LL1726:
	cmp	%o4, 0
	bl	%icc, .LL1721
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1727
	 nop
	cmp	%o5, 0
.LL1727:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1725
	 nop
.LL1721:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1724
	 nop
.LL1722:
	cmp	%o4, 0
	bg	%icc, .LL1723
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1728
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1723
	 nop
	cmp	%o5, 0
.LL1728:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1725
	 nop
.LL1723:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1724:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1725:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE341:
	.size	Mod__Mod_var_C_u64, .-Mod__Mod_var_C_u64
	.align 4
	.type	Mod__uMod_param_C_u64, #function
	.proc	05
Mod__uMod_param_C_u64:
.LLFB342:
	save	%sp, -104, %sp
.LLCFI342:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE342:
	.size	Mod__uMod_param_C_u64, .-Mod__uMod_param_C_u64
	.align 4
	.type	Mod__Mod_param_C_u64, #function
	.proc	05
Mod__Mod_param_C_u64:
.LLFB343:
	save	%sp, -104, %sp
.LLCFI343:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1732
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1736
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1736:
	cmp	%o4, 0
	bl	%icc, .LL1731
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1737
	 nop
	cmp	%o5, 0
.LL1737:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1735
	 nop
.LL1731:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1734
	 nop
.LL1732:
	cmp	%o4, 0
	bg	%icc, .LL1733
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1738
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1733
	 nop
	cmp	%o5, 0
.LL1738:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1735
	 nop
.LL1733:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1734:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1735:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE343:
	.size	Mod__Mod_param_C_u64, .-Mod__Mod_param_C_u64
	.align 4
	.type	Mod__uMod_var_C_LC, #function
	.proc	05
Mod__uMod_var_C_LC:
.LLFB344:
	save	%sp, -96, %sp
.LLCFI344:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE344:
	.size	Mod__uMod_var_C_LC, .-Mod__uMod_var_C_LC
	.align 4
	.type	Mod__Mod_var_C_LC, #function
	.proc	05
Mod__Mod_var_C_LC:
.LLFB345:
	save	%sp, -96, %sp
.LLCFI345:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1742
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1746
	 nop
	cmp	%l1, 0
.LL1746:
	cmp	%o4, 0
	bl	%icc, .LL1741
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1747
	 nop
	cmp	%o5, 0
.LL1747:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1745
	 nop
.LL1741:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1744
	 nop
.LL1742:
	cmp	%o4, 0
	bg	%icc, .LL1743
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1748
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1743
	 nop
	cmp	%o5, 0
.LL1748:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1745
	 nop
.LL1743:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1744:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1745:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE345:
	.size	Mod__Mod_var_C_LC, .-Mod__Mod_var_C_LC
	.align 4
	.type	Mod__uMod_param_C_LC, #function
	.proc	05
Mod__uMod_param_C_LC:
.LLFB346:
	save	%sp, -104, %sp
.LLCFI346:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE346:
	.size	Mod__uMod_param_C_LC, .-Mod__uMod_param_C_LC
	.align 4
	.type	Mod__Mod_param_C_LC, #function
	.proc	05
Mod__Mod_param_C_LC:
.LLFB347:
	save	%sp, -104, %sp
.LLCFI347:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1752
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1756
	 nop
	cmp	%l1, 0
.LL1756:
	cmp	%o4, 0
	bl	%icc, .LL1751
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1757
	 nop
	cmp	%o5, 0
.LL1757:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1755
	 nop
.LL1751:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1754
	 nop
.LL1752:
	cmp	%o4, 0
	bg	%icc, .LL1753
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1758
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1753
	 nop
	cmp	%o5, 0
.LL1758:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1755
	 nop
.LL1753:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1754:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1755:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE347:
	.size	Mod__Mod_param_C_LC, .-Mod__Mod_param_C_LC
	.align 4
	.type	Mod__uMod_var_C_i32, #function
	.proc	04
Mod__uMod_var_C_i32:
.LLFB348:
	save	%sp, -96, %sp
.LLCFI348:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE348:
	.size	Mod__uMod_var_C_i32, .-Mod__uMod_var_C_i32
	.align 4
	.type	Mod__Mod_var_C_i32, #function
	.proc	016
Mod__Mod_var_C_i32:
.LLFB349:
	save	%sp, -96, %sp
.LLCFI349:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1762
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1761
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1765
	 nop
.LL1761:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1764
	 nop
.LL1762:
	cmp	%g1, 0
	bg	%icc, .LL1763
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1765
	 nop
.LL1763:
	add	%g1, -1, %g1
.LL1764:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1765:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1766
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1766:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1768
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1767
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1771
	 nop
.LL1767:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1770
	 nop
.LL1768:
	cmp	%g1, 0
	bg	%icc, .LL1769
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1771
	 nop
.LL1769:
	add	%g1, -1, %g1
.LL1770:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1771:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE349:
	.size	Mod__Mod_var_C_i32, .-Mod__Mod_var_C_i32
	.align 4
	.type	Mod__uMod_param_C_i32, #function
	.proc	04
Mod__uMod_param_C_i32:
.LLFB350:
	save	%sp, -96, %sp
.LLCFI350:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE350:
	.size	Mod__uMod_param_C_i32, .-Mod__uMod_param_C_i32
	.align 4
	.type	Mod__Mod_param_C_i32, #function
	.proc	016
Mod__Mod_param_C_i32:
.LLFB351:
	save	%sp, -96, %sp
.LLCFI351:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1775
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1774
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1778
	 nop
.LL1774:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1777
	 nop
.LL1775:
	cmp	%g1, 0
	bg	%icc, .LL1776
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1778
	 nop
.LL1776:
	add	%g1, -1, %g1
.LL1777:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1778:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1779
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1779:
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1781
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1780
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1784
	 nop
.LL1780:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1783
	 nop
.LL1781:
	cmp	%g1, 0
	bg	%icc, .LL1782
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1784
	 nop
.LL1782:
	add	%g1, -1, %g1
.LL1783:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1784:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE351:
	.size	Mod__Mod_param_C_i32, .-Mod__Mod_param_C_i32
	.align 4
	.type	Mod__uMod_var_C_i16, #function
	.proc	04
Mod__uMod_var_C_i16:
.LLFB352:
	save	%sp, -96, %sp
.LLCFI352:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE352:
	.size	Mod__uMod_var_C_i16, .-Mod__uMod_var_C_i16
	.align 4
	.type	Mod__Mod_var_C_i16, #function
	.proc	016
Mod__Mod_var_C_i16:
.LLFB353:
	save	%sp, -96, %sp
.LLCFI353:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1788
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1787
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1791
	 nop
.LL1787:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1790
	 nop
.LL1788:
	cmp	%g1, 0
	bg	%icc, .LL1789
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1791
	 nop
.LL1789:
	add	%g1, -1, %g1
.LL1790:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1791:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1792
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL1792:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1794
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1793
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1797
	 nop
.LL1793:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1796
	 nop
.LL1794:
	cmp	%g1, 0
	bg	%icc, .LL1795
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1797
	 nop
.LL1795:
	add	%g1, -1, %g1
.LL1796:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1797:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE353:
	.size	Mod__Mod_var_C_i16, .-Mod__Mod_var_C_i16
	.align 4
	.type	Mod__uMod_param_C_i16, #function
	.proc	04
Mod__uMod_param_C_i16:
.LLFB354:
	save	%sp, -96, %sp
.LLCFI354:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE354:
	.size	Mod__uMod_param_C_i16, .-Mod__uMod_param_C_i16
	.align 4
	.type	Mod__Mod_param_C_i16, #function
	.proc	016
Mod__Mod_param_C_i16:
.LLFB355:
	save	%sp, -96, %sp
.LLCFI355:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1801
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1800
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1804
	 nop
.LL1800:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1803
	 nop
.LL1801:
	cmp	%g1, 0
	bg	%icc, .LL1802
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1804
	 nop
.LL1802:
	add	%g1, -1, %g1
.LL1803:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1804:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1805
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL1805:
	ld	[%fp+68], %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1807
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1806
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1810
	 nop
.LL1806:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1809
	 nop
.LL1807:
	cmp	%g1, 0
	bg	%icc, .LL1808
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1810
	 nop
.LL1808:
	add	%g1, -1, %g1
.LL1809:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1810:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE355:
	.size	Mod__Mod_param_C_i16, .-Mod__Mod_param_C_i16
	.align 4
	.type	Mod__uMod_var_C_I, #function
	.proc	04
Mod__uMod_var_C_I:
.LLFB356:
	save	%sp, -96, %sp
.LLCFI356:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE356:
	.size	Mod__uMod_var_C_I, .-Mod__uMod_var_C_I
	.align 4
	.type	Mod__Mod_var_C_I, #function
	.proc	016
Mod__Mod_var_C_I:
.LLFB357:
	save	%sp, -96, %sp
.LLCFI357:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1814
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1813
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1817
	 nop
.LL1813:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1816
	 nop
.LL1814:
	cmp	%g1, 0
	bg	%icc, .LL1815
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1817
	 nop
.LL1815:
	add	%g1, -1, %g1
.LL1816:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1817:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1818
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1818:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1820
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1819
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1823
	 nop
.LL1819:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1822
	 nop
.LL1820:
	cmp	%g1, 0
	bg	%icc, .LL1821
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1823
	 nop
.LL1821:
	add	%g1, -1, %g1
.LL1822:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1823:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE357:
	.size	Mod__Mod_var_C_I, .-Mod__Mod_var_C_I
	.align 4
	.type	Mod__uMod_param_C_I, #function
	.proc	04
Mod__uMod_param_C_I:
.LLFB358:
	save	%sp, -96, %sp
.LLCFI358:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE358:
	.size	Mod__uMod_param_C_I, .-Mod__uMod_param_C_I
	.align 4
	.type	Mod__Mod_param_C_I, #function
	.proc	016
Mod__Mod_param_C_I:
.LLFB359:
	save	%sp, -96, %sp
.LLCFI359:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1827
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1826
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1830
	 nop
.LL1826:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1829
	 nop
.LL1827:
	cmp	%g1, 0
	bg	%icc, .LL1828
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1830
	 nop
.LL1828:
	add	%g1, -1, %g1
.LL1829:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1830:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1831
	 nop
	sethi	%hi(12288), %g1
	or	%g1, 929, %o0
	call	_m3_fault, 0
	 nop
.LL1831:
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1833
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1832
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1836
	 nop
.LL1832:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1835
	 nop
.LL1833:
	cmp	%g1, 0
	bg	%icc, .LL1834
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1836
	 nop
.LL1834:
	add	%g1, -1, %g1
.LL1835:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1836:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE359:
	.size	Mod__Mod_param_C_I, .-Mod__Mod_param_C_I
	.align 4
	.type	Mod__uMod_var_C_i64, #function
	.proc	05
Mod__uMod_var_C_i64:
.LLFB360:
	save	%sp, -96, %sp
.LLCFI360:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE360:
	.size	Mod__uMod_var_C_i64, .-Mod__uMod_var_C_i64
	.align 4
	.type	Mod__Mod_var_C_i64, #function
	.proc	05
Mod__Mod_var_C_i64:
.LLFB361:
	save	%sp, -96, %sp
.LLCFI361:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1840
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1844
	 nop
	cmp	%l1, 0
.LL1844:
	cmp	%o4, 0
	bl	%icc, .LL1839
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1845
	 nop
	cmp	%o5, 0
.LL1845:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1843
	 nop
.LL1839:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1842
	 nop
.LL1840:
	cmp	%o4, 0
	bg	%icc, .LL1841
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1846
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1841
	 nop
	cmp	%o5, 0
.LL1846:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1843
	 nop
.LL1841:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1842:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1843:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE361:
	.size	Mod__Mod_var_C_i64, .-Mod__Mod_var_C_i64
	.align 4
	.type	Mod__uMod_param_C_i64, #function
	.proc	05
Mod__uMod_param_C_i64:
.LLFB362:
	save	%sp, -104, %sp
.LLCFI362:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE362:
	.size	Mod__uMod_param_C_i64, .-Mod__uMod_param_C_i64
	.align 4
	.type	Mod__Mod_param_C_i64, #function
	.proc	05
Mod__Mod_param_C_i64:
.LLFB363:
	save	%sp, -104, %sp
.LLCFI363:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1850
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1854
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1854:
	cmp	%o4, 0
	bl	%icc, .LL1849
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1855
	 nop
	cmp	%o5, 0
.LL1855:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1853
	 nop
.LL1849:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1852
	 nop
.LL1850:
	cmp	%o4, 0
	bg	%icc, .LL1851
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1856
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1851
	 nop
	cmp	%o5, 0
.LL1856:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1853
	 nop
.LL1851:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1852:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1853:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE363:
	.size	Mod__Mod_param_C_i64, .-Mod__Mod_param_C_i64
	.align 4
	.type	Mod__uMod_var_C_C, #function
	.proc	04
Mod__uMod_var_C_C:
.LLFB364:
	save	%sp, -96, %sp
.LLCFI364:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE364:
	.size	Mod__uMod_var_C_C, .-Mod__uMod_var_C_C
	.align 4
	.type	Mod__Mod_var_C_C, #function
	.proc	016
Mod__Mod_var_C_C:
.LLFB365:
	save	%sp, -96, %sp
.LLCFI365:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1860
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1859
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1863
	 nop
.LL1859:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1862
	 nop
.LL1860:
	cmp	%g1, 0
	bg	%icc, .LL1861
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1863
	 nop
.LL1861:
	add	%g1, -1, %g1
.LL1862:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1863:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE365:
	.size	Mod__Mod_var_C_C, .-Mod__Mod_var_C_C
	.align 4
	.type	Mod__uMod_param_C_C, #function
	.proc	04
Mod__uMod_param_C_C:
.LLFB366:
	save	%sp, -96, %sp
.LLCFI366:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE366:
	.size	Mod__uMod_param_C_C, .-Mod__uMod_param_C_C
	.align 4
	.type	Mod__Mod_param_C_C, #function
	.proc	016
Mod__Mod_param_C_C:
.LLFB367:
	save	%sp, -96, %sp
.LLCFI367:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1867
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1866
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1870
	 nop
.LL1866:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1869
	 nop
.LL1867:
	cmp	%g1, 0
	bg	%icc, .LL1868
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1870
	 nop
.LL1868:
	add	%g1, -1, %g1
.LL1869:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1870:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE367:
	.size	Mod__Mod_param_C_C, .-Mod__Mod_param_C_C
	.align 4
	.type	Mod__uMod_var_C_u8, #function
	.proc	04
Mod__uMod_var_C_u8:
.LLFB368:
	save	%sp, -96, %sp
.LLCFI368:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE368:
	.size	Mod__uMod_var_C_u8, .-Mod__uMod_var_C_u8
	.align 4
	.type	Mod__Mod_var_C_u8, #function
	.proc	016
Mod__Mod_var_C_u8:
.LLFB369:
	save	%sp, -96, %sp
.LLCFI369:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1874
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1873
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1877
	 nop
.LL1873:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1876
	 nop
.LL1874:
	cmp	%g1, 0
	bg	%icc, .LL1875
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1877
	 nop
.LL1875:
	add	%g1, -1, %g1
.LL1876:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1877:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE369:
	.size	Mod__Mod_var_C_u8, .-Mod__Mod_var_C_u8
	.align 4
	.type	Mod__uMod_param_C_u8, #function
	.proc	04
Mod__uMod_param_C_u8:
.LLFB370:
	save	%sp, -96, %sp
.LLCFI370:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE370:
	.size	Mod__uMod_param_C_u8, .-Mod__uMod_param_C_u8
	.align 4
	.type	Mod__Mod_param_C_u8, #function
	.proc	016
Mod__Mod_param_C_u8:
.LLFB371:
	save	%sp, -96, %sp
.LLCFI371:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1881
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1880
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1884
	 nop
.LL1880:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1883
	 nop
.LL1881:
	cmp	%g1, 0
	bg	%icc, .LL1882
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1884
	 nop
.LL1882:
	add	%g1, -1, %g1
.LL1883:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1884:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE371:
	.size	Mod__Mod_param_C_u8, .-Mod__Mod_param_C_u8
	.align 4
	.type	Mod__uMod_var_C_L, #function
	.proc	05
Mod__uMod_var_C_L:
.LLFB372:
	save	%sp, -96, %sp
.LLCFI372:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE372:
	.size	Mod__uMod_var_C_L, .-Mod__uMod_var_C_L
	.align 4
	.type	Mod__Mod_var_C_L, #function
	.proc	05
Mod__Mod_var_C_L:
.LLFB373:
	save	%sp, -96, %sp
.LLCFI373:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1888
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1892
	 nop
	cmp	%l1, 0
.LL1892:
	cmp	%o4, 0
	bl	%icc, .LL1887
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1893
	 nop
	cmp	%o5, 0
.LL1893:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1891
	 nop
.LL1887:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1890
	 nop
.LL1888:
	cmp	%o4, 0
	bg	%icc, .LL1889
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1894
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1889
	 nop
	cmp	%o5, 0
.LL1894:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1891
	 nop
.LL1889:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1890:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1891:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE373:
	.size	Mod__Mod_var_C_L, .-Mod__Mod_var_C_L
	.align 4
	.type	Mod__uMod_param_C_L, #function
	.proc	05
Mod__uMod_param_C_L:
.LLFB374:
	save	%sp, -104, %sp
.LLCFI374:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE374:
	.size	Mod__uMod_param_C_L, .-Mod__uMod_param_C_L
	.align 4
	.type	Mod__Mod_param_C_L, #function
	.proc	05
Mod__Mod_param_C_L:
.LLFB375:
	save	%sp, -104, %sp
.LLCFI375:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL1898
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL1902
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL1902:
	cmp	%o4, 0
	bl	%icc, .LL1897
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1903
	 nop
	cmp	%o5, 0
.LL1903:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1901
	 nop
.LL1897:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1900
	 nop
.LL1898:
	cmp	%o4, 0
	bg	%icc, .LL1899
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1904
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1899
	 nop
	cmp	%o5, 0
.LL1904:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1901
	 nop
.LL1899:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1900:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1901:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE375:
	.size	Mod__Mod_param_C_L, .-Mod__Mod_param_C_L
	.align 4
	.type	Mod__uMod_var_C_i8, #function
	.proc	04
Mod__uMod_var_C_i8:
.LLFB376:
	save	%sp, -96, %sp
.LLCFI376:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE376:
	.size	Mod__uMod_var_C_i8, .-Mod__uMod_var_C_i8
	.align 4
	.type	Mod__Mod_var_C_i8, #function
	.proc	016
Mod__Mod_var_C_i8:
.LLFB377:
	save	%sp, -96, %sp
.LLCFI377:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1908
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1907
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1911
	 nop
.LL1907:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1910
	 nop
.LL1908:
	cmp	%g1, 0
	bg	%icc, .LL1909
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1911
	 nop
.LL1909:
	add	%g1, -1, %g1
.LL1910:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1911:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1912
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL1912:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1914
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1913
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1917
	 nop
.LL1913:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1916
	 nop
.LL1914:
	cmp	%g1, 0
	bg	%icc, .LL1915
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1917
	 nop
.LL1915:
	add	%g1, -1, %g1
.LL1916:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1917:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE377:
	.size	Mod__Mod_var_C_i8, .-Mod__Mod_var_C_i8
	.align 4
	.type	Mod__uMod_param_C_i8, #function
	.proc	04
Mod__uMod_param_C_i8:
.LLFB378:
	save	%sp, -96, %sp
.LLCFI378:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE378:
	.size	Mod__uMod_param_C_i8, .-Mod__uMod_param_C_i8
	.align 4
	.type	Mod__Mod_param_C_i8, #function
	.proc	016
Mod__Mod_param_C_i8:
.LLFB379:
	save	%sp, -96, %sp
.LLCFI379:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1921
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1920
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1924
	 nop
.LL1920:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1923
	 nop
.LL1921:
	cmp	%g1, 0
	bg	%icc, .LL1922
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1924
	 nop
.LL1922:
	add	%g1, -1, %g1
.LL1923:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1924:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1925
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 545, %o0
	call	_m3_fault, 0
	 nop
.LL1925:
	ld	[%fp+68], %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1927
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1926
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1930
	 nop
.LL1926:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1929
	 nop
.LL1927:
	cmp	%g1, 0
	bg	%icc, .LL1928
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1930
	 nop
.LL1928:
	add	%g1, -1, %g1
.LL1929:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1930:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE379:
	.size	Mod__Mod_param_C_i8, .-Mod__Mod_param_C_i8
	.align 4
	.type	Mod__uMod_var_C_u32, #function
	.proc	04
Mod__uMod_var_C_u32:
.LLFB380:
	save	%sp, -96, %sp
.LLCFI380:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE380:
	.size	Mod__uMod_var_C_u32, .-Mod__uMod_var_C_u32
	.align 4
	.type	Mod__Mod_var_C_u32, #function
	.proc	016
Mod__Mod_var_C_u32:
.LLFB381:
	save	%sp, -96, %sp
.LLCFI381:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1934
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1933
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1937
	 nop
.LL1933:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1936
	 nop
.LL1934:
	cmp	%g1, 0
	bg	%icc, .LL1935
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1937
	 nop
.LL1935:
	add	%g1, -1, %g1
.LL1936:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1937:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1938
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 609, %o0
	call	_m3_fault, 0
	 nop
.LL1938:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1940
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1939
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1943
	 nop
.LL1939:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1942
	 nop
.LL1940:
	cmp	%g1, 0
	bg	%icc, .LL1941
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1943
	 nop
.LL1941:
	add	%g1, -1, %g1
.LL1942:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1943:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE381:
	.size	Mod__Mod_var_C_u32, .-Mod__Mod_var_C_u32
	.align 4
	.type	Mod__uMod_param_C_u32, #function
	.proc	04
Mod__uMod_param_C_u32:
.LLFB382:
	save	%sp, -96, %sp
.LLCFI382:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE382:
	.size	Mod__uMod_param_C_u32, .-Mod__uMod_param_C_u32
	.align 4
	.type	Mod__Mod_param_C_u32, #function
	.proc	016
Mod__Mod_param_C_u32:
.LLFB383:
	save	%sp, -96, %sp
.LLCFI383:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1947
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1946
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1950
	 nop
.LL1946:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1949
	 nop
.LL1947:
	cmp	%g1, 0
	bg	%icc, .LL1948
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1950
	 nop
.LL1948:
	add	%g1, -1, %g1
.LL1949:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1950:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 0
	bge	%icc, .LL1951
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL1951:
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL1953
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1952
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1956
	 nop
.LL1952:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1955
	 nop
.LL1953:
	cmp	%g1, 0
	bg	%icc, .LL1954
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL1956
	 nop
.LL1954:
	add	%g1, -1, %g1
.LL1955:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL1956:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE383:
	.size	Mod__Mod_param_C_u32, .-Mod__Mod_param_C_u32
	.align 4
	.type	Mod__Mod_var_f64_f64, #function
	.proc	07
Mod__Mod_var_f64_f64:
.LLFB384:
	save	%sp, -136, %sp
.LLCFI384:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	std	%f8, [%fp-32]
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	add	%g1, 104, %g1
	ldd	[%g1], %f8
	std	%f8, [%fp-24]
	ldd	[%fp-32], %f10
	ldd	[%fp-24], %f8
	fdivd	%f10, %f8, %f8
	std	%f8, [%fp-8]
	ldd	[%fp-8], %f8
	fdtoi	%f8, %f8
	st	%f8, [%fp-12]
	ldd	[%fp-32], %f10
	ld	[%fp-12], %f8
	fitod	%f8, %f12
	ldd	[%fp-8], %f8
	fcmped	%fcc0, %f12, %f8
	mov	0, %g1
	movle	%fcc0, 1, %g1
	and	%g1, 0xff, %g1
	cmp	%g1, 0
	be	%icc, .LL1958
	 nop
	ld	[%fp-12], %f8
	fitod	%f8, %f8
	ba,pt	%xcc, .LL1959
	 nop
.LL1958:
	ld	[%fp-12], %g1
	add	%g1, -1, %g1
	st	%g1, [%fp-36]
	ld	[%fp-36], %f12
	fitod	%f12, %f8
.LL1959:
	ldd	[%fp-24], %f12
	fmuld	%f8, %f12, %f8
	fsubd	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE384:
	.size	Mod__Mod_var_f64_f64, .-Mod__Mod_var_f64_f64
	.align 4
	.type	Mod__Mod_param_f64_f64, #function
	.proc	07
Mod__Mod_param_f64_f64:
.LLFB385:
	save	%sp, -152, %sp
.LLCFI385:
	std	%i0, [%fp-40]
	std	%i2, [%fp-48]
	ldd	[%fp-40], %f8
	std	%f8, [%fp-32]
	ldd	[%fp-48], %f8
	std	%f8, [%fp-24]
	ldd	[%fp-32], %f10
	ldd	[%fp-24], %f8
	fdivd	%f10, %f8, %f8
	std	%f8, [%fp-8]
	ldd	[%fp-8], %f8
	fdtoi	%f8, %f8
	st	%f8, [%fp-12]
	ldd	[%fp-32], %f10
	ld	[%fp-12], %f8
	fitod	%f8, %f12
	ldd	[%fp-8], %f8
	fcmped	%fcc1, %f12, %f8
	mov	0, %g1
	movle	%fcc1, 1, %g1
	and	%g1, 0xff, %g1
	cmp	%g1, 0
	be	%icc, .LL1961
	 nop
	ld	[%fp-12], %f8
	fitod	%f8, %f8
	ba,pt	%xcc, .LL1962
	 nop
.LL1961:
	ld	[%fp-12], %g1
	add	%g1, -1, %g1
	st	%g1, [%fp-52]
	ld	[%fp-52], %f12
	fitod	%f12, %f8
.LL1962:
	ldd	[%fp-24], %f12
	fmuld	%f8, %f12, %f8
	fsubd	%f10, %f8, %f8
	fmovd	%f8, %f0
	return	%i7+8
	 nop
.LLFE385:
	.size	Mod__Mod_param_f64_f64, .-Mod__Mod_param_f64_f64
	.align 4
	.type	Mod__uMod_var_u8_u16, #function
	.proc	04
Mod__uMod_var_u8_u16:
.LLFB386:
	save	%sp, -96, %sp
.LLCFI386:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE386:
	.size	Mod__uMod_var_u8_u16, .-Mod__uMod_var_u8_u16
	.align 4
	.type	Mod__Mod_var_u8_u16, #function
	.proc	014
Mod__Mod_var_u8_u16:
.LLFB387:
	save	%sp, -96, %sp
.LLCFI387:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1966
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1965
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1969
	 nop
.LL1965:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1968
	 nop
.LL1966:
	cmp	%g1, 0
	bg	%icc, .LL1967
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1969
	 nop
.LL1967:
	add	%g1, -1, %g1
.LL1968:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1969:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL1970
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL1970:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1972
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1971
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1975
	 nop
.LL1971:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1974
	 nop
.LL1972:
	cmp	%g1, 0
	bg	%icc, .LL1973
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1975
	 nop
.LL1973:
	add	%g1, -1, %g1
.LL1974:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1975:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE387:
	.size	Mod__Mod_var_u8_u16, .-Mod__Mod_var_u8_u16
	.align 4
	.type	Mod__uMod_param_u8_u16, #function
	.proc	04
Mod__uMod_param_u8_u16:
.LLFB388:
	save	%sp, -96, %sp
.LLCFI388:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE388:
	.size	Mod__uMod_param_u8_u16, .-Mod__uMod_param_u8_u16
	.align 4
	.type	Mod__Mod_param_u8_u16, #function
	.proc	014
Mod__Mod_param_u8_u16:
.LLFB389:
	save	%sp, -96, %sp
.LLCFI389:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1979
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1978
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1982
	 nop
.LL1978:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1981
	 nop
.LL1979:
	cmp	%g1, 0
	bg	%icc, .LL1980
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1982
	 nop
.LL1980:
	add	%g1, -1, %g1
.LL1981:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1982:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL1983
	 nop
	sethi	%hi(13312), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL1983:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL1985
	 nop
	cmp	%g1, 0
	bl	%icc, .LL1984
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1988
	 nop
.LL1984:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL1987
	 nop
.LL1985:
	cmp	%g1, 0
	bg	%icc, .LL1986
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL1988
	 nop
.LL1986:
	add	%g1, -1, %g1
.LL1987:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL1988:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE389:
	.size	Mod__Mod_param_u8_u16, .-Mod__Mod_param_u8_u16
	.align 4
	.type	Mod__uMod_var_u8_u64, #function
	.proc	05
Mod__uMod_var_u8_u64:
.LLFB390:
	save	%sp, -96, %sp
.LLCFI390:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE390:
	.size	Mod__uMod_var_u8_u64, .-Mod__uMod_var_u8_u64
	.align 4
	.type	Mod__Mod_var_u8_u64, #function
	.proc	05
Mod__Mod_var_u8_u64:
.LLFB391:
	save	%sp, -96, %sp
.LLCFI391:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL1992
	 nop
	cmp	%l0, 0
	bne	%icc, .LL1996
	 nop
	cmp	%l1, 0
.LL1996:
	cmp	%o4, 0
	bl	%icc, .LL1991
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1997
	 nop
	cmp	%o5, 0
.LL1997:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1995
	 nop
.LL1991:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL1994
	 nop
.LL1992:
	cmp	%o4, 0
	bg	%icc, .LL1993
	 nop
	cmp	%o4, 0
	bne	%icc, .LL1998
	 nop
	cmp	%o5, 0
	bne	%icc, .LL1993
	 nop
	cmp	%o5, 0
.LL1998:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL1995
	 nop
.LL1993:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1994:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL1995:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE391:
	.size	Mod__Mod_var_u8_u64, .-Mod__Mod_var_u8_u64
	.align 4
	.type	Mod__uMod_param_u8_u64, #function
	.proc	05
Mod__uMod_param_u8_u64:
.LLFB392:
	save	%sp, -104, %sp
.LLCFI392:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE392:
	.size	Mod__uMod_param_u8_u64, .-Mod__uMod_param_u8_u64
	.align 4
	.type	Mod__Mod_param_u8_u64, #function
	.proc	05
Mod__Mod_param_u8_u64:
.LLFB393:
	save	%sp, -104, %sp
.LLCFI393:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2002
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2006
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2006:
	cmp	%o4, 0
	bl	%icc, .LL2001
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2007
	 nop
	cmp	%o5, 0
.LL2007:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2005
	 nop
.LL2001:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2004
	 nop
.LL2002:
	cmp	%o4, 0
	bg	%icc, .LL2003
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2008
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2003
	 nop
	cmp	%o5, 0
.LL2008:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2005
	 nop
.LL2003:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2004:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2005:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE393:
	.size	Mod__Mod_param_u8_u64, .-Mod__Mod_param_u8_u64
	.align 4
	.type	Mod__uMod_var_u8_LC, #function
	.proc	05
Mod__uMod_var_u8_LC:
.LLFB394:
	save	%sp, -96, %sp
.LLCFI394:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE394:
	.size	Mod__uMod_var_u8_LC, .-Mod__uMod_var_u8_LC
	.align 4
	.type	Mod__Mod_var_u8_LC, #function
	.proc	05
Mod__Mod_var_u8_LC:
.LLFB395:
	save	%sp, -96, %sp
.LLCFI395:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2012
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2016
	 nop
	cmp	%l1, 0
.LL2016:
	cmp	%o4, 0
	bl	%icc, .LL2011
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2017
	 nop
	cmp	%o5, 0
.LL2017:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2015
	 nop
.LL2011:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2014
	 nop
.LL2012:
	cmp	%o4, 0
	bg	%icc, .LL2013
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2018
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2013
	 nop
	cmp	%o5, 0
.LL2018:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2015
	 nop
.LL2013:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2014:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2015:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE395:
	.size	Mod__Mod_var_u8_LC, .-Mod__Mod_var_u8_LC
	.align 4
	.type	Mod__uMod_param_u8_LC, #function
	.proc	05
Mod__uMod_param_u8_LC:
.LLFB396:
	save	%sp, -104, %sp
.LLCFI396:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE396:
	.size	Mod__uMod_param_u8_LC, .-Mod__uMod_param_u8_LC
	.align 4
	.type	Mod__Mod_param_u8_LC, #function
	.proc	05
Mod__Mod_param_u8_LC:
.LLFB397:
	save	%sp, -104, %sp
.LLCFI397:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %l2
	mov	%g1, %l3
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2022
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2026
	 nop
	cmp	%l1, 0
.LL2026:
	cmp	%o4, 0
	bl	%icc, .LL2021
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2027
	 nop
	cmp	%o5, 0
.LL2027:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2025
	 nop
.LL2021:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2024
	 nop
.LL2022:
	cmp	%o4, 0
	bg	%icc, .LL2023
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2028
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2023
	 nop
	cmp	%o5, 0
.LL2028:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2025
	 nop
.LL2023:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2024:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2025:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE397:
	.size	Mod__Mod_param_u8_LC, .-Mod__Mod_param_u8_LC
	.align 4
	.type	Mod__uMod_var_u8_i32, #function
	.proc	04
Mod__uMod_var_u8_i32:
.LLFB398:
	save	%sp, -96, %sp
.LLCFI398:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE398:
	.size	Mod__uMod_var_u8_i32, .-Mod__uMod_var_u8_i32
	.align 4
	.type	Mod__Mod_var_u8_i32, #function
	.proc	014
Mod__Mod_var_u8_i32:
.LLFB399:
	save	%sp, -96, %sp
.LLCFI399:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2032
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2031
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2035
	 nop
.LL2031:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2034
	 nop
.LL2032:
	cmp	%g1, 0
	bg	%icc, .LL2033
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2035
	 nop
.LL2033:
	add	%g1, -1, %g1
.LL2034:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2035:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2036
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2036:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2038
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2037
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2041
	 nop
.LL2037:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2040
	 nop
.LL2038:
	cmp	%g1, 0
	bg	%icc, .LL2039
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2041
	 nop
.LL2039:
	add	%g1, -1, %g1
.LL2040:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2041:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE399:
	.size	Mod__Mod_var_u8_i32, .-Mod__Mod_var_u8_i32
	.align 4
	.type	Mod__uMod_param_u8_i32, #function
	.proc	04
Mod__uMod_param_u8_i32:
.LLFB400:
	save	%sp, -96, %sp
.LLCFI400:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE400:
	.size	Mod__uMod_param_u8_i32, .-Mod__uMod_param_u8_i32
	.align 4
	.type	Mod__Mod_param_u8_i32, #function
	.proc	014
Mod__Mod_param_u8_i32:
.LLFB401:
	save	%sp, -96, %sp
.LLCFI401:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2045
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2044
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2048
	 nop
.LL2044:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2047
	 nop
.LL2045:
	cmp	%g1, 0
	bg	%icc, .LL2046
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2048
	 nop
.LL2046:
	add	%g1, -1, %g1
.LL2047:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2048:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2049
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2049:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2051
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2050
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2054
	 nop
.LL2050:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2053
	 nop
.LL2051:
	cmp	%g1, 0
	bg	%icc, .LL2052
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2054
	 nop
.LL2052:
	add	%g1, -1, %g1
.LL2053:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2054:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE401:
	.size	Mod__Mod_param_u8_i32, .-Mod__Mod_param_u8_i32
	.align 4
	.type	Mod__uMod_var_u8_i16, #function
	.proc	04
Mod__uMod_var_u8_i16:
.LLFB402:
	save	%sp, -96, %sp
.LLCFI402:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE402:
	.size	Mod__uMod_var_u8_i16, .-Mod__uMod_var_u8_i16
	.align 4
	.type	Mod__Mod_var_u8_i16, #function
	.proc	014
Mod__Mod_var_u8_i16:
.LLFB403:
	save	%sp, -96, %sp
.LLCFI403:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2058
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2057
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2061
	 nop
.LL2057:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2060
	 nop
.LL2058:
	cmp	%g1, 0
	bg	%icc, .LL2059
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2061
	 nop
.LL2059:
	add	%g1, -1, %g1
.LL2060:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2061:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2062
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL2062:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2064
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2063
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2067
	 nop
.LL2063:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2066
	 nop
.LL2064:
	cmp	%g1, 0
	bg	%icc, .LL2065
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2067
	 nop
.LL2065:
	add	%g1, -1, %g1
.LL2066:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2067:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE403:
	.size	Mod__Mod_var_u8_i16, .-Mod__Mod_var_u8_i16
	.align 4
	.type	Mod__uMod_param_u8_i16, #function
	.proc	04
Mod__uMod_param_u8_i16:
.LLFB404:
	save	%sp, -96, %sp
.LLCFI404:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE404:
	.size	Mod__uMod_param_u8_i16, .-Mod__uMod_param_u8_i16
	.align 4
	.type	Mod__Mod_param_u8_i16, #function
	.proc	014
Mod__Mod_param_u8_i16:
.LLFB405:
	save	%sp, -96, %sp
.LLCFI405:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2071
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2070
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2074
	 nop
.LL2070:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2073
	 nop
.LL2071:
	cmp	%g1, 0
	bg	%icc, .LL2072
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2074
	 nop
.LL2072:
	add	%g1, -1, %g1
.LL2073:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2074:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2075
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL2075:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2077
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2076
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2080
	 nop
.LL2076:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2079
	 nop
.LL2077:
	cmp	%g1, 0
	bg	%icc, .LL2078
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2080
	 nop
.LL2078:
	add	%g1, -1, %g1
.LL2079:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2080:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE405:
	.size	Mod__Mod_param_u8_i16, .-Mod__Mod_param_u8_i16
	.align 4
	.type	Mod__uMod_var_u8_I, #function
	.proc	04
Mod__uMod_var_u8_I:
.LLFB406:
	save	%sp, -96, %sp
.LLCFI406:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE406:
	.size	Mod__uMod_var_u8_I, .-Mod__uMod_var_u8_I
	.align 4
	.type	Mod__Mod_var_u8_I, #function
	.proc	014
Mod__Mod_var_u8_I:
.LLFB407:
	save	%sp, -96, %sp
.LLCFI407:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2084
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2083
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2087
	 nop
.LL2083:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2086
	 nop
.LL2084:
	cmp	%g1, 0
	bg	%icc, .LL2085
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2087
	 nop
.LL2085:
	add	%g1, -1, %g1
.LL2086:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2087:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2088
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2088:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2090
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2089
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2093
	 nop
.LL2089:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2092
	 nop
.LL2090:
	cmp	%g1, 0
	bg	%icc, .LL2091
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2093
	 nop
.LL2091:
	add	%g1, -1, %g1
.LL2092:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2093:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE407:
	.size	Mod__Mod_var_u8_I, .-Mod__Mod_var_u8_I
	.align 4
	.type	Mod__uMod_param_u8_I, #function
	.proc	04
Mod__uMod_param_u8_I:
.LLFB408:
	save	%sp, -96, %sp
.LLCFI408:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE408:
	.size	Mod__uMod_param_u8_I, .-Mod__uMod_param_u8_I
	.align 4
	.type	Mod__Mod_param_u8_I, #function
	.proc	014
Mod__Mod_param_u8_I:
.LLFB409:
	save	%sp, -96, %sp
.LLCFI409:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2097
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2096
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2100
	 nop
.LL2096:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2099
	 nop
.LL2097:
	cmp	%g1, 0
	bg	%icc, .LL2098
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2100
	 nop
.LL2098:
	add	%g1, -1, %g1
.LL2099:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2100:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2101
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL2101:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2103
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2102
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2106
	 nop
.LL2102:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2105
	 nop
.LL2103:
	cmp	%g1, 0
	bg	%icc, .LL2104
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2106
	 nop
.LL2104:
	add	%g1, -1, %g1
.LL2105:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2106:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE409:
	.size	Mod__Mod_param_u8_I, .-Mod__Mod_param_u8_I
	.align 4
	.type	Mod__uMod_var_u8_i64, #function
	.proc	05
Mod__uMod_var_u8_i64:
.LLFB410:
	save	%sp, -96, %sp
.LLCFI410:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE410:
	.size	Mod__uMod_var_u8_i64, .-Mod__uMod_var_u8_i64
	.align 4
	.type	Mod__Mod_var_u8_i64, #function
	.proc	05
Mod__Mod_var_u8_i64:
.LLFB411:
	save	%sp, -96, %sp
.LLCFI411:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2110
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2114
	 nop
	cmp	%l1, 0
.LL2114:
	cmp	%o4, 0
	bl	%icc, .LL2109
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2115
	 nop
	cmp	%o5, 0
.LL2115:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2113
	 nop
.LL2109:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2112
	 nop
.LL2110:
	cmp	%o4, 0
	bg	%icc, .LL2111
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2116
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2111
	 nop
	cmp	%o5, 0
.LL2116:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2113
	 nop
.LL2111:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2112:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2113:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE411:
	.size	Mod__Mod_var_u8_i64, .-Mod__Mod_var_u8_i64
	.align 4
	.type	Mod__uMod_param_u8_i64, #function
	.proc	05
Mod__uMod_param_u8_i64:
.LLFB412:
	save	%sp, -104, %sp
.LLCFI412:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE412:
	.size	Mod__uMod_param_u8_i64, .-Mod__uMod_param_u8_i64
	.align 4
	.type	Mod__Mod_param_u8_i64, #function
	.proc	05
Mod__Mod_param_u8_i64:
.LLFB413:
	save	%sp, -104, %sp
.LLCFI413:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2120
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2124
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2124:
	cmp	%o4, 0
	bl	%icc, .LL2119
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2125
	 nop
	cmp	%o5, 0
.LL2125:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2123
	 nop
.LL2119:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2122
	 nop
.LL2120:
	cmp	%o4, 0
	bg	%icc, .LL2121
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2126
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2121
	 nop
	cmp	%o5, 0
.LL2126:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2123
	 nop
.LL2121:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2122:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2123:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE413:
	.size	Mod__Mod_param_u8_i64, .-Mod__Mod_param_u8_i64
	.align 4
	.type	Mod__uMod_var_u8_C, #function
	.proc	04
Mod__uMod_var_u8_C:
.LLFB414:
	save	%sp, -96, %sp
.LLCFI414:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE414:
	.size	Mod__uMod_var_u8_C, .-Mod__uMod_var_u8_C
	.align 4
	.type	Mod__Mod_var_u8_C, #function
	.proc	014
Mod__Mod_var_u8_C:
.LLFB415:
	save	%sp, -96, %sp
.LLCFI415:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2130
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2129
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2133
	 nop
.LL2129:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2132
	 nop
.LL2130:
	cmp	%g1, 0
	bg	%icc, .LL2131
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2133
	 nop
.LL2131:
	add	%g1, -1, %g1
.LL2132:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2133:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2134
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2134:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2136
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2135
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2139
	 nop
.LL2135:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2138
	 nop
.LL2136:
	cmp	%g1, 0
	bg	%icc, .LL2137
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2139
	 nop
.LL2137:
	add	%g1, -1, %g1
.LL2138:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2139:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE415:
	.size	Mod__Mod_var_u8_C, .-Mod__Mod_var_u8_C
	.align 4
	.type	Mod__uMod_param_u8_C, #function
	.proc	04
Mod__uMod_param_u8_C:
.LLFB416:
	save	%sp, -96, %sp
.LLCFI416:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE416:
	.size	Mod__uMod_param_u8_C, .-Mod__uMod_param_u8_C
	.align 4
	.type	Mod__Mod_param_u8_C, #function
	.proc	014
Mod__Mod_param_u8_C:
.LLFB417:
	save	%sp, -96, %sp
.LLCFI417:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2143
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2142
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2146
	 nop
.LL2142:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2145
	 nop
.LL2143:
	cmp	%g1, 0
	bg	%icc, .LL2144
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2146
	 nop
.LL2144:
	add	%g1, -1, %g1
.LL2145:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2146:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2147
	 nop
	sethi	%hi(14336), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL2147:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2149
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2148
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2152
	 nop
.LL2148:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2151
	 nop
.LL2149:
	cmp	%g1, 0
	bg	%icc, .LL2150
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2152
	 nop
.LL2150:
	add	%g1, -1, %g1
.LL2151:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2152:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE417:
	.size	Mod__Mod_param_u8_C, .-Mod__Mod_param_u8_C
	.align 4
	.type	Mod__uMod_var_u8_u8, #function
	.proc	04
Mod__uMod_var_u8_u8:
.LLFB418:
	save	%sp, -96, %sp
.LLCFI418:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE418:
	.size	Mod__uMod_var_u8_u8, .-Mod__uMod_var_u8_u8
	.align 4
	.type	Mod__Mod_var_u8_u8, #function
	.proc	014
Mod__Mod_var_u8_u8:
.LLFB419:
	save	%sp, -96, %sp
.LLCFI419:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2156
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2155
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2159
	 nop
.LL2155:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2158
	 nop
.LL2156:
	cmp	%g1, 0
	bg	%icc, .LL2157
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2159
	 nop
.LL2157:
	add	%g1, -1, %g1
.LL2158:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2159:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE419:
	.size	Mod__Mod_var_u8_u8, .-Mod__Mod_var_u8_u8
	.align 4
	.type	Mod__uMod_param_u8_u8, #function
	.proc	04
Mod__uMod_param_u8_u8:
.LLFB420:
	save	%sp, -96, %sp
.LLCFI420:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE420:
	.size	Mod__uMod_param_u8_u8, .-Mod__uMod_param_u8_u8
	.align 4
	.type	Mod__Mod_param_u8_u8, #function
	.proc	014
Mod__Mod_param_u8_u8:
.LLFB421:
	save	%sp, -96, %sp
.LLCFI421:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2163
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2162
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2166
	 nop
.LL2162:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2165
	 nop
.LL2163:
	cmp	%g1, 0
	bg	%icc, .LL2164
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2166
	 nop
.LL2164:
	add	%g1, -1, %g1
.LL2165:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2166:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE421:
	.size	Mod__Mod_param_u8_u8, .-Mod__Mod_param_u8_u8
	.align 4
	.type	Mod__uMod_var_u8_L, #function
	.proc	05
Mod__uMod_var_u8_L:
.LLFB422:
	save	%sp, -96, %sp
.LLCFI422:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE422:
	.size	Mod__uMod_var_u8_L, .-Mod__uMod_var_u8_L
	.align 4
	.type	Mod__Mod_var_u8_L, #function
	.proc	05
Mod__Mod_var_u8_L:
.LLFB423:
	save	%sp, -96, %sp
.LLCFI423:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l2
	mov	%g1, %l3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2170
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2174
	 nop
	cmp	%l1, 0
.LL2174:
	cmp	%o4, 0
	bl	%icc, .LL2169
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2175
	 nop
	cmp	%o5, 0
.LL2175:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2173
	 nop
.LL2169:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2172
	 nop
.LL2170:
	cmp	%o4, 0
	bg	%icc, .LL2171
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2176
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2171
	 nop
	cmp	%o5, 0
.LL2176:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2173
	 nop
.LL2171:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2172:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2173:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE423:
	.size	Mod__Mod_var_u8_L, .-Mod__Mod_var_u8_L
	.align 4
	.type	Mod__uMod_param_u8_L, #function
	.proc	05
Mod__uMod_param_u8_L:
.LLFB424:
	save	%sp, -104, %sp
.LLCFI424:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %o2
	mov	%g1, %o3
	ldd	[%fp-8], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE424:
	.size	Mod__uMod_param_u8_L, .-Mod__uMod_param_u8_L
	.align 4
	.type	Mod__Mod_param_u8_L, #function
	.proc	05
Mod__Mod_param_u8_L:
.LLFB425:
	save	%sp, -104, %sp
.LLCFI425:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2180
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2184
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2184:
	cmp	%o4, 0
	bl	%icc, .LL2179
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2185
	 nop
	cmp	%o5, 0
.LL2185:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2183
	 nop
.LL2179:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2182
	 nop
.LL2180:
	cmp	%o4, 0
	bg	%icc, .LL2181
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2186
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2181
	 nop
	cmp	%o5, 0
.LL2186:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2183
	 nop
.LL2181:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2182:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2183:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE425:
	.size	Mod__Mod_param_u8_L, .-Mod__Mod_param_u8_L
	.align 4
	.type	Mod__uMod_var_u8_i8, #function
	.proc	04
Mod__uMod_var_u8_i8:
.LLFB426:
	save	%sp, -96, %sp
.LLCFI426:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE426:
	.size	Mod__uMod_var_u8_i8, .-Mod__uMod_var_u8_i8
	.align 4
	.type	Mod__Mod_var_u8_i8, #function
	.proc	014
Mod__Mod_var_u8_i8:
.LLFB427:
	save	%sp, -96, %sp
.LLCFI427:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2190
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2189
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2193
	 nop
.LL2189:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2192
	 nop
.LL2190:
	cmp	%g1, 0
	bg	%icc, .LL2191
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2193
	 nop
.LL2191:
	add	%g1, -1, %g1
.LL2192:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2193:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2194
	 nop
	sethi	%hi(15360), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL2194:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2196
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2195
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2199
	 nop
.LL2195:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2198
	 nop
.LL2196:
	cmp	%g1, 0
	bg	%icc, .LL2197
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2199
	 nop
.LL2197:
	add	%g1, -1, %g1
.LL2198:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2199:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE427:
	.size	Mod__Mod_var_u8_i8, .-Mod__Mod_var_u8_i8
	.align 4
	.type	Mod__uMod_param_u8_i8, #function
	.proc	04
Mod__uMod_param_u8_i8:
.LLFB428:
	save	%sp, -96, %sp
.LLCFI428:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE428:
	.size	Mod__uMod_param_u8_i8, .-Mod__uMod_param_u8_i8
	.align 4
	.type	Mod__Mod_param_u8_i8, #function
	.proc	014
Mod__Mod_param_u8_i8:
.LLFB429:
	save	%sp, -96, %sp
.LLCFI429:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2203
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2202
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2206
	 nop
.LL2202:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2205
	 nop
.LL2203:
	cmp	%g1, 0
	bg	%icc, .LL2204
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2206
	 nop
.LL2204:
	add	%g1, -1, %g1
.LL2205:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2206:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2207
	 nop
	sethi	%hi(15360), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL2207:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2209
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2208
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2212
	 nop
.LL2208:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2211
	 nop
.LL2209:
	cmp	%g1, 0
	bg	%icc, .LL2210
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2212
	 nop
.LL2210:
	add	%g1, -1, %g1
.LL2211:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2212:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE429:
	.size	Mod__Mod_param_u8_i8, .-Mod__Mod_param_u8_i8
	.align 4
	.type	Mod__uMod_var_u8_u32, #function
	.proc	04
Mod__uMod_var_u8_u32:
.LLFB430:
	save	%sp, -96, %sp
.LLCFI430:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE430:
	.size	Mod__uMod_var_u8_u32, .-Mod__uMod_var_u8_u32
	.align 4
	.type	Mod__Mod_var_u8_u32, #function
	.proc	014
Mod__Mod_var_u8_u32:
.LLFB431:
	save	%sp, -96, %sp
.LLCFI431:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2216
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2215
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2219
	 nop
.LL2215:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2218
	 nop
.LL2216:
	cmp	%g1, 0
	bg	%icc, .LL2217
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2219
	 nop
.LL2217:
	add	%g1, -1, %g1
.LL2218:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2219:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2220
	 nop
	sethi	%hi(15360), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2220:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2222
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2221
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2225
	 nop
.LL2221:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2224
	 nop
.LL2222:
	cmp	%g1, 0
	bg	%icc, .LL2223
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2225
	 nop
.LL2223:
	add	%g1, -1, %g1
.LL2224:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2225:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE431:
	.size	Mod__Mod_var_u8_u32, .-Mod__Mod_var_u8_u32
	.align 4
	.type	Mod__uMod_param_u8_u32, #function
	.proc	04
Mod__uMod_param_u8_u32:
.LLFB432:
	save	%sp, -96, %sp
.LLCFI432:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE432:
	.size	Mod__uMod_param_u8_u32, .-Mod__uMod_param_u8_u32
	.align 4
	.type	Mod__Mod_param_u8_u32, #function
	.proc	014
Mod__Mod_param_u8_u32:
.LLFB433:
	save	%sp, -96, %sp
.LLCFI433:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2229
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2228
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2232
	 nop
.LL2228:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2231
	 nop
.LL2229:
	cmp	%g1, 0
	bg	%icc, .LL2230
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2232
	 nop
.LL2230:
	add	%g1, -1, %g1
.LL2231:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2232:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 255
	ble	%icc, .LL2233
	 nop
	sethi	%hi(15360), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2233:
	ldub	[%fp+68], %g1
	and	%g1, 0xff, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2235
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2234
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2238
	 nop
.LL2234:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2237
	 nop
.LL2235:
	cmp	%g1, 0
	bg	%icc, .LL2236
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2238
	 nop
.LL2236:
	add	%g1, -1, %g1
.LL2237:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2238:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE433:
	.size	Mod__Mod_param_u8_u32, .-Mod__Mod_param_u8_u32
	.align 4
	.type	Mod__uMod_var_L_u16, #function
	.proc	05
Mod__uMod_var_L_u16:
.LLFB434:
	save	%sp, -96, %sp
.LLCFI434:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE434:
	.size	Mod__uMod_var_L_u16, .-Mod__uMod_var_L_u16
	.align 4
	.type	Mod__Mod_var_L_u16, #function
	.proc	05
Mod__Mod_var_L_u16:
.LLFB435:
	save	%sp, -96, %sp
.LLCFI435:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2242
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2246
	 nop
	cmp	%l1, 0
.LL2246:
	cmp	%o4, 0
	bl	%icc, .LL2241
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2247
	 nop
	cmp	%o5, 0
.LL2247:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2245
	 nop
.LL2241:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2244
	 nop
.LL2242:
	cmp	%o4, 0
	bg	%icc, .LL2243
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2248
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2243
	 nop
	cmp	%o5, 0
.LL2248:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2245
	 nop
.LL2243:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2244:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2245:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE435:
	.size	Mod__Mod_var_L_u16, .-Mod__Mod_var_L_u16
	.align 4
	.type	Mod__uMod_param_L_u16, #function
	.proc	05
Mod__uMod_param_L_u16:
.LLFB436:
	save	%sp, -104, %sp
.LLCFI436:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE436:
	.size	Mod__uMod_param_L_u16, .-Mod__uMod_param_L_u16
	.align 4
	.type	Mod__Mod_param_L_u16, #function
	.proc	05
Mod__Mod_param_L_u16:
.LLFB437:
	save	%sp, -104, %sp
.LLCFI437:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %g1
	mov	0, %l0
	mov	%g1, %l1
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2252
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2256
	 nop
	cmp	%l1, 0
.LL2256:
	cmp	%o4, 0
	bl	%icc, .LL2251
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2257
	 nop
	cmp	%o5, 0
.LL2257:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2255
	 nop
.LL2251:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2254
	 nop
.LL2252:
	cmp	%o4, 0
	bg	%icc, .LL2253
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2258
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2253
	 nop
	cmp	%o5, 0
.LL2258:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2255
	 nop
.LL2253:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2254:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2255:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE437:
	.size	Mod__Mod_param_L_u16, .-Mod__Mod_param_L_u16
	.align 4
	.type	Mod__uMod_var_L_u64, #function
	.proc	05
Mod__uMod_var_L_u64:
.LLFB438:
	save	%sp, -96, %sp
.LLCFI438:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE438:
	.size	Mod__uMod_var_L_u64, .-Mod__uMod_var_L_u64
	.align 4
	.type	Mod__Mod_var_L_u64, #function
	.proc	05
Mod__Mod_var_L_u64:
.LLFB439:
	save	%sp, -96, %sp
.LLCFI439:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2262
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2266
	 nop
	cmp	%l1, 0
.LL2266:
	cmp	%o4, 0
	bl	%icc, .LL2261
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2267
	 nop
	cmp	%o5, 0
.LL2267:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2265
	 nop
.LL2261:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2264
	 nop
.LL2262:
	cmp	%o4, 0
	bg	%icc, .LL2263
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2268
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2263
	 nop
	cmp	%o5, 0
.LL2268:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2265
	 nop
.LL2263:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2264:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2265:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE439:
	.size	Mod__Mod_var_L_u64, .-Mod__Mod_var_L_u64
	.align 4
	.type	Mod__uMod_param_L_u64, #function
	.proc	05
Mod__uMod_param_L_u64:
.LLFB440:
	save	%sp, -112, %sp
.LLCFI440:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE440:
	.size	Mod__uMod_param_L_u64, .-Mod__uMod_param_L_u64
	.align 4
	.type	Mod__Mod_param_L_u64, #function
	.proc	05
Mod__Mod_param_L_u64:
.LLFB441:
	save	%sp, -112, %sp
.LLCFI441:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL2272
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL2276
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL2276:
	cmp	%o4, 0
	bl	%icc, .LL2271
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2277
	 nop
	cmp	%o5, 0
.LL2277:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2275
	 nop
.LL2271:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2274
	 nop
.LL2272:
	cmp	%o4, 0
	bg	%icc, .LL2273
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2278
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2273
	 nop
	cmp	%o5, 0
.LL2278:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2275
	 nop
.LL2273:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2274:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2275:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE441:
	.size	Mod__Mod_param_L_u64, .-Mod__Mod_param_L_u64
	.align 4
	.type	Mod__uMod_var_L_LC, #function
	.proc	05
Mod__uMod_var_L_LC:
.LLFB442:
	save	%sp, -96, %sp
.LLCFI442:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE442:
	.size	Mod__uMod_var_L_LC, .-Mod__uMod_var_L_LC
	.align 4
	.type	Mod__Mod_var_L_LC, #function
	.proc	05
Mod__Mod_var_L_LC:
.LLFB443:
	save	%sp, -96, %sp
.LLCFI443:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2282
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2286
	 nop
	cmp	%l1, 0
.LL2286:
	cmp	%o4, 0
	bl	%icc, .LL2281
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2287
	 nop
	cmp	%o5, 0
.LL2287:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2285
	 nop
.LL2281:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2284
	 nop
.LL2282:
	cmp	%o4, 0
	bg	%icc, .LL2283
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2288
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2283
	 nop
	cmp	%o5, 0
.LL2288:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2285
	 nop
.LL2283:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2284:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2285:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE443:
	.size	Mod__Mod_var_L_LC, .-Mod__Mod_var_L_LC
	.align 4
	.type	Mod__uMod_param_L_LC, #function
	.proc	05
Mod__uMod_param_L_LC:
.LLFB444:
	save	%sp, -112, %sp
.LLCFI444:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE444:
	.size	Mod__uMod_param_L_LC, .-Mod__uMod_param_L_LC
	.align 4
	.type	Mod__Mod_param_L_LC, #function
	.proc	05
Mod__Mod_param_L_LC:
.LLFB445:
	save	%sp, -112, %sp
.LLCFI445:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-16], %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2292
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2296
	 nop
	cmp	%l1, 0
.LL2296:
	cmp	%o4, 0
	bl	%icc, .LL2291
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2297
	 nop
	cmp	%o5, 0
.LL2297:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2295
	 nop
.LL2291:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2294
	 nop
.LL2292:
	cmp	%o4, 0
	bg	%icc, .LL2293
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2298
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2293
	 nop
	cmp	%o5, 0
.LL2298:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2295
	 nop
.LL2293:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2294:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2295:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE445:
	.size	Mod__Mod_param_L_LC, .-Mod__Mod_param_L_LC
	.align 4
	.type	Mod__uMod_var_L_i32, #function
	.proc	05
Mod__uMod_var_L_i32:
.LLFB446:
	save	%sp, -96, %sp
.LLCFI446:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE446:
	.size	Mod__uMod_var_L_i32, .-Mod__uMod_var_L_i32
	.align 4
	.type	Mod__Mod_var_L_i32, #function
	.proc	05
Mod__Mod_var_L_i32:
.LLFB447:
	save	%sp, -96, %sp
.LLCFI447:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2302
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2306
	 nop
	cmp	%l1, 0
.LL2306:
	cmp	%o4, 0
	bl	%icc, .LL2301
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2307
	 nop
	cmp	%o5, 0
.LL2307:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2305
	 nop
.LL2301:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2304
	 nop
.LL2302:
	cmp	%o4, 0
	bg	%icc, .LL2303
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2308
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2303
	 nop
	cmp	%o5, 0
.LL2308:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2305
	 nop
.LL2303:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2304:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2305:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE447:
	.size	Mod__Mod_var_L_i32, .-Mod__Mod_var_L_i32
	.align 4
	.type	Mod__uMod_param_L_i32, #function
	.proc	05
Mod__uMod_param_L_i32:
.LLFB448:
	save	%sp, -104, %sp
.LLCFI448:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE448:
	.size	Mod__uMod_param_L_i32, .-Mod__uMod_param_L_i32
	.align 4
	.type	Mod__Mod_param_L_i32, #function
	.proc	05
Mod__Mod_param_L_i32:
.LLFB449:
	save	%sp, -104, %sp
.LLCFI449:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2312
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2316
	 nop
	cmp	%l1, 0
.LL2316:
	cmp	%o4, 0
	bl	%icc, .LL2311
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2317
	 nop
	cmp	%o5, 0
.LL2317:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2315
	 nop
.LL2311:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2314
	 nop
.LL2312:
	cmp	%o4, 0
	bg	%icc, .LL2313
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2318
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2313
	 nop
	cmp	%o5, 0
.LL2318:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2315
	 nop
.LL2313:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2314:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2315:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE449:
	.size	Mod__Mod_param_L_i32, .-Mod__Mod_param_L_i32
	.align 4
	.type	Mod__uMod_var_L_i16, #function
	.proc	05
Mod__uMod_var_L_i16:
.LLFB450:
	save	%sp, -96, %sp
.LLCFI450:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE450:
	.size	Mod__uMod_var_L_i16, .-Mod__uMod_var_L_i16
	.align 4
	.type	Mod__Mod_var_L_i16, #function
	.proc	05
Mod__Mod_var_L_i16:
.LLFB451:
	save	%sp, -96, %sp
.LLCFI451:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2322
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2326
	 nop
	cmp	%l1, 0
.LL2326:
	cmp	%o4, 0
	bl	%icc, .LL2321
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2327
	 nop
	cmp	%o5, 0
.LL2327:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2325
	 nop
.LL2321:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2324
	 nop
.LL2322:
	cmp	%o4, 0
	bg	%icc, .LL2323
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2328
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2323
	 nop
	cmp	%o5, 0
.LL2328:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2325
	 nop
.LL2323:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2324:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2325:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE451:
	.size	Mod__Mod_var_L_i16, .-Mod__Mod_var_L_i16
	.align 4
	.type	Mod__uMod_param_L_i16, #function
	.proc	05
Mod__uMod_param_L_i16:
.LLFB452:
	save	%sp, -104, %sp
.LLCFI452:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE452:
	.size	Mod__uMod_param_L_i16, .-Mod__uMod_param_L_i16
	.align 4
	.type	Mod__Mod_param_L_i16, #function
	.proc	05
Mod__Mod_param_L_i16:
.LLFB453:
	save	%sp, -104, %sp
.LLCFI453:
	std	%i0, [%fp-8]
	sth	%i2, [%fp+76]
	lduh	[%fp+76], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2332
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2336
	 nop
	cmp	%l1, 0
.LL2336:
	cmp	%o4, 0
	bl	%icc, .LL2331
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2337
	 nop
	cmp	%o5, 0
.LL2337:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2335
	 nop
.LL2331:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2334
	 nop
.LL2332:
	cmp	%o4, 0
	bg	%icc, .LL2333
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2338
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2333
	 nop
	cmp	%o5, 0
.LL2338:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2335
	 nop
.LL2333:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2334:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2335:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE453:
	.size	Mod__Mod_param_L_i16, .-Mod__Mod_param_L_i16
	.align 4
	.type	Mod__uMod_var_L_I, #function
	.proc	05
Mod__uMod_var_L_I:
.LLFB454:
	save	%sp, -96, %sp
.LLCFI454:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE454:
	.size	Mod__uMod_var_L_I, .-Mod__uMod_var_L_I
	.align 4
	.type	Mod__Mod_var_L_I, #function
	.proc	05
Mod__Mod_var_L_I:
.LLFB455:
	save	%sp, -96, %sp
.LLCFI455:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2342
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2346
	 nop
	cmp	%l1, 0
.LL2346:
	cmp	%o4, 0
	bl	%icc, .LL2341
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2347
	 nop
	cmp	%o5, 0
.LL2347:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2345
	 nop
.LL2341:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2344
	 nop
.LL2342:
	cmp	%o4, 0
	bg	%icc, .LL2343
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2348
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2343
	 nop
	cmp	%o5, 0
.LL2348:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2345
	 nop
.LL2343:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2344:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2345:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE455:
	.size	Mod__Mod_var_L_I, .-Mod__Mod_var_L_I
	.align 4
	.type	Mod__uMod_param_L_I, #function
	.proc	05
Mod__uMod_param_L_I:
.LLFB456:
	save	%sp, -104, %sp
.LLCFI456:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE456:
	.size	Mod__uMod_param_L_I, .-Mod__uMod_param_L_I
	.align 4
	.type	Mod__Mod_param_L_I, #function
	.proc	05
Mod__Mod_param_L_I:
.LLFB457:
	save	%sp, -104, %sp
.LLCFI457:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2352
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2356
	 nop
	cmp	%l1, 0
.LL2356:
	cmp	%o4, 0
	bl	%icc, .LL2351
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2357
	 nop
	cmp	%o5, 0
.LL2357:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2355
	 nop
.LL2351:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2354
	 nop
.LL2352:
	cmp	%o4, 0
	bg	%icc, .LL2353
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2358
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2353
	 nop
	cmp	%o5, 0
.LL2358:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2355
	 nop
.LL2353:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2354:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2355:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE457:
	.size	Mod__Mod_param_L_I, .-Mod__Mod_param_L_I
	.align 4
	.type	Mod__uMod_var_L_i64, #function
	.proc	05
Mod__uMod_var_L_i64:
.LLFB458:
	save	%sp, -96, %sp
.LLCFI458:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE458:
	.size	Mod__uMod_var_L_i64, .-Mod__uMod_var_L_i64
	.align 4
	.type	Mod__Mod_var_L_i64, #function
	.proc	05
Mod__Mod_var_L_i64:
.LLFB459:
	save	%sp, -96, %sp
.LLCFI459:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2362
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2366
	 nop
	cmp	%l1, 0
.LL2366:
	cmp	%o4, 0
	bl	%icc, .LL2361
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2367
	 nop
	cmp	%o5, 0
.LL2367:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2365
	 nop
.LL2361:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2364
	 nop
.LL2362:
	cmp	%o4, 0
	bg	%icc, .LL2363
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2368
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2363
	 nop
	cmp	%o5, 0
.LL2368:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2365
	 nop
.LL2363:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2364:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2365:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE459:
	.size	Mod__Mod_var_L_i64, .-Mod__Mod_var_L_i64
	.align 4
	.type	Mod__uMod_param_L_i64, #function
	.proc	05
Mod__uMod_param_L_i64:
.LLFB460:
	save	%sp, -112, %sp
.LLCFI460:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE460:
	.size	Mod__uMod_param_L_i64, .-Mod__uMod_param_L_i64
	.align 4
	.type	Mod__Mod_param_L_i64, #function
	.proc	05
Mod__Mod_param_L_i64:
.LLFB461:
	save	%sp, -112, %sp
.LLCFI461:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL2372
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL2376
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL2376:
	cmp	%o4, 0
	bl	%icc, .LL2371
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2377
	 nop
	cmp	%o5, 0
.LL2377:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2375
	 nop
.LL2371:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2374
	 nop
.LL2372:
	cmp	%o4, 0
	bg	%icc, .LL2373
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2378
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2373
	 nop
	cmp	%o5, 0
.LL2378:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2375
	 nop
.LL2373:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2374:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2375:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE461:
	.size	Mod__Mod_param_L_i64, .-Mod__Mod_param_L_i64
	.align 4
	.type	Mod__uMod_var_L_C, #function
	.proc	05
Mod__uMod_var_L_C:
.LLFB462:
	save	%sp, -96, %sp
.LLCFI462:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE462:
	.size	Mod__uMod_var_L_C, .-Mod__uMod_var_L_C
	.align 4
	.type	Mod__Mod_var_L_C, #function
	.proc	05
Mod__Mod_var_L_C:
.LLFB463:
	save	%sp, -96, %sp
.LLCFI463:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2382
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2386
	 nop
	cmp	%l1, 0
.LL2386:
	cmp	%o4, 0
	bl	%icc, .LL2381
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2387
	 nop
	cmp	%o5, 0
.LL2387:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2385
	 nop
.LL2381:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2384
	 nop
.LL2382:
	cmp	%o4, 0
	bg	%icc, .LL2383
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2388
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2383
	 nop
	cmp	%o5, 0
.LL2388:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2385
	 nop
.LL2383:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2384:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2385:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE463:
	.size	Mod__Mod_var_L_C, .-Mod__Mod_var_L_C
	.align 4
	.type	Mod__uMod_param_L_C, #function
	.proc	05
Mod__uMod_param_L_C:
.LLFB464:
	save	%sp, -104, %sp
.LLCFI464:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE464:
	.size	Mod__uMod_param_L_C, .-Mod__uMod_param_L_C
	.align 4
	.type	Mod__Mod_param_L_C, #function
	.proc	05
Mod__Mod_param_L_C:
.LLFB465:
	save	%sp, -104, %sp
.LLCFI465:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2392
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2396
	 nop
	cmp	%l1, 0
.LL2396:
	cmp	%o4, 0
	bl	%icc, .LL2391
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2397
	 nop
	cmp	%o5, 0
.LL2397:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2395
	 nop
.LL2391:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2394
	 nop
.LL2392:
	cmp	%o4, 0
	bg	%icc, .LL2393
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2398
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2393
	 nop
	cmp	%o5, 0
.LL2398:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2395
	 nop
.LL2393:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2394:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2395:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE465:
	.size	Mod__Mod_param_L_C, .-Mod__Mod_param_L_C
	.align 4
	.type	Mod__uMod_var_L_u8, #function
	.proc	05
Mod__uMod_var_L_u8:
.LLFB466:
	save	%sp, -96, %sp
.LLCFI466:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE466:
	.size	Mod__uMod_var_L_u8, .-Mod__uMod_var_L_u8
	.align 4
	.type	Mod__Mod_var_L_u8, #function
	.proc	05
Mod__Mod_var_L_u8:
.LLFB467:
	save	%sp, -96, %sp
.LLCFI467:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2402
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2406
	 nop
	cmp	%l1, 0
.LL2406:
	cmp	%o4, 0
	bl	%icc, .LL2401
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2407
	 nop
	cmp	%o5, 0
.LL2407:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2405
	 nop
.LL2401:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2404
	 nop
.LL2402:
	cmp	%o4, 0
	bg	%icc, .LL2403
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2408
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2403
	 nop
	cmp	%o5, 0
.LL2408:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2405
	 nop
.LL2403:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2404:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2405:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE467:
	.size	Mod__Mod_var_L_u8, .-Mod__Mod_var_L_u8
	.align 4
	.type	Mod__uMod_param_L_u8, #function
	.proc	05
Mod__uMod_param_L_u8:
.LLFB468:
	save	%sp, -104, %sp
.LLCFI468:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %o4
	mov	%g1, %o5
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE468:
	.size	Mod__uMod_param_L_u8, .-Mod__uMod_param_L_u8
	.align 4
	.type	Mod__Mod_param_L_u8, #function
	.proc	05
Mod__Mod_param_L_u8:
.LLFB469:
	save	%sp, -104, %sp
.LLCFI469:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	and	%g1, 0xff, %g1
	mov	0, %l0
	mov	%g1, %l1
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2412
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2416
	 nop
	cmp	%l1, 0
.LL2416:
	cmp	%o4, 0
	bl	%icc, .LL2411
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2417
	 nop
	cmp	%o5, 0
.LL2417:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2415
	 nop
.LL2411:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2414
	 nop
.LL2412:
	cmp	%o4, 0
	bg	%icc, .LL2413
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2418
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2413
	 nop
	cmp	%o5, 0
.LL2418:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2415
	 nop
.LL2413:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2414:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2415:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE469:
	.size	Mod__Mod_param_L_u8, .-Mod__Mod_param_L_u8
	.align 4
	.type	Mod__uMod_var_L_L, #function
	.proc	05
Mod__uMod_var_L_L:
.LLFB470:
	save	%sp, -96, %sp
.LLCFI470:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o4, %o2
	mov	%o5, %o3
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE470:
	.size	Mod__uMod_var_L_L, .-Mod__uMod_var_L_L
	.align 4
	.type	Mod__Mod_var_L_L, #function
	.proc	05
Mod__Mod_var_L_L:
.LLFB471:
	save	%sp, -96, %sp
.LLCFI471:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2422
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2426
	 nop
	cmp	%l1, 0
.LL2426:
	cmp	%o4, 0
	bl	%icc, .LL2421
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2427
	 nop
	cmp	%o5, 0
.LL2427:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2425
	 nop
.LL2421:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2424
	 nop
.LL2422:
	cmp	%o4, 0
	bg	%icc, .LL2423
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2428
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2423
	 nop
	cmp	%o5, 0
.LL2428:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2425
	 nop
.LL2423:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2424:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2425:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE471:
	.size	Mod__Mod_var_L_L, .-Mod__Mod_var_L_L
	.align 4
	.type	Mod__uMod_param_L_L, #function
	.proc	05
Mod__uMod_param_L_L:
.LLFB472:
	save	%sp, -112, %sp
.LLCFI472:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %o2
	ldd	[%fp-16], %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE472:
	.size	Mod__uMod_param_L_L, .-Mod__uMod_param_L_L
	.align 4
	.type	Mod__Mod_param_L_L, #function
	.proc	05
Mod__Mod_param_L_L:
.LLFB473:
	save	%sp, -112, %sp
.LLCFI473:
	std	%i0, [%fp-8]
	std	%i2, [%fp-16]
	ldd	[%fp-8], %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bl	%icc, .LL2432
	 nop
	ld	[%fp-16], %g1
	cmp	%g1, 0
	bne	%icc, .LL2436
	 nop
	ld	[%fp-12], %g1
	cmp	%g1, 0
.LL2436:
	cmp	%o4, 0
	bl	%icc, .LL2431
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2437
	 nop
	cmp	%o5, 0
.LL2437:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2435
	 nop
.LL2431:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2434
	 nop
.LL2432:
	cmp	%o4, 0
	bg	%icc, .LL2433
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2438
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2433
	 nop
	cmp	%o5, 0
.LL2438:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2435
	 nop
.LL2433:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2434:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-16], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2435:
	ldd	[%fp-16], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE473:
	.size	Mod__Mod_param_L_L, .-Mod__Mod_param_L_L
	.align 4
	.type	Mod__uMod_var_L_i8, #function
	.proc	05
Mod__uMod_var_L_i8:
.LLFB474:
	save	%sp, -96, %sp
.LLCFI474:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE474:
	.size	Mod__uMod_var_L_i8, .-Mod__uMod_var_L_i8
	.align 4
	.type	Mod__Mod_var_L_i8, #function
	.proc	05
Mod__Mod_var_L_i8:
.LLFB475:
	save	%sp, -96, %sp
.LLCFI475:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2442
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2446
	 nop
	cmp	%l1, 0
.LL2446:
	cmp	%o4, 0
	bl	%icc, .LL2441
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2447
	 nop
	cmp	%o5, 0
.LL2447:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2445
	 nop
.LL2441:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2444
	 nop
.LL2442:
	cmp	%o4, 0
	bg	%icc, .LL2443
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2448
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2443
	 nop
	cmp	%o5, 0
.LL2448:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2445
	 nop
.LL2443:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2444:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2445:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE475:
	.size	Mod__Mod_var_L_i8, .-Mod__Mod_var_L_i8
	.align 4
	.type	Mod__uMod_param_L_i8, #function
	.proc	05
Mod__uMod_param_L_i8:
.LLFB476:
	save	%sp, -104, %sp
.LLCFI476:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE476:
	.size	Mod__uMod_param_L_i8, .-Mod__uMod_param_L_i8
	.align 4
	.type	Mod__Mod_param_L_i8, #function
	.proc	05
Mod__Mod_param_L_i8:
.LLFB477:
	save	%sp, -104, %sp
.LLCFI477:
	std	%i0, [%fp-8]
	stb	%i2, [%fp+76]
	ldub	[%fp+76], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2452
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2456
	 nop
	cmp	%l1, 0
.LL2456:
	cmp	%o4, 0
	bl	%icc, .LL2451
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2457
	 nop
	cmp	%o5, 0
.LL2457:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2455
	 nop
.LL2451:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2454
	 nop
.LL2452:
	cmp	%o4, 0
	bg	%icc, .LL2453
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2458
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2453
	 nop
	cmp	%o5, 0
.LL2458:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2455
	 nop
.LL2453:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2454:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2455:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE477:
	.size	Mod__Mod_param_L_i8, .-Mod__Mod_param_L_i8
	.align 4
	.type	Mod__uMod_var_L_u32, #function
	.proc	05
Mod__uMod_var_L_u32:
.LLFB478:
	save	%sp, -96, %sp
.LLCFI478:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE478:
	.size	Mod__uMod_var_L_u32, .-Mod__uMod_var_L_u32
	.align 4
	.type	Mod__Mod_var_L_u32, #function
	.proc	05
Mod__Mod_var_L_u32:
.LLFB479:
	save	%sp, -96, %sp
.LLCFI479:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2462
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2466
	 nop
	cmp	%l1, 0
.LL2466:
	cmp	%o4, 0
	bl	%icc, .LL2461
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2467
	 nop
	cmp	%o5, 0
.LL2467:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2465
	 nop
.LL2461:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2464
	 nop
.LL2462:
	cmp	%o4, 0
	bg	%icc, .LL2463
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2468
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2463
	 nop
	cmp	%o5, 0
.LL2468:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2465
	 nop
.LL2463:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2464:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2465:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE479:
	.size	Mod__Mod_var_L_u32, .-Mod__Mod_var_L_u32
	.align 4
	.type	Mod__uMod_param_L_u32, #function
	.proc	05
Mod__uMod_param_L_u32:
.LLFB480:
	save	%sp, -104, %sp
.LLCFI480:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ldd	[%fp-8], %o2
	ld	[%fp+76], %o5
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o2, %o0
	mov	%o3, %o1
	mov	%o4, %o2
	mov	%o5, %o3
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE480:
	.size	Mod__uMod_param_L_u32, .-Mod__uMod_param_L_u32
	.align 4
	.type	Mod__Mod_param_L_u32, #function
	.proc	05
Mod__Mod_param_L_u32:
.LLFB481:
	save	%sp, -104, %sp
.LLCFI481:
	std	%i0, [%fp-8]
	st	%i2, [%fp+76]
	ld	[%fp+76], %l1
	ld	[%fp+76], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	ldd	[%fp-8], %l2
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2472
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2476
	 nop
	cmp	%l1, 0
.LL2476:
	cmp	%o4, 0
	bl	%icc, .LL2471
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2477
	 nop
	cmp	%o5, 0
.LL2477:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2475
	 nop
.LL2471:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2474
	 nop
.LL2472:
	cmp	%o4, 0
	bg	%icc, .LL2473
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2478
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2473
	 nop
	cmp	%o5, 0
.LL2478:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2475
	 nop
.LL2473:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2474:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2475:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE481:
	.size	Mod__Mod_param_L_u32, .-Mod__Mod_param_L_u32
	.align 4
	.type	Mod__uMod_var_i8_u16, #function
	.proc	04
Mod__uMod_var_i8_u16:
.LLFB482:
	save	%sp, -96, %sp
.LLCFI482:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE482:
	.size	Mod__uMod_var_i8_u16, .-Mod__uMod_var_i8_u16
	.align 4
	.type	Mod__Mod_var_i8_u16, #function
	.proc	02
Mod__Mod_var_i8_u16:
.LLFB483:
	save	%sp, -96, %sp
.LLCFI483:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2482
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2481
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2485
	 nop
.LL2481:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2484
	 nop
.LL2482:
	cmp	%g1, 0
	bg	%icc, .LL2483
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2485
	 nop
.LL2483:
	add	%g1, -1, %g1
.LL2484:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2485:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2486
	 nop
	sethi	%hi(16384), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL2486:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2488
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2487
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2491
	 nop
.LL2487:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2490
	 nop
.LL2488:
	cmp	%g1, 0
	bg	%icc, .LL2489
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2491
	 nop
.LL2489:
	add	%g1, -1, %g1
.LL2490:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2491:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE483:
	.size	Mod__Mod_var_i8_u16, .-Mod__Mod_var_i8_u16
	.align 4
	.type	Mod__uMod_param_i8_u16, #function
	.proc	04
Mod__uMod_param_i8_u16:
.LLFB484:
	save	%sp, -96, %sp
.LLCFI484:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE484:
	.size	Mod__uMod_param_i8_u16, .-Mod__uMod_param_i8_u16
	.align 4
	.type	Mod__Mod_param_i8_u16, #function
	.proc	02
Mod__Mod_param_i8_u16:
.LLFB485:
	save	%sp, -96, %sp
.LLCFI485:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2495
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2494
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2498
	 nop
.LL2494:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2497
	 nop
.LL2495:
	cmp	%g1, 0
	bg	%icc, .LL2496
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2498
	 nop
.LL2496:
	add	%g1, -1, %g1
.LL2497:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2498:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2499
	 nop
	sethi	%hi(16384), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL2499:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2501
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2500
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2504
	 nop
.LL2500:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2503
	 nop
.LL2501:
	cmp	%g1, 0
	bg	%icc, .LL2502
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2504
	 nop
.LL2502:
	add	%g1, -1, %g1
.LL2503:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2504:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE485:
	.size	Mod__Mod_param_i8_u16, .-Mod__Mod_param_i8_u16
	.align 4
	.type	Mod__uMod_var_i8_u64, #function
	.proc	05
Mod__uMod_var_i8_u64:
.LLFB486:
	save	%sp, -96, %sp
.LLCFI486:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE486:
	.size	Mod__uMod_var_i8_u64, .-Mod__uMod_var_i8_u64
	.align 4
	.type	Mod__Mod_var_i8_u64, #function
	.proc	05
Mod__Mod_var_i8_u64:
.LLFB487:
	save	%sp, -96, %sp
.LLCFI487:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2508
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2512
	 nop
	cmp	%l1, 0
.LL2512:
	cmp	%o4, 0
	bl	%icc, .LL2507
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2513
	 nop
	cmp	%o5, 0
.LL2513:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2511
	 nop
.LL2507:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2510
	 nop
.LL2508:
	cmp	%o4, 0
	bg	%icc, .LL2509
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2514
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2509
	 nop
	cmp	%o5, 0
.LL2514:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2511
	 nop
.LL2509:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2510:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2511:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE487:
	.size	Mod__Mod_var_i8_u64, .-Mod__Mod_var_i8_u64
	.align 4
	.type	Mod__uMod_param_i8_u64, #function
	.proc	05
Mod__uMod_param_i8_u64:
.LLFB488:
	save	%sp, -104, %sp
.LLCFI488:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE488:
	.size	Mod__uMod_param_i8_u64, .-Mod__uMod_param_i8_u64
	.align 4
	.type	Mod__Mod_param_i8_u64, #function
	.proc	05
Mod__Mod_param_i8_u64:
.LLFB489:
	save	%sp, -104, %sp
.LLCFI489:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2518
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2522
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2522:
	cmp	%o4, 0
	bl	%icc, .LL2517
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2523
	 nop
	cmp	%o5, 0
.LL2523:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2521
	 nop
.LL2517:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2520
	 nop
.LL2518:
	cmp	%o4, 0
	bg	%icc, .LL2519
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2524
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2519
	 nop
	cmp	%o5, 0
.LL2524:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2521
	 nop
.LL2519:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2520:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2521:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE489:
	.size	Mod__Mod_param_i8_u64, .-Mod__Mod_param_i8_u64
	.align 4
	.type	Mod__uMod_var_i8_LC, #function
	.proc	05
Mod__uMod_var_i8_LC:
.LLFB490:
	save	%sp, -96, %sp
.LLCFI490:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE490:
	.size	Mod__uMod_var_i8_LC, .-Mod__uMod_var_i8_LC
	.align 4
	.type	Mod__Mod_var_i8_LC, #function
	.proc	05
Mod__Mod_var_i8_LC:
.LLFB491:
	save	%sp, -96, %sp
.LLCFI491:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2528
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2532
	 nop
	cmp	%l1, 0
.LL2532:
	cmp	%o4, 0
	bl	%icc, .LL2527
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2533
	 nop
	cmp	%o5, 0
.LL2533:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2531
	 nop
.LL2527:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2530
	 nop
.LL2528:
	cmp	%o4, 0
	bg	%icc, .LL2529
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2534
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2529
	 nop
	cmp	%o5, 0
.LL2534:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2531
	 nop
.LL2529:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2530:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2531:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE491:
	.size	Mod__Mod_var_i8_LC, .-Mod__Mod_var_i8_LC
	.align 4
	.type	Mod__uMod_param_i8_LC, #function
	.proc	05
Mod__uMod_param_i8_LC:
.LLFB492:
	save	%sp, -104, %sp
.LLCFI492:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE492:
	.size	Mod__uMod_param_i8_LC, .-Mod__uMod_param_i8_LC
	.align 4
	.type	Mod__Mod_param_i8_LC, #function
	.proc	05
Mod__Mod_param_i8_LC:
.LLFB493:
	save	%sp, -104, %sp
.LLCFI493:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2538
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2542
	 nop
	cmp	%l1, 0
.LL2542:
	cmp	%o4, 0
	bl	%icc, .LL2537
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2543
	 nop
	cmp	%o5, 0
.LL2543:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2541
	 nop
.LL2537:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2540
	 nop
.LL2538:
	cmp	%o4, 0
	bg	%icc, .LL2539
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2544
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2539
	 nop
	cmp	%o5, 0
.LL2544:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2541
	 nop
.LL2539:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2540:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2541:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE493:
	.size	Mod__Mod_param_i8_LC, .-Mod__Mod_param_i8_LC
	.align 4
	.type	Mod__uMod_var_i8_i32, #function
	.proc	04
Mod__uMod_var_i8_i32:
.LLFB494:
	save	%sp, -96, %sp
.LLCFI494:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE494:
	.size	Mod__uMod_var_i8_i32, .-Mod__uMod_var_i8_i32
	.align 4
	.type	Mod__Mod_var_i8_i32, #function
	.proc	02
Mod__Mod_var_i8_i32:
.LLFB495:
	save	%sp, -96, %sp
.LLCFI495:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2548
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2547
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2551
	 nop
.LL2547:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2550
	 nop
.LL2548:
	cmp	%g1, 0
	bg	%icc, .LL2549
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2551
	 nop
.LL2549:
	add	%g1, -1, %g1
.LL2550:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2551:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2552
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2554
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2553
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2557
	 nop
.LL2553:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2556
	 nop
.LL2554:
	cmp	%g1, 0
	bg	%icc, .LL2555
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2557
	 nop
.LL2555:
	add	%g1, -1, %g1
.LL2556:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2557:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2558
	 nop
.LL2552:
	sethi	%hi(17408), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2558:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2560
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2559
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2563
	 nop
.LL2559:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2562
	 nop
.LL2560:
	cmp	%g1, 0
	bg	%icc, .LL2561
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2563
	 nop
.LL2561:
	add	%g1, -1, %g1
.LL2562:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2563:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE495:
	.size	Mod__Mod_var_i8_i32, .-Mod__Mod_var_i8_i32
	.align 4
	.type	Mod__uMod_param_i8_i32, #function
	.proc	04
Mod__uMod_param_i8_i32:
.LLFB496:
	save	%sp, -96, %sp
.LLCFI496:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE496:
	.size	Mod__uMod_param_i8_i32, .-Mod__uMod_param_i8_i32
	.align 4
	.type	Mod__Mod_param_i8_i32, #function
	.proc	02
Mod__Mod_param_i8_i32:
.LLFB497:
	save	%sp, -96, %sp
.LLCFI497:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2567
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2566
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2570
	 nop
.LL2566:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2569
	 nop
.LL2567:
	cmp	%g1, 0
	bg	%icc, .LL2568
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2570
	 nop
.LL2568:
	add	%g1, -1, %g1
.LL2569:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2570:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2571
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2573
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2572
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2576
	 nop
.LL2572:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2575
	 nop
.LL2573:
	cmp	%g1, 0
	bg	%icc, .LL2574
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2576
	 nop
.LL2574:
	add	%g1, -1, %g1
.LL2575:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2576:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2577
	 nop
.LL2571:
	sethi	%hi(17408), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2577:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2579
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2578
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2582
	 nop
.LL2578:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2581
	 nop
.LL2579:
	cmp	%g1, 0
	bg	%icc, .LL2580
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2582
	 nop
.LL2580:
	add	%g1, -1, %g1
.LL2581:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2582:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE497:
	.size	Mod__Mod_param_i8_i32, .-Mod__Mod_param_i8_i32
	.align 4
	.type	Mod__uMod_var_i8_i16, #function
	.proc	04
Mod__uMod_var_i8_i16:
.LLFB498:
	save	%sp, -96, %sp
.LLCFI498:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE498:
	.size	Mod__uMod_var_i8_i16, .-Mod__uMod_var_i8_i16
	.align 4
	.type	Mod__Mod_var_i8_i16, #function
	.proc	02
Mod__Mod_var_i8_i16:
.LLFB499:
	save	%sp, -96, %sp
.LLCFI499:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2586
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2585
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2589
	 nop
.LL2585:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2588
	 nop
.LL2586:
	cmp	%g1, 0
	bg	%icc, .LL2587
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2589
	 nop
.LL2587:
	add	%g1, -1, %g1
.LL2588:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2589:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2590
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2592
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2591
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2595
	 nop
.LL2591:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2594
	 nop
.LL2592:
	cmp	%g1, 0
	bg	%icc, .LL2593
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2595
	 nop
.LL2593:
	add	%g1, -1, %g1
.LL2594:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2595:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2596
	 nop
.LL2590:
	sethi	%hi(17408), %g1
	or	%g1, 289, %o0
	call	_m3_fault, 0
	 nop
.LL2596:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2598
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2597
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2601
	 nop
.LL2597:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2600
	 nop
.LL2598:
	cmp	%g1, 0
	bg	%icc, .LL2599
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2601
	 nop
.LL2599:
	add	%g1, -1, %g1
.LL2600:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2601:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE499:
	.size	Mod__Mod_var_i8_i16, .-Mod__Mod_var_i8_i16
	.align 4
	.type	Mod__uMod_param_i8_i16, #function
	.proc	04
Mod__uMod_param_i8_i16:
.LLFB500:
	save	%sp, -96, %sp
.LLCFI500:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE500:
	.size	Mod__uMod_param_i8_i16, .-Mod__uMod_param_i8_i16
	.align 4
	.type	Mod__Mod_param_i8_i16, #function
	.proc	02
Mod__Mod_param_i8_i16:
.LLFB501:
	save	%sp, -96, %sp
.LLCFI501:
	stb	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2605
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2604
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2608
	 nop
.LL2604:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2607
	 nop
.LL2605:
	cmp	%g1, 0
	bg	%icc, .LL2606
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2608
	 nop
.LL2606:
	add	%g1, -1, %g1
.LL2607:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2608:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2609
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2611
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2610
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2614
	 nop
.LL2610:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2613
	 nop
.LL2611:
	cmp	%g1, 0
	bg	%icc, .LL2612
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2614
	 nop
.LL2612:
	add	%g1, -1, %g1
.LL2613:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2614:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2615
	 nop
.LL2609:
	sethi	%hi(17408), %g1
	or	%g1, 353, %o0
	call	_m3_fault, 0
	 nop
.LL2615:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2617
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2616
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2620
	 nop
.LL2616:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2619
	 nop
.LL2617:
	cmp	%g1, 0
	bg	%icc, .LL2618
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2620
	 nop
.LL2618:
	add	%g1, -1, %g1
.LL2619:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2620:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE501:
	.size	Mod__Mod_param_i8_i16, .-Mod__Mod_param_i8_i16
	.align 4
	.type	Mod__uMod_var_i8_I, #function
	.proc	04
Mod__uMod_var_i8_I:
.LLFB502:
	save	%sp, -96, %sp
.LLCFI502:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE502:
	.size	Mod__uMod_var_i8_I, .-Mod__uMod_var_i8_I
	.align 4
	.type	Mod__Mod_var_i8_I, #function
	.proc	02
Mod__Mod_var_i8_I:
.LLFB503:
	save	%sp, -96, %sp
.LLCFI503:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2624
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2623
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2627
	 nop
.LL2623:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2626
	 nop
.LL2624:
	cmp	%g1, 0
	bg	%icc, .LL2625
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2627
	 nop
.LL2625:
	add	%g1, -1, %g1
.LL2626:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2627:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2628
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2630
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2629
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2633
	 nop
.LL2629:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2632
	 nop
.LL2630:
	cmp	%g1, 0
	bg	%icc, .LL2631
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2633
	 nop
.LL2631:
	add	%g1, -1, %g1
.LL2632:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2633:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2634
	 nop
.LL2628:
	sethi	%hi(17408), %g1
	or	%g1, 417, %o0
	call	_m3_fault, 0
	 nop
.LL2634:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2636
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2635
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2639
	 nop
.LL2635:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2638
	 nop
.LL2636:
	cmp	%g1, 0
	bg	%icc, .LL2637
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2639
	 nop
.LL2637:
	add	%g1, -1, %g1
.LL2638:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2639:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE503:
	.size	Mod__Mod_var_i8_I, .-Mod__Mod_var_i8_I
	.align 4
	.type	Mod__uMod_param_i8_I, #function
	.proc	04
Mod__uMod_param_i8_I:
.LLFB504:
	save	%sp, -96, %sp
.LLCFI504:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE504:
	.size	Mod__uMod_param_i8_I, .-Mod__uMod_param_i8_I
	.align 4
	.type	Mod__Mod_param_i8_I, #function
	.proc	02
Mod__Mod_param_i8_I:
.LLFB505:
	save	%sp, -96, %sp
.LLCFI505:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2643
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2642
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2646
	 nop
.LL2642:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2645
	 nop
.LL2643:
	cmp	%g1, 0
	bg	%icc, .LL2644
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2646
	 nop
.LL2644:
	add	%g1, -1, %g1
.LL2645:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2646:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2647
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2649
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2648
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2652
	 nop
.LL2648:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2651
	 nop
.LL2649:
	cmp	%g1, 0
	bg	%icc, .LL2650
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2652
	 nop
.LL2650:
	add	%g1, -1, %g1
.LL2651:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2652:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2653
	 nop
.LL2647:
	sethi	%hi(17408), %g1
	or	%g1, 481, %o0
	call	_m3_fault, 0
	 nop
.LL2653:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2655
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2654
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2658
	 nop
.LL2654:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2657
	 nop
.LL2655:
	cmp	%g1, 0
	bg	%icc, .LL2656
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2658
	 nop
.LL2656:
	add	%g1, -1, %g1
.LL2657:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2658:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE505:
	.size	Mod__Mod_param_i8_I, .-Mod__Mod_param_i8_I
	.align 4
	.type	Mod__uMod_var_i8_i64, #function
	.proc	05
Mod__uMod_var_i8_i64:
.LLFB506:
	save	%sp, -96, %sp
.LLCFI506:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE506:
	.size	Mod__uMod_var_i8_i64, .-Mod__uMod_var_i8_i64
	.align 4
	.type	Mod__Mod_var_i8_i64, #function
	.proc	05
Mod__Mod_var_i8_i64:
.LLFB507:
	save	%sp, -96, %sp
.LLCFI507:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2662
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2666
	 nop
	cmp	%l1, 0
.LL2666:
	cmp	%o4, 0
	bl	%icc, .LL2661
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2667
	 nop
	cmp	%o5, 0
.LL2667:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2665
	 nop
.LL2661:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2664
	 nop
.LL2662:
	cmp	%o4, 0
	bg	%icc, .LL2663
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2668
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2663
	 nop
	cmp	%o5, 0
.LL2668:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2665
	 nop
.LL2663:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2664:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2665:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE507:
	.size	Mod__Mod_var_i8_i64, .-Mod__Mod_var_i8_i64
	.align 4
	.type	Mod__uMod_param_i8_i64, #function
	.proc	05
Mod__uMod_param_i8_i64:
.LLFB508:
	save	%sp, -104, %sp
.LLCFI508:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE508:
	.size	Mod__uMod_param_i8_i64, .-Mod__uMod_param_i8_i64
	.align 4
	.type	Mod__Mod_param_i8_i64, #function
	.proc	05
Mod__Mod_param_i8_i64:
.LLFB509:
	save	%sp, -104, %sp
.LLCFI509:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2672
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2676
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2676:
	cmp	%o4, 0
	bl	%icc, .LL2671
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2677
	 nop
	cmp	%o5, 0
.LL2677:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2675
	 nop
.LL2671:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2674
	 nop
.LL2672:
	cmp	%o4, 0
	bg	%icc, .LL2673
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2678
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2673
	 nop
	cmp	%o5, 0
.LL2678:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2675
	 nop
.LL2673:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2674:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2675:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE509:
	.size	Mod__Mod_param_i8_i64, .-Mod__Mod_param_i8_i64
	.align 4
	.type	Mod__uMod_var_i8_C, #function
	.proc	04
Mod__uMod_var_i8_C:
.LLFB510:
	save	%sp, -96, %sp
.LLCFI510:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE510:
	.size	Mod__uMod_var_i8_C, .-Mod__uMod_var_i8_C
	.align 4
	.type	Mod__Mod_var_i8_C, #function
	.proc	02
Mod__Mod_var_i8_C:
.LLFB511:
	save	%sp, -96, %sp
.LLCFI511:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2682
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2681
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2685
	 nop
.LL2681:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2684
	 nop
.LL2682:
	cmp	%g1, 0
	bg	%icc, .LL2683
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2685
	 nop
.LL2683:
	add	%g1, -1, %g1
.LL2684:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2685:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2686
	 nop
	sethi	%hi(17408), %g1
	or	%g1, 673, %o0
	call	_m3_fault, 0
	 nop
.LL2686:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2688
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2687
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2691
	 nop
.LL2687:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2690
	 nop
.LL2688:
	cmp	%g1, 0
	bg	%icc, .LL2689
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2691
	 nop
.LL2689:
	add	%g1, -1, %g1
.LL2690:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2691:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE511:
	.size	Mod__Mod_var_i8_C, .-Mod__Mod_var_i8_C
	.align 4
	.type	Mod__uMod_param_i8_C, #function
	.proc	04
Mod__uMod_param_i8_C:
.LLFB512:
	save	%sp, -96, %sp
.LLCFI512:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE512:
	.size	Mod__uMod_param_i8_C, .-Mod__uMod_param_i8_C
	.align 4
	.type	Mod__Mod_param_i8_C, #function
	.proc	02
Mod__Mod_param_i8_C:
.LLFB513:
	save	%sp, -96, %sp
.LLCFI513:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2695
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2694
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2698
	 nop
.LL2694:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2697
	 nop
.LL2695:
	cmp	%g1, 0
	bg	%icc, .LL2696
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2698
	 nop
.LL2696:
	add	%g1, -1, %g1
.LL2697:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2698:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2699
	 nop
	sethi	%hi(17408), %g1
	or	%g1, 737, %o0
	call	_m3_fault, 0
	 nop
.LL2699:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ld	[%fp+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2701
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2700
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2704
	 nop
.LL2700:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2703
	 nop
.LL2701:
	cmp	%g1, 0
	bg	%icc, .LL2702
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2704
	 nop
.LL2702:
	add	%g1, -1, %g1
.LL2703:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2704:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE513:
	.size	Mod__Mod_param_i8_C, .-Mod__Mod_param_i8_C
	.align 4
	.type	Mod__uMod_var_i8_u8, #function
	.proc	04
Mod__uMod_var_i8_u8:
.LLFB514:
	save	%sp, -96, %sp
.LLCFI514:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE514:
	.size	Mod__uMod_var_i8_u8, .-Mod__uMod_var_i8_u8
	.align 4
	.type	Mod__Mod_var_i8_u8, #function
	.proc	02
Mod__Mod_var_i8_u8:
.LLFB515:
	save	%sp, -96, %sp
.LLCFI515:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2708
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2707
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2711
	 nop
.LL2707:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2710
	 nop
.LL2708:
	cmp	%g1, 0
	bg	%icc, .LL2709
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2711
	 nop
.LL2709:
	add	%g1, -1, %g1
.LL2710:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2711:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2712
	 nop
	sethi	%hi(17408), %g1
	or	%g1, 801, %o0
	call	_m3_fault, 0
	 nop
.LL2712:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2714
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2713
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2717
	 nop
.LL2713:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2716
	 nop
.LL2714:
	cmp	%g1, 0
	bg	%icc, .LL2715
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2717
	 nop
.LL2715:
	add	%g1, -1, %g1
.LL2716:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2717:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE515:
	.size	Mod__Mod_var_i8_u8, .-Mod__Mod_var_i8_u8
	.align 4
	.type	Mod__uMod_param_i8_u8, #function
	.proc	04
Mod__uMod_param_i8_u8:
.LLFB516:
	save	%sp, -96, %sp
.LLCFI516:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE516:
	.size	Mod__uMod_param_i8_u8, .-Mod__uMod_param_i8_u8
	.align 4
	.type	Mod__Mod_param_i8_u8, #function
	.proc	02
Mod__Mod_param_i8_u8:
.LLFB517:
	save	%sp, -96, %sp
.LLCFI517:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2721
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2720
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2724
	 nop
.LL2720:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2723
	 nop
.LL2721:
	cmp	%g1, 0
	bg	%icc, .LL2722
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2724
	 nop
.LL2722:
	add	%g1, -1, %g1
.LL2723:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2724:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2725
	 nop
	sethi	%hi(17408), %g1
	or	%g1, 865, %o0
	call	_m3_fault, 0
	 nop
.LL2725:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2727
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2726
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2730
	 nop
.LL2726:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2729
	 nop
.LL2727:
	cmp	%g1, 0
	bg	%icc, .LL2728
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2730
	 nop
.LL2728:
	add	%g1, -1, %g1
.LL2729:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2730:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE517:
	.size	Mod__Mod_param_i8_u8, .-Mod__Mod_param_i8_u8
	.align 4
	.type	Mod__uMod_var_i8_L, #function
	.proc	05
Mod__uMod_var_i8_L:
.LLFB518:
	save	%sp, -96, %sp
.LLCFI518:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE518:
	.size	Mod__uMod_var_i8_L, .-Mod__uMod_var_i8_L
	.align 4
	.type	Mod__Mod_var_i8_L, #function
	.proc	05
Mod__Mod_var_i8_L:
.LLFB519:
	save	%sp, -96, %sp
.LLCFI519:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2734
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2738
	 nop
	cmp	%l1, 0
.LL2738:
	cmp	%o4, 0
	bl	%icc, .LL2733
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2739
	 nop
	cmp	%o5, 0
.LL2739:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2737
	 nop
.LL2733:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2736
	 nop
.LL2734:
	cmp	%o4, 0
	bg	%icc, .LL2735
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2740
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2735
	 nop
	cmp	%o5, 0
.LL2740:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2737
	 nop
.LL2735:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2736:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2737:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE519:
	.size	Mod__Mod_var_i8_L, .-Mod__Mod_var_i8_L
	.align 4
	.type	Mod__uMod_param_i8_L, #function
	.proc	05
Mod__uMod_param_i8_L:
.LLFB520:
	save	%sp, -104, %sp
.LLCFI520:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE520:
	.size	Mod__uMod_param_i8_L, .-Mod__uMod_param_i8_L
	.align 4
	.type	Mod__Mod_param_i8_L, #function
	.proc	05
Mod__Mod_param_i8_L:
.LLFB521:
	save	%sp, -104, %sp
.LLCFI521:
	stb	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	mov	%g1, %l1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2744
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2748
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2748:
	cmp	%o4, 0
	bl	%icc, .LL2743
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2749
	 nop
	cmp	%o5, 0
.LL2749:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2747
	 nop
.LL2743:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2746
	 nop
.LL2744:
	cmp	%o4, 0
	bg	%icc, .LL2745
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2750
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2745
	 nop
	cmp	%o5, 0
.LL2750:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2747
	 nop
.LL2745:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2746:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2747:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE521:
	.size	Mod__Mod_param_i8_L, .-Mod__Mod_param_i8_L
	.align 4
	.type	Mod__uMod_var_i8_i8, #function
	.proc	04
Mod__uMod_var_i8_i8:
.LLFB522:
	save	%sp, -96, %sp
.LLCFI522:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE522:
	.size	Mod__uMod_var_i8_i8, .-Mod__uMod_var_i8_i8
	.align 4
	.type	Mod__Mod_var_i8_i8, #function
	.proc	02
Mod__Mod_var_i8_i8:
.LLFB523:
	save	%sp, -96, %sp
.LLCFI523:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2754
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2753
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2757
	 nop
.LL2753:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2756
	 nop
.LL2754:
	cmp	%g1, 0
	bg	%icc, .LL2755
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2757
	 nop
.LL2755:
	add	%g1, -1, %g1
.LL2756:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2757:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2758
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2760
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2759
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2763
	 nop
.LL2759:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2762
	 nop
.LL2760:
	cmp	%g1, 0
	bg	%icc, .LL2761
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2763
	 nop
.LL2761:
	add	%g1, -1, %g1
.LL2762:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2763:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2764
	 nop
.LL2758:
	sethi	%hi(18432), %g1
	or	%g1, 33, %o0
	call	_m3_fault, 0
	 nop
.LL2764:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2766
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2765
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2769
	 nop
.LL2765:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2768
	 nop
.LL2766:
	cmp	%g1, 0
	bg	%icc, .LL2767
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2769
	 nop
.LL2767:
	add	%g1, -1, %g1
.LL2768:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2769:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE523:
	.size	Mod__Mod_var_i8_i8, .-Mod__Mod_var_i8_i8
	.align 4
	.type	Mod__uMod_param_i8_i8, #function
	.proc	04
Mod__uMod_param_i8_i8:
.LLFB524:
	save	%sp, -96, %sp
.LLCFI524:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE524:
	.size	Mod__uMod_param_i8_i8, .-Mod__uMod_param_i8_i8
	.align 4
	.type	Mod__Mod_param_i8_i8, #function
	.proc	02
Mod__Mod_param_i8_i8:
.LLFB525:
	save	%sp, -96, %sp
.LLCFI525:
	stb	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2773
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2772
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2776
	 nop
.LL2772:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2775
	 nop
.LL2773:
	cmp	%g1, 0
	bg	%icc, .LL2774
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2776
	 nop
.LL2774:
	add	%g1, -1, %g1
.LL2775:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2776:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2777
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2779
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2778
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2782
	 nop
.LL2778:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2781
	 nop
.LL2779:
	cmp	%g1, 0
	bg	%icc, .LL2780
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2782
	 nop
.LL2780:
	add	%g1, -1, %g1
.LL2781:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2782:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2783
	 nop
.LL2777:
	sethi	%hi(18432), %g1
	or	%g1, 97, %o0
	call	_m3_fault, 0
	 nop
.LL2783:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2785
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2784
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2788
	 nop
.LL2784:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2787
	 nop
.LL2785:
	cmp	%g1, 0
	bg	%icc, .LL2786
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2788
	 nop
.LL2786:
	add	%g1, -1, %g1
.LL2787:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2788:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE525:
	.size	Mod__Mod_param_i8_i8, .-Mod__Mod_param_i8_i8
	.align 4
	.type	Mod__uMod_var_i8_u32, #function
	.proc	04
Mod__uMod_var_i8_u32:
.LLFB526:
	save	%sp, -96, %sp
.LLCFI526:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE526:
	.size	Mod__uMod_var_i8_u32, .-Mod__uMod_var_i8_u32
	.align 4
	.type	Mod__Mod_var_i8_u32, #function
	.proc	02
Mod__Mod_var_i8_u32:
.LLFB527:
	save	%sp, -96, %sp
.LLCFI527:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2792
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2791
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2795
	 nop
.LL2791:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2794
	 nop
.LL2792:
	cmp	%g1, 0
	bg	%icc, .LL2793
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2795
	 nop
.LL2793:
	add	%g1, -1, %g1
.LL2794:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2795:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2796
	 nop
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2798
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2797
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2801
	 nop
.LL2797:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2800
	 nop
.LL2798:
	cmp	%g1, 0
	bg	%icc, .LL2799
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2801
	 nop
.LL2799:
	add	%g1, -1, %g1
.LL2800:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2801:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2802
	 nop
.LL2796:
	sethi	%hi(18432), %g1
	or	%g1, 161, %o0
	call	_m3_fault, 0
	 nop
.LL2802:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2804
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2803
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2807
	 nop
.LL2803:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2806
	 nop
.LL2804:
	cmp	%g1, 0
	bg	%icc, .LL2805
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2807
	 nop
.LL2805:
	add	%g1, -1, %g1
.LL2806:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2807:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE527:
	.size	Mod__Mod_var_i8_u32, .-Mod__Mod_var_i8_u32
	.align 4
	.type	Mod__uMod_param_i8_u32, #function
	.proc	04
Mod__uMod_param_i8_u32:
.LLFB528:
	save	%sp, -96, %sp
.LLCFI528:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE528:
	.size	Mod__uMod_param_i8_u32, .-Mod__uMod_param_i8_u32
	.align 4
	.type	Mod__Mod_param_i8_u32, #function
	.proc	02
Mod__Mod_param_i8_u32:
.LLFB529:
	save	%sp, -96, %sp
.LLCFI529:
	stb	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2811
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2810
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2814
	 nop
.LL2810:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2813
	 nop
.LL2811:
	cmp	%g1, 0
	bg	%icc, .LL2812
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2814
	 nop
.LL2812:
	add	%g1, -1, %g1
.LL2813:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2814:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, -128
	bl	%icc, .LL2815
	 nop
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2817
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2816
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2820
	 nop
.LL2816:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2819
	 nop
.LL2817:
	cmp	%g1, 0
	bg	%icc, .LL2818
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2820
	 nop
.LL2818:
	add	%g1, -1, %g1
.LL2819:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2820:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	cmp	%g1, 127
	ble	%icc, .LL2821
	 nop
.LL2815:
	sethi	%hi(18432), %g1
	or	%g1, 225, %o0
	call	_m3_fault, 0
	 nop
.LL2821:
	ldub	[%fp+68], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2823
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2822
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2826
	 nop
.LL2822:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2825
	 nop
.LL2823:
	cmp	%g1, 0
	bg	%icc, .LL2824
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2826
	 nop
.LL2824:
	add	%g1, -1, %g1
.LL2825:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2826:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE529:
	.size	Mod__Mod_param_i8_u32, .-Mod__Mod_param_i8_u32
	.align 4
	.type	Mod__uMod_var_u32_u16, #function
	.proc	04
Mod__uMod_var_u32_u16:
.LLFB530:
	save	%sp, -96, %sp
.LLCFI530:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+52], %o5
	srl	%o5, 16, %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE530:
	.size	Mod__uMod_var_u32_u16, .-Mod__uMod_var_u32_u16
	.align 4
	.type	Mod__Mod_var_u32_u16, #function
	.proc	04
Mod__Mod_var_u32_u16:
.LLFB531:
	save	%sp, -96, %sp
.LLCFI531:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+52], %g1
	srl	%g1, 16, %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2830
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2829
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2833
	 nop
.LL2829:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2832
	 nop
.LL2830:
	cmp	%g1, 0
	bg	%icc, .LL2831
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2833
	 nop
.LL2831:
	add	%g1, -1, %g1
.LL2832:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2833:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE531:
	.size	Mod__Mod_var_u32_u16, .-Mod__Mod_var_u32_u16
	.align 4
	.type	Mod__uMod_param_u32_u16, #function
	.proc	04
Mod__uMod_param_u32_u16:
.LLFB532:
	save	%sp, -96, %sp
.LLCFI532:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	srl	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE532:
	.size	Mod__uMod_param_u32_u16, .-Mod__uMod_param_u32_u16
	.align 4
	.type	Mod__Mod_param_u32_u16, #function
	.proc	04
Mod__Mod_param_u32_u16:
.LLFB533:
	save	%sp, -96, %sp
.LLCFI533:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	srl	%g1, 16, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2837
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2836
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2840
	 nop
.LL2836:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2839
	 nop
.LL2837:
	cmp	%g1, 0
	bg	%icc, .LL2838
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2840
	 nop
.LL2838:
	add	%g1, -1, %g1
.LL2839:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2840:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE533:
	.size	Mod__Mod_param_u32_u16, .-Mod__Mod_param_u32_u16
	.align 4
	.type	Mod__uMod_var_u32_u64, #function
	.proc	05
Mod__uMod_var_u32_u64:
.LLFB534:
	save	%sp, -96, %sp
.LLCFI534:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE534:
	.size	Mod__uMod_var_u32_u64, .-Mod__uMod_var_u32_u64
	.align 4
	.type	Mod__Mod_var_u32_u64, #function
	.proc	05
Mod__Mod_var_u32_u64:
.LLFB535:
	save	%sp, -96, %sp
.LLCFI535:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+56], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2844
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2848
	 nop
	cmp	%l1, 0
.LL2848:
	cmp	%o4, 0
	bl	%icc, .LL2843
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2849
	 nop
	cmp	%o5, 0
.LL2849:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2847
	 nop
.LL2843:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2846
	 nop
.LL2844:
	cmp	%o4, 0
	bg	%icc, .LL2845
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2850
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2845
	 nop
	cmp	%o5, 0
.LL2850:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2847
	 nop
.LL2845:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2846:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2847:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE535:
	.size	Mod__Mod_var_u32_u64, .-Mod__Mod_var_u32_u64
	.align 4
	.type	Mod__uMod_param_u32_u64, #function
	.proc	05
Mod__uMod_param_u32_u64:
.LLFB536:
	save	%sp, -104, %sp
.LLCFI536:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE536:
	.size	Mod__uMod_param_u32_u64, .-Mod__uMod_param_u32_u64
	.align 4
	.type	Mod__Mod_param_u32_u64, #function
	.proc	05
Mod__Mod_param_u32_u64:
.LLFB537:
	save	%sp, -104, %sp
.LLCFI537:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2854
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2858
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2858:
	cmp	%o4, 0
	bl	%icc, .LL2853
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2859
	 nop
	cmp	%o5, 0
.LL2859:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2857
	 nop
.LL2853:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2856
	 nop
.LL2854:
	cmp	%o4, 0
	bg	%icc, .LL2855
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2860
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2855
	 nop
	cmp	%o5, 0
.LL2860:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2857
	 nop
.LL2855:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2856:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2857:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE537:
	.size	Mod__Mod_param_u32_u64, .-Mod__Mod_param_u32_u64
	.align 4
	.type	Mod__uMod_var_u32_LC, #function
	.proc	05
Mod__uMod_var_u32_LC:
.LLFB538:
	save	%sp, -96, %sp
.LLCFI538:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE538:
	.size	Mod__uMod_var_u32_LC, .-Mod__uMod_var_u32_LC
	.align 4
	.type	Mod__Mod_var_u32_LC, #function
	.proc	05
Mod__Mod_var_u32_LC:
.LLFB539:
	save	%sp, -96, %sp
.LLCFI539:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+64], %o4
	mov	%o4, %l0
	mov	%o5, %l1
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2864
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2868
	 nop
	cmp	%l1, 0
.LL2868:
	cmp	%o4, 0
	bl	%icc, .LL2863
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2869
	 nop
	cmp	%o5, 0
.LL2869:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2867
	 nop
.LL2863:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2866
	 nop
.LL2864:
	cmp	%o4, 0
	bg	%icc, .LL2865
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2870
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2865
	 nop
	cmp	%o5, 0
.LL2870:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2867
	 nop
.LL2865:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2866:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2867:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE539:
	.size	Mod__Mod_var_u32_LC, .-Mod__Mod_var_u32_LC
	.align 4
	.type	Mod__uMod_param_u32_LC, #function
	.proc	05
Mod__uMod_param_u32_LC:
.LLFB540:
	save	%sp, -104, %sp
.LLCFI540:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE540:
	.size	Mod__uMod_param_u32_LC, .-Mod__uMod_param_u32_LC
	.align 4
	.type	Mod__Mod_param_u32_LC, #function
	.proc	05
Mod__Mod_param_u32_LC:
.LLFB541:
	save	%sp, -104, %sp
.LLCFI541:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l3
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l2
	ldd	[%fp-8], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2874
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2878
	 nop
	cmp	%l1, 0
.LL2878:
	cmp	%o4, 0
	bl	%icc, .LL2873
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2879
	 nop
	cmp	%o5, 0
.LL2879:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2877
	 nop
.LL2873:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2876
	 nop
.LL2874:
	cmp	%o4, 0
	bg	%icc, .LL2875
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2880
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2875
	 nop
	cmp	%o5, 0
.LL2880:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2877
	 nop
.LL2875:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2876:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2877:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE541:
	.size	Mod__Mod_param_u32_LC, .-Mod__Mod_param_u32_LC
	.align 4
	.type	Mod__uMod_var_u32_i32, #function
	.proc	04
Mod__uMod_var_u32_i32:
.LLFB542:
	save	%sp, -96, %sp
.LLCFI542:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE542:
	.size	Mod__uMod_var_u32_i32, .-Mod__uMod_var_u32_i32
	.align 4
	.type	Mod__Mod_var_u32_i32, #function
	.proc	04
Mod__Mod_var_u32_i32:
.LLFB543:
	save	%sp, -96, %sp
.LLCFI543:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+72], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2884
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2883
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2887
	 nop
.LL2883:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2886
	 nop
.LL2884:
	cmp	%g1, 0
	bg	%icc, .LL2885
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2887
	 nop
.LL2885:
	add	%g1, -1, %g1
.LL2886:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2887:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE543:
	.size	Mod__Mod_var_u32_i32, .-Mod__Mod_var_u32_i32
	.align 4
	.type	Mod__uMod_param_u32_i32, #function
	.proc	04
Mod__uMod_param_u32_i32:
.LLFB544:
	save	%sp, -96, %sp
.LLCFI544:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE544:
	.size	Mod__uMod_param_u32_i32, .-Mod__uMod_param_u32_i32
	.align 4
	.type	Mod__Mod_param_u32_i32, #function
	.proc	04
Mod__Mod_param_u32_i32:
.LLFB545:
	save	%sp, -96, %sp
.LLCFI545:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2891
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2890
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2894
	 nop
.LL2890:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2893
	 nop
.LL2891:
	cmp	%g1, 0
	bg	%icc, .LL2892
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2894
	 nop
.LL2892:
	add	%g1, -1, %g1
.LL2893:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2894:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE545:
	.size	Mod__Mod_param_u32_i32, .-Mod__Mod_param_u32_i32
	.align 4
	.type	Mod__uMod_var_u32_i16, #function
	.proc	04
Mod__uMod_var_u32_i16:
.LLFB546:
	save	%sp, -96, %sp
.LLCFI546:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+76], %o5
	sra	%o5, 16, %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE546:
	.size	Mod__uMod_var_u32_i16, .-Mod__uMod_var_u32_i16
	.align 4
	.type	Mod__Mod_var_u32_i16, #function
	.proc	04
Mod__Mod_var_u32_i16:
.LLFB547:
	save	%sp, -96, %sp
.LLCFI547:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+76], %g1
	sra	%g1, 16, %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2898
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2897
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2901
	 nop
.LL2897:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2900
	 nop
.LL2898:
	cmp	%g1, 0
	bg	%icc, .LL2899
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2901
	 nop
.LL2899:
	add	%g1, -1, %g1
.LL2900:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2901:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE547:
	.size	Mod__Mod_var_u32_i16, .-Mod__Mod_var_u32_i16
	.align 4
	.type	Mod__uMod_param_u32_i16, #function
	.proc	04
Mod__uMod_param_u32_i16:
.LLFB548:
	save	%sp, -96, %sp
.LLCFI548:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	ld	[%fp+68], %g1
	lduh	[%fp+72], %o5
	sll	%o5, 16, %o5
	sra	%o5, 16, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE548:
	.size	Mod__uMod_param_u32_i16, .-Mod__uMod_param_u32_i16
	.align 4
	.type	Mod__Mod_param_u32_i16, #function
	.proc	04
Mod__Mod_param_u32_i16:
.LLFB549:
	save	%sp, -96, %sp
.LLCFI549:
	st	%i0, [%fp+68]
	sth	%i1, [%fp+72]
	lduh	[%fp+72], %g1
	sll	%g1, 16, %g1
	sra	%g1, 16, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2905
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2904
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2908
	 nop
.LL2904:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2907
	 nop
.LL2905:
	cmp	%g1, 0
	bg	%icc, .LL2906
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2908
	 nop
.LL2906:
	add	%g1, -1, %g1
.LL2907:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2908:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE549:
	.size	Mod__Mod_param_u32_i16, .-Mod__Mod_param_u32_i16
	.align 4
	.type	Mod__uMod_var_u32_I, #function
	.proc	04
Mod__uMod_var_u32_I:
.LLFB550:
	save	%sp, -96, %sp
.LLCFI550:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+80], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE550:
	.size	Mod__uMod_var_u32_I, .-Mod__uMod_var_u32_I
	.align 4
	.type	Mod__Mod_var_u32_I, #function
	.proc	04
Mod__Mod_var_u32_I:
.LLFB551:
	save	%sp, -96, %sp
.LLCFI551:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+80], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2912
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2911
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2915
	 nop
.LL2911:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2914
	 nop
.LL2912:
	cmp	%g1, 0
	bg	%icc, .LL2913
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2915
	 nop
.LL2913:
	add	%g1, -1, %g1
.LL2914:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2915:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE551:
	.size	Mod__Mod_var_u32_I, .-Mod__Mod_var_u32_I
	.align 4
	.type	Mod__uMod_param_u32_I, #function
	.proc	04
Mod__uMod_param_u32_I:
.LLFB552:
	save	%sp, -96, %sp
.LLCFI552:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE552:
	.size	Mod__uMod_param_u32_I, .-Mod__uMod_param_u32_I
	.align 4
	.type	Mod__Mod_param_u32_I, #function
	.proc	04
Mod__Mod_param_u32_I:
.LLFB553:
	save	%sp, -96, %sp
.LLCFI553:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL2919
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2918
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2922
	 nop
.LL2918:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2921
	 nop
.LL2919:
	cmp	%g1, 0
	bg	%icc, .LL2920
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL2922
	 nop
.LL2920:
	add	%g1, -1, %g1
.LL2921:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL2922:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE553:
	.size	Mod__Mod_param_u32_I, .-Mod__Mod_param_u32_I
	.align 4
	.type	Mod__uMod_var_u32_i64, #function
	.proc	05
Mod__uMod_var_u32_i64:
.LLFB554:
	save	%sp, -96, %sp
.LLCFI554:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE554:
	.size	Mod__uMod_var_u32_i64, .-Mod__uMod_var_u32_i64
	.align 4
	.type	Mod__Mod_var_u32_i64, #function
	.proc	05
Mod__Mod_var_u32_i64:
.LLFB555:
	save	%sp, -96, %sp
.LLCFI555:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+88], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2926
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2930
	 nop
	cmp	%l1, 0
.LL2930:
	cmp	%o4, 0
	bl	%icc, .LL2925
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2931
	 nop
	cmp	%o5, 0
.LL2931:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2929
	 nop
.LL2925:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2928
	 nop
.LL2926:
	cmp	%o4, 0
	bg	%icc, .LL2927
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2932
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2927
	 nop
	cmp	%o5, 0
.LL2932:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2929
	 nop
.LL2927:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2928:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2929:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE555:
	.size	Mod__Mod_var_u32_i64, .-Mod__Mod_var_u32_i64
	.align 4
	.type	Mod__uMod_param_u32_i64, #function
	.proc	05
Mod__uMod_param_u32_i64:
.LLFB556:
	save	%sp, -104, %sp
.LLCFI556:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE556:
	.size	Mod__uMod_param_u32_i64, .-Mod__uMod_param_u32_i64
	.align 4
	.type	Mod__Mod_param_u32_i64, #function
	.proc	05
Mod__Mod_param_u32_i64:
.LLFB557:
	save	%sp, -104, %sp
.LLCFI557:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2936
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2940
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2940:
	cmp	%o4, 0
	bl	%icc, .LL2935
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2941
	 nop
	cmp	%o5, 0
.LL2941:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2939
	 nop
.LL2935:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2938
	 nop
.LL2936:
	cmp	%o4, 0
	bg	%icc, .LL2937
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2942
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2937
	 nop
	cmp	%o5, 0
.LL2942:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2939
	 nop
.LL2937:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2938:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2939:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE557:
	.size	Mod__Mod_param_u32_i64, .-Mod__Mod_param_u32_i64
	.align 4
	.type	Mod__uMod_var_u32_C, #function
	.proc	04
Mod__uMod_var_u32_C:
.LLFB558:
	save	%sp, -96, %sp
.LLCFI558:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+96], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE558:
	.size	Mod__uMod_var_u32_C, .-Mod__uMod_var_u32_C
	.align 4
	.type	Mod__Mod_var_u32_C, #function
	.proc	04
Mod__Mod_var_u32_C:
.LLFB559:
	save	%sp, -96, %sp
.LLCFI559:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+96], %g1
	mov	%g1, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2946
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2945
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2949
	 nop
.LL2945:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2948
	 nop
.LL2946:
	cmp	%g1, 0
	bg	%icc, .LL2947
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2949
	 nop
.LL2947:
	add	%g1, -1, %g1
.LL2948:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2949:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE559:
	.size	Mod__Mod_var_u32_C, .-Mod__Mod_var_u32_C
	.align 4
	.type	Mod__uMod_param_u32_C, #function
	.proc	04
Mod__uMod_param_u32_C:
.LLFB560:
	save	%sp, -96, %sp
.LLCFI560:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	wr	%g0, 0, %y
	ld	[%fp+72], %o4
	udiv	%g1, %o4, %o4
	ld	[%fp+72], %o5
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE560:
	.size	Mod__uMod_param_u32_C, .-Mod__uMod_param_u32_C
	.align 4
	.type	Mod__Mod_param_u32_C, #function
	.proc	04
Mod__Mod_param_u32_C:
.LLFB561:
	save	%sp, -96, %sp
.LLCFI561:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+72], %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2953
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2952
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2956
	 nop
.LL2952:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2955
	 nop
.LL2953:
	cmp	%g1, 0
	bg	%icc, .LL2954
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2956
	 nop
.LL2954:
	add	%g1, -1, %g1
.LL2955:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2956:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE561:
	.size	Mod__Mod_param_u32_C, .-Mod__Mod_param_u32_C
	.align 4
	.type	Mod__uMod_var_u32_u8, #function
	.proc	04
Mod__uMod_var_u32_u8:
.LLFB562:
	save	%sp, -96, %sp
.LLCFI562:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+112], %o5
	srl	%o5, 24, %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE562:
	.size	Mod__uMod_var_u32_u8, .-Mod__uMod_var_u32_u8
	.align 4
	.type	Mod__Mod_var_u32_u8, #function
	.proc	04
Mod__Mod_var_u32_u8:
.LLFB563:
	save	%sp, -96, %sp
.LLCFI563:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+112], %g1
	srl	%g1, 24, %g1
	and	%g1, 0xff, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2960
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2959
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2963
	 nop
.LL2959:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2962
	 nop
.LL2960:
	cmp	%g1, 0
	bg	%icc, .LL2961
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2963
	 nop
.LL2961:
	add	%g1, -1, %g1
.LL2962:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2963:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE563:
	.size	Mod__Mod_var_u32_u8, .-Mod__Mod_var_u32_u8
	.align 4
	.type	Mod__uMod_param_u32_u8, #function
	.proc	04
Mod__uMod_param_u32_u8:
.LLFB564:
	save	%sp, -96, %sp
.LLCFI564:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	and	%o5, 0xff, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE564:
	.size	Mod__uMod_param_u32_u8, .-Mod__uMod_param_u32_u8
	.align 4
	.type	Mod__Mod_param_u32_u8, #function
	.proc	04
Mod__Mod_param_u32_u8:
.LLFB565:
	save	%sp, -96, %sp
.LLCFI565:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	and	%g1, 0xff, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2967
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2966
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2970
	 nop
.LL2966:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2969
	 nop
.LL2967:
	cmp	%g1, 0
	bg	%icc, .LL2968
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2970
	 nop
.LL2968:
	add	%g1, -1, %g1
.LL2969:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2970:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE565:
	.size	Mod__Mod_param_u32_u8, .-Mod__Mod_param_u32_u8
	.align 4
	.type	Mod__uMod_var_u32_L, #function
	.proc	05
Mod__uMod_var_u32_L:
.LLFB566:
	save	%sp, -96, %sp
.LLCFI566:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %o5
	sra	%g1, 31, %g1
	mov	%g1, %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE566:
	.size	Mod__uMod_var_u32_L, .-Mod__uMod_var_u32_L
	.align 4
	.type	Mod__Mod_var_u32_L, #function
	.proc	05
Mod__Mod_var_u32_L:
.LLFB567:
	save	%sp, -96, %sp
.LLCFI567:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	mov	%g1, %l3
	sra	%g1, 31, %g1
	mov	%g1, %l2
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ldd	[%g1+120], %l0
	mov	%l2, %o4
	mov	%l3, %o5
	cmp	%l0, 0
	bl	%icc, .LL2974
	 nop
	cmp	%l0, 0
	bne	%icc, .LL2978
	 nop
	cmp	%l1, 0
.LL2978:
	cmp	%o4, 0
	bl	%icc, .LL2973
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2979
	 nop
	cmp	%o5, 0
.LL2979:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2977
	 nop
.LL2973:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2976
	 nop
.LL2974:
	cmp	%o4, 0
	bg	%icc, .LL2975
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2980
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2975
	 nop
	cmp	%o5, 0
.LL2980:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2977
	 nop
.LL2975:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2976:
	mov	%o4, %o0
	mov	%o5, %o1
	mov	%l0, %o2
	mov	%l1, %o3
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2977:
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	srl	%l1, 0, %l1
	or	%o5, %o4, %o4
	sllx	%l0, 32, %o5
	or	%l1, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l3, %o5, %o5
	subx	%l2, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE567:
	.size	Mod__Mod_var_u32_L, .-Mod__Mod_var_u32_L
	.align 4
	.type	Mod__uMod_param_u32_L, #function
	.proc	05
Mod__uMod_param_u32_L:
.LLFB568:
	save	%sp, -104, %sp
.LLCFI568:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %o5
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %o4
	ldd	[%fp-8], %o2
	mov	%o4, %o0
	mov	%o5, %o1
	call	__umoddi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE568:
	.size	Mod__uMod_param_u32_L, .-Mod__uMod_param_u32_L
	.align 4
	.type	Mod__Mod_param_u32_L, #function
	.proc	05
Mod__Mod_param_u32_L:
.LLFB569:
	save	%sp, -104, %sp
.LLCFI569:
	st	%i0, [%fp+68]
	st	%i1, [%fp-8]
	st	%i2, [%fp-4]
	ld	[%fp+68], %l1
	ld	[%fp+68], %g1
	sra	%g1, 31, %g1
	mov	%g1, %l0
	mov	%l0, %o4
	mov	%l1, %o5
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bl	%icc, .LL2984
	 nop
	ld	[%fp-8], %g1
	cmp	%g1, 0
	bne	%icc, .LL2988
	 nop
	ld	[%fp-4], %g1
	cmp	%g1, 0
.LL2988:
	cmp	%o4, 0
	bl	%icc, .LL2983
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2989
	 nop
	cmp	%o5, 0
.LL2989:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2987
	 nop
.LL2983:
	addcc	%o5, 1, %o5
	addx	%o4, 0, %o4
	ba,pt	%xcc, .LL2986
	 nop
.LL2984:
	cmp	%o4, 0
	bg	%icc, .LL2985
	 nop
	cmp	%o4, 0
	bne	%icc, .LL2990
	 nop
	cmp	%o5, 0
	bne	%icc, .LL2985
	 nop
	cmp	%o5, 0
.LL2990:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	ba,pt	%xcc, .LL2987
	 nop
.LL2985:
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2986:
	mov	%o4, %o0
	mov	%o5, %o1
	ldd	[%fp-8], %o2
	call	__divdi3, 0
	 nop
	mov	%o0, %o4
	mov	%o1, %o5
	addcc	%o5, -1, %o5
	addx	%o4, -1, %o4
.LL2987:
	ldd	[%fp-8], %o2
	srl	%o5, 0, %o5
	sllx	%o4, 32, %o4
	or	%o5, %o4, %o4
	sllx	%o2, 32, %o5
	or	%o3, %o5, %o5
	mulx	%o4, %o5, %o5
	srlx	%o5, 32, %o4
	subcc	%l1, %o5, %o5
	subx	%l0, %o4, %o4
	mov	%o4, %i0
	mov	%o5, %i1
	return	%i7+8
	 nop
.LLFE569:
	.size	Mod__Mod_param_u32_L, .-Mod__Mod_param_u32_L
	.align 4
	.type	Mod__uMod_var_u32_i8, #function
	.proc	04
Mod__uMod_var_u32_i8:
.LLFB570:
	save	%sp, -96, %sp
.LLCFI570:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+128], %o5
	sra	%o5, 24, %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE570:
	.size	Mod__uMod_var_u32_i8, .-Mod__uMod_var_u32_i8
	.align 4
	.type	Mod__Mod_var_u32_i8, #function
	.proc	04
Mod__Mod_var_u32_i8:
.LLFB571:
	save	%sp, -96, %sp
.LLCFI571:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+128], %g1
	sra	%g1, 24, %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL2994
	 nop
	cmp	%g1, 0
	bl	%icc, .LL2993
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2997
	 nop
.LL2993:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL2996
	 nop
.LL2994:
	cmp	%g1, 0
	bg	%icc, .LL2995
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL2997
	 nop
.LL2995:
	add	%g1, -1, %g1
.LL2996:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL2997:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE571:
	.size	Mod__Mod_var_u32_i8, .-Mod__Mod_var_u32_i8
	.align 4
	.type	Mod__uMod_param_u32_i8, #function
	.proc	04
Mod__uMod_param_u32_i8:
.LLFB572:
	save	%sp, -96, %sp
.LLCFI572:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ldub	[%fp+72], %o5
	sll	%o5, 24, %o5
	sra	%o5, 24, %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE572:
	.size	Mod__uMod_param_u32_i8, .-Mod__uMod_param_u32_i8
	.align 4
	.type	Mod__Mod_param_u32_i8, #function
	.proc	04
Mod__Mod_param_u32_i8:
.LLFB573:
	save	%sp, -96, %sp
.LLCFI573:
	st	%i0, [%fp+68]
	stb	%i1, [%fp+72]
	ldub	[%fp+72], %g1
	sll	%g1, 24, %g1
	sra	%g1, 24, %o5
	ld	[%fp+68], %o4
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL3001
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3000
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3004
	 nop
.LL3000:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3003
	 nop
.LL3001:
	cmp	%g1, 0
	bg	%icc, .LL3002
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3004
	 nop
.LL3002:
	add	%g1, -1, %g1
.LL3003:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3004:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE573:
	.size	Mod__Mod_param_u32_i8, .-Mod__Mod_param_u32_i8
	.align 4
	.type	Mod__uMod_var_u32_u32, #function
	.proc	04
Mod__uMod_var_u32_u32:
.LLFB574:
	save	%sp, -96, %sp
.LLCFI574:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %g1
	sethi	%hi(MM_Mod), %o5
	or	%o5, %lo(MM_Mod), %o5
	ld	[%o5+132], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE574:
	.size	Mod__uMod_var_u32_u32, .-Mod__uMod_var_u32_u32
	.align 4
	.type	Mod__Mod_var_u32_u32, #function
	.proc	04
Mod__Mod_var_u32_u32:
.LLFB575:
	save	%sp, -96, %sp
.LLCFI575:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o4
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	ld	[%g1+132], %o5
	mov	%o4, %g1
	cmp	%o5, 0
	bl	%icc, .LL3008
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3007
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3011
	 nop
.LL3007:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3010
	 nop
.LL3008:
	cmp	%g1, 0
	bg	%icc, .LL3009
	 nop
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	ba,pt	%xcc, .LL3011
	 nop
.LL3009:
	add	%g1, -1, %g1
.LL3010:
	sra	%g1, 31, %o3
	wr	%o3, 0, %y
	sdiv	%g1, %o5, %g1
	add	%g1, -1, %g1
.LL3011:
	smul	%g1, %o5, %g1
	sub	%o4, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE575:
	.size	Mod__Mod_var_u32_u32, .-Mod__Mod_var_u32_u32
	.align 4
	.type	Mod__uMod_param_u32_u32, #function
	.proc	04
Mod__uMod_param_u32_u32:
.LLFB576:
	save	%sp, -96, %sp
.LLCFI576:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %g1
	ld	[%fp+72], %o5
	wr	%g0, 0, %y
	udiv	%g1, %o5, %o4
	smul	%o4, %o5, %o5
	sub	%g1, %o5, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE576:
	.size	Mod__uMod_param_u32_u32, .-Mod__uMod_param_u32_u32
	.align 4
	.type	Mod__Mod_param_u32_u32, #function
	.proc	04
Mod__Mod_param_u32_u32:
.LLFB577:
	save	%sp, -96, %sp
.LLCFI577:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %o5
	mov	%o5, %g1
	ld	[%fp+72], %o4
	cmp	%o4, 0
	bl	%icc, .LL3015
	 nop
	cmp	%g1, 0
	bl	%icc, .LL3014
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL3018
	 nop
.LL3014:
	add	%g1, 1, %g1
	ba,pt	%xcc, .LL3017
	 nop
.LL3015:
	cmp	%g1, 0
	bg	%icc, .LL3016
	 nop
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	ba,pt	%xcc, .LL3018
	 nop
.LL3016:
	add	%g1, -1, %g1
.LL3017:
	sra	%g1, 31, %o4
	wr	%o4, 0, %y
	ld	[%fp+72], %o4
	sdiv	%g1, %o4, %g1
	add	%g1, -1, %g1
.LL3018:
	ld	[%fp+72], %o4
	smul	%g1, %o4, %g1
	sub	%o5, %g1, %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE577:
	.size	Mod__Mod_param_u32_u32, .-Mod__Mod_param_u32_u32
	.align 4
	.type	Mod__Mod_var_f32_f32, #function
	.proc	06
Mod__Mod_var_f32_f32:
.LLFB578:
	save	%sp, -120, %sp
.LLCFI578:
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	st	%f8, [%fp-16]
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	add	%g1, 136, %g1
	ld	[%g1], %f8
	st	%f8, [%fp-12]
	ld	[%fp-16], %f9
	ld	[%fp-12], %f8
	fdivs	%f9, %f8, %f8
	st	%f8, [%fp-4]
	ld	[%fp-4], %f8
	fstoi	%f8, %f8
	st	%f8, [%fp-8]
	ld	[%fp-16], %f9
	ld	[%fp-8], %f8
	fitos	%f8, %f10
	ld	[%fp-4], %f8
	fcmpes	%fcc2, %f10, %f8
	mov	0, %g1
	movle	%fcc2, 1, %g1
	and	%g1, 0xff, %g1
	cmp	%g1, 0
	be	%icc, .LL3020
	 nop
	ld	[%fp-8], %f8
	fitos	%f8, %f8
	ba,pt	%xcc, .LL3021
	 nop
.LL3020:
	ld	[%fp-8], %g1
	add	%g1, -1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %f10
	fitos	%f10, %f8
.LL3021:
	ld	[%fp-12], %f10
	fmuls	%f8, %f10, %f8
	fsubs	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE578:
	.size	Mod__Mod_var_f32_f32, .-Mod__Mod_var_f32_f32
	.align 4
	.type	Mod__Mod_param_f32_f32, #function
	.proc	06
Mod__Mod_param_f32_f32:
.LLFB579:
	save	%sp, -120, %sp
.LLCFI579:
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	ld	[%fp+68], %f8
	st	%f8, [%fp-16]
	ld	[%fp+72], %f8
	st	%f8, [%fp-12]
	ld	[%fp-16], %f9
	ld	[%fp-12], %f8
	fdivs	%f9, %f8, %f8
	st	%f8, [%fp-4]
	ld	[%fp-4], %f8
	fstoi	%f8, %f8
	st	%f8, [%fp-8]
	ld	[%fp-16], %f9
	ld	[%fp-8], %f8
	fitos	%f8, %f10
	ld	[%fp-4], %f8
	fcmpes	%fcc3, %f10, %f8
	mov	0, %g1
	movle	%fcc3, 1, %g1
	and	%g1, 0xff, %g1
	cmp	%g1, 0
	be	%icc, .LL3023
	 nop
	ld	[%fp-8], %f8
	fitos	%f8, %f8
	ba,pt	%xcc, .LL3024
	 nop
.LL3023:
	ld	[%fp-8], %g1
	add	%g1, -1, %g1
	st	%g1, [%fp-20]
	ld	[%fp-20], %f10
	fitos	%f10, %f8
.LL3024:
	ld	[%fp-12], %f10
	fmuls	%f8, %f10, %f8
	fsubs	%f9, %f8, %f8
	fmovs	%f8, %f0
	return	%i7+8
	 nop
.LLFE579:
	.size	Mod__Mod_param_f32_f32, .-Mod__Mod_param_f32_f32
	.align 4
	.global Mod_M3
	.type	Mod_M3, #function
	.proc	0120
Mod_M3:
.LLFB580:
	save	%sp, -96, %sp
.LLCFI580:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %g1
	mov	%g1, %i0
	return	%i7+8
	 nop
.LLFE580:
	.size	Mod_M3, .-Mod_M3
	.align 4
	.type	_m3_fault, #function
	.proc	020
_m3_fault:
.LLFB581:
	save	%sp, -96, %sp
.LLCFI581:
	st	%i0, [%fp+68]
	sethi	%hi(MM_Mod), %g1
	or	%g1, %lo(MM_Mod), %o0
	ld	[%fp+68], %o1
	call	RTHooks__ReportFault, 0
	 nop
	return	%i7+8
	 nop
.LLFE581:
	.size	_m3_fault, .-_m3_fault
	.section	".data"
	.align 8
	.type	L_1, #object
	.size	L_1, 13968
L_1:
	.ascii	"Mod_M3"
	.skip 1
	.ascii	"Mod_param_f32_f32"
	.skip 1
	.ascii	"Mod_var_f32_f32"
	.skip 1
	.ascii	"Mod_param_u32_u32"
	.skip 1
	.ascii	"uMod_param_u32_u32"
	.skip 1
	.ascii	"Mod_var_u32_u32"
	.skip 1
	.ascii	"uMod_var_u32_u32"
	.skip 1
	.ascii	"Mod_param_u32_i8"
	.skip 1
	.ascii	"uMod_param_u32_i8"
	.skip 1
	.ascii	"Mod_var_u32_i8"
	.skip 1
	.ascii	"uMod_var_u32_i8"
	.skip 1
	.ascii	"Mod_param_u32_L"
	.skip 1
	.ascii	"uMod_param_u32_L"
	.skip 1
	.ascii	"Mod_var_u32_L"
	.skip 1
	.ascii	"uMod_var_u32_L"
	.skip 1
	.ascii	"Mod_param_u32_u8"
	.skip 1
	.ascii	"uMod_param_u32_u8"
	.skip 1
	.ascii	"Mod_var_u32_u8"
	.skip 1
	.ascii	"uMod_var_u32_u8"
	.skip 1
	.ascii	"Mod_param_u32_C"
	.skip 1
	.ascii	"uMod_param_u32_C"
	.skip 1
	.ascii	"Mod_var_u32_C"
	.skip 1
	.ascii	"uMod_var_u32_C"
	.skip 1
	.ascii	"Mod_param_u32_i64"
	.skip 1
	.ascii	"uMod_param_u32_i64"
	.skip 1
	.ascii	"Mod_var_u32_i64"
	.skip 1
	.ascii	"uMod_var_u32_i64"
	.skip 1
	.ascii	"Mod_param_u32_I"
	.skip 1
	.ascii	"uMod_param_u32_I"
	.skip 1
	.ascii	"Mod_var_u32_I"
	.skip 1
	.ascii	"uMod_var_u32_I"
	.skip 1
	.ascii	"Mod_param_u32_i16"
	.skip 1
	.ascii	"uMod_param_u32_i16"
	.skip 1
	.ascii	"Mod_var_u32_i16"
	.skip 1
	.ascii	"uMod_var_u32_i16"
	.skip 1
	.ascii	"Mod_param_u32_i32"
	.skip 1
	.ascii	"uMod_param_u32_i32"
	.skip 1
	.ascii	"Mod_var_u32_i32"
	.skip 1
	.ascii	"uMod_var_u32_i32"
	.skip 1
	.ascii	"Mod_param_u32_LC"
	.skip 1
	.ascii	"uMod_param_u32_LC"
	.skip 1
	.ascii	"Mod_var_u32_LC"
	.skip 1
	.ascii	"uMod_var_u32_LC"
	.skip 1
	.ascii	"Mod_param_u32_u64"
	.skip 1
	.ascii	"uMod_param_u32_u64"
	.skip 1
	.ascii	"Mod_var_u32_u64"
	.skip 1
	.ascii	"uMod_var_u32_u64"
	.skip 1
	.ascii	"Mod_param_u32_u16"
	.skip 1
	.ascii	"uMod_param_u32_u16"
	.skip 1
	.ascii	"Mod_var_u32_u16"
	.skip 1
	.ascii	"uMod_var_u32_u16"
	.skip 1
	.ascii	"Mod_param_i8_u32"
	.skip 1
	.ascii	"uMod_param_i8_u32"
	.skip 1
	.ascii	"Mod_var_i8_u32"
	.skip 1
	.ascii	"uMod_var_i8_u32"
	.skip 1
	.ascii	"Mod_param_i8_i8"
	.skip 1
	.ascii	"uMod_param_i8_i8"
	.skip 1
	.ascii	"Mod_var_i8_i8"
	.skip 1
	.ascii	"uMod_var_i8_i8"
	.skip 1
	.ascii	"Mod_param_i8_L"
	.skip 1
	.ascii	"uMod_param_i8_L"
	.skip 1
	.ascii	"Mod_var_i8_L"
	.skip 1
	.ascii	"uMod_var_i8_L"
	.skip 1
	.ascii	"Mod_param_i8_u8"
	.skip 1
	.ascii	"uMod_param_i8_u8"
	.skip 1
	.ascii	"Mod_var_i8_u8"
	.skip 1
	.ascii	"uMod_var_i8_u8"
	.skip 1
	.ascii	"Mod_param_i8_C"
	.skip 1
	.ascii	"uMod_param_i8_C"
	.skip 1
	.ascii	"Mod_var_i8_C"
	.skip 1
	.ascii	"uMod_var_i8_C"
	.skip 1
	.ascii	"Mod_param_i8_i64"
	.skip 1
	.ascii	"uMod_param_i8_i64"
	.skip 1
	.ascii	"Mod_var_i8_i64"
	.skip 1
	.ascii	"uMod_var_i8_i64"
	.skip 1
	.ascii	"Mod_param_i8_I"
	.skip 1
	.ascii	"uMod_param_i8_I"
	.skip 1
	.ascii	"Mod_var_i8_I"
	.skip 1
	.ascii	"uMod_var_i8_I"
	.skip 1
	.ascii	"Mod_param_i8_i16"
	.skip 1
	.ascii	"uMod_param_i8_i16"
	.skip 1
	.ascii	"Mod_var_i8_i16"
	.skip 1
	.ascii	"uMod_var_i8_i16"
	.skip 1
	.ascii	"Mod_param_i8_i32"
	.skip 1
	.ascii	"uMod_param_i8_i32"
	.skip 1
	.ascii	"Mod_var_i8_i32"
	.skip 1
	.ascii	"uMod_var_i8_i32"
	.skip 1
	.ascii	"Mod_param_i8_LC"
	.skip 1
	.ascii	"uMod_param_i8_LC"
	.skip 1
	.ascii	"Mod_var_i8_LC"
	.skip 1
	.ascii	"uMod_var_i8_LC"
	.skip 1
	.ascii	"Mod_param_i8_u64"
	.skip 1
	.ascii	"uMod_param_i8_u64"
	.skip 1
	.ascii	"Mod_var_i8_u64"
	.skip 1
	.ascii	"uMod_var_i8_u64"
	.skip 1
	.ascii	"Mod_param_i8_u16"
	.skip 1
	.ascii	"uMod_param_i8_u16"
	.skip 1
	.ascii	"Mod_var_i8_u16"
	.skip 1
	.ascii	"uMod_var_i8_u16"
	.skip 1
	.ascii	"Mod_param_L_u32"
	.skip 1
	.ascii	"uMod_param_L_u32"
	.skip 1
	.ascii	"Mod_var_L_u32"
	.skip 1
	.ascii	"uMod_var_L_u32"
	.skip 1
	.ascii	"Mod_param_L_i8"
	.skip 1
	.ascii	"uMod_param_L_i8"
	.skip 1
	.ascii	"Mod_var_L_i8"
	.skip 1
	.ascii	"uMod_var_L_i8"
	.skip 1
	.ascii	"Mod_param_L_L"
	.skip 1
	.ascii	"uMod_param_L_L"
	.skip 1
	.ascii	"Mod_var_L_L"
	.skip 1
	.ascii	"uMod_var_L_L"
	.skip 1
	.ascii	"Mod_param_L_u8"
	.skip 1
	.ascii	"uMod_param_L_u8"
	.skip 1
	.ascii	"Mod_var_L_u8"
	.skip 1
	.ascii	"uMod_var_L_u8"
	.skip 1
	.ascii	"Mod_param_L_C"
	.skip 1
	.ascii	"uMod_param_L_C"
	.skip 1
	.ascii	"Mod_var_L_C"
	.skip 1
	.ascii	"uMod_var_L_C"
	.skip 1
	.ascii	"Mod_param_L_i64"
	.skip 1
	.ascii	"uMod_param_L_i64"
	.skip 1
	.ascii	"Mod_var_L_i64"
	.skip 1
	.ascii	"uMod_var_L_i64"
	.skip 1
	.ascii	"Mod_param_L_I"
	.skip 1
	.ascii	"uMod_param_L_I"
	.skip 1
	.ascii	"Mod_var_L_I"
	.skip 1
	.ascii	"uMod_var_L_I"
	.skip 1
	.ascii	"Mod_param_L_i16"
	.skip 1
	.ascii	"uMod_param_L_i16"
	.skip 1
	.ascii	"Mod_var_L_i16"
	.skip 1
	.ascii	"uMod_var_L_i16"
	.skip 1
	.ascii	"Mod_param_L_i32"
	.skip 1
	.ascii	"uMod_param_L_i32"
	.skip 1
	.ascii	"Mod_var_L_i32"
	.skip 1
	.ascii	"uMod_var_L_i32"
	.skip 1
	.ascii	"Mod_param_L_LC"
	.skip 1
	.ascii	"uMod_param_L_LC"
	.skip 1
	.ascii	"Mod_var_L_LC"
	.skip 1
	.ascii	"uMod_var_L_LC"
	.skip 1
	.ascii	"Mod_param_L_u64"
	.skip 1
	.ascii	"uMod_param_L_u64"
	.skip 1
	.ascii	"Mod_var_L_u64"
	.skip 1
	.ascii	"uMod_var_L_u64"
	.skip 1
	.ascii	"Mod_param_L_u16"
	.skip 1
	.ascii	"uMod_param_L_u16"
	.skip 1
	.ascii	"Mod_var_L_u16"
	.skip 1
	.ascii	"uMod_var_L_u16"
	.skip 1
	.ascii	"Mod_param_u8_u32"
	.skip 1
	.ascii	"uMod_param_u8_u32"
	.skip 1
	.ascii	"Mod_var_u8_u32"
	.skip 1
	.ascii	"uMod_var_u8_u32"
	.skip 1
	.ascii	"Mod_param_u8_i8"
	.skip 1
	.ascii	"uMod_param_u8_i8"
	.skip 1
	.ascii	"Mod_var_u8_i8"
	.skip 1
	.ascii	"uMod_var_u8_i8"
	.skip 1
	.ascii	"Mod_param_u8_L"
	.skip 1
	.ascii	"uMod_param_u8_L"
	.skip 1
	.ascii	"Mod_var_u8_L"
	.skip 1
	.ascii	"uMod_var_u8_L"
	.skip 1
	.ascii	"Mod_param_u8_u8"
	.skip 1
	.ascii	"uMod_param_u8_u8"
	.skip 1
	.ascii	"Mod_var_u8_u8"
	.skip 1
	.ascii	"uMod_var_u8_u8"
	.skip 1
	.ascii	"Mod_param_u8_C"
	.skip 1
	.ascii	"uMod_param_u8_C"
	.skip 1
	.ascii	"Mod_var_u8_C"
	.skip 1
	.ascii	"uMod_var_u8_C"
	.skip 1
	.ascii	"Mod_param_u8_i64"
	.skip 1
	.ascii	"uMod_param_u8_i64"
	.skip 1
	.ascii	"Mod_var_u8_i64"
	.skip 1
	.ascii	"uMod_var_u8_i64"
	.skip 1
	.ascii	"Mod_param_u8_I"
	.skip 1
	.ascii	"uMod_param_u8_I"
	.skip 1
	.ascii	"Mod_var_u8_I"
	.skip 1
	.ascii	"uMod_var_u8_I"
	.skip 1
	.ascii	"Mod_param_u8_i16"
	.skip 1
	.ascii	"uMod_param_u8_i16"
	.skip 1
	.ascii	"Mod_var_u8_i16"
	.skip 1
	.ascii	"uMod_var_u8_i16"
	.skip 1
	.ascii	"Mod_param_u8_i32"
	.skip 1
	.ascii	"uMod_param_u8_i32"
	.skip 1
	.ascii	"Mod_var_u8_i32"
	.skip 1
	.ascii	"uMod_var_u8_i32"
	.skip 1
	.ascii	"Mod_param_u8_LC"
	.skip 1
	.ascii	"uMod_param_u8_LC"
	.skip 1
	.ascii	"Mod_var_u8_LC"
	.skip 1
	.ascii	"uMod_var_u8_LC"
	.skip 1
	.ascii	"Mod_param_u8_u64"
	.skip 1
	.ascii	"uMod_param_u8_u64"
	.skip 1
	.ascii	"Mod_var_u8_u64"
	.skip 1
	.ascii	"uMod_var_u8_u64"
	.skip 1
	.ascii	"Mod_param_u8_u16"
	.skip 1
	.ascii	"uMod_param_u8_u16"
	.skip 1
	.ascii	"Mod_var_u8_u16"
	.skip 1
	.ascii	"uMod_var_u8_u16"
	.skip 1
	.ascii	"Mod_param_f64_f64"
	.skip 1
	.ascii	"Mod_var_f64_f64"
	.skip 1
	.ascii	"Mod_param_C_u32"
	.skip 1
	.ascii	"uMod_param_C_u32"
	.skip 1
	.ascii	"Mod_var_C_u32"
	.skip 1
	.ascii	"uMod_var_C_u32"
	.skip 1
	.ascii	"Mod_param_C_i8"
	.skip 1
	.ascii	"uMod_param_C_i8"
	.skip 1
	.ascii	"Mod_var_C_i8"
	.skip 1
	.ascii	"uMod_var_C_i8"
	.skip 1
	.ascii	"Mod_param_C_L"
	.skip 1
	.ascii	"uMod_param_C_L"
	.skip 1
	.ascii	"Mod_var_C_L"
	.skip 1
	.ascii	"uMod_var_C_L"
	.skip 1
	.ascii	"Mod_param_C_u8"
	.skip 1
	.ascii	"uMod_param_C_u8"
	.skip 1
	.ascii	"Mod_var_C_u8"
	.skip 1
	.ascii	"uMod_var_C_u8"
	.skip 1
	.ascii	"Mod_param_C_C"
	.skip 1
	.ascii	"uMod_param_C_C"
	.skip 1
	.ascii	"Mod_var_C_C"
	.skip 1
	.ascii	"uMod_var_C_C"
	.skip 1
	.ascii	"Mod_param_C_i64"
	.skip 1
	.ascii	"uMod_param_C_i64"
	.skip 1
	.ascii	"Mod_var_C_i64"
	.skip 1
	.ascii	"uMod_var_C_i64"
	.skip 1
	.ascii	"Mod_param_C_I"
	.skip 1
	.ascii	"uMod_param_C_I"
	.skip 1
	.ascii	"Mod_var_C_I"
	.skip 1
	.ascii	"uMod_var_C_I"
	.skip 1
	.ascii	"Mod_param_C_i16"
	.skip 1
	.ascii	"uMod_param_C_i16"
	.skip 1
	.ascii	"Mod_var_C_i16"
	.skip 1
	.ascii	"uMod_var_C_i16"
	.skip 1
	.ascii	"Mod_param_C_i32"
	.skip 1
	.ascii	"uMod_param_C_i32"
	.skip 1
	.ascii	"Mod_var_C_i32"
	.skip 1
	.ascii	"uMod_var_C_i32"
	.skip 1
	.ascii	"Mod_param_C_LC"
	.skip 1
	.ascii	"uMod_param_C_LC"
	.skip 1
	.ascii	"Mod_var_C_LC"
	.skip 1
	.ascii	"uMod_var_C_LC"
	.skip 1
	.ascii	"Mod_param_C_u64"
	.skip 1
	.ascii	"uMod_param_C_u64"
	.skip 1
	.ascii	"Mod_var_C_u64"
	.skip 1
	.ascii	"uMod_var_C_u64"
	.skip 1
	.ascii	"Mod_param_C_u16"
	.skip 1
	.ascii	"uMod_param_C_u16"
	.skip 1
	.ascii	"Mod_var_C_u16"
	.skip 1
	.ascii	"uMod_var_C_u16"
	.skip 1
	.ascii	"Mod_param_i64_u32"
	.skip 1
	.ascii	"uMod_param_i64_u32"
	.skip 1
	.ascii	"Mod_var_i64_u32"
	.skip 1
	.ascii	"uMod_var_i64_u32"
	.skip 1
	.ascii	"Mod_param_i64_i8"
	.skip 1
	.ascii	"uMod_param_i64_i8"
	.skip 1
	.ascii	"Mod_var_i64_i8"
	.skip 1
	.ascii	"uMod_var_i64_i8"
	.skip 1
	.ascii	"Mod_param_i64_L"
	.skip 1
	.ascii	"uMod_param_i64_L"
	.skip 1
	.ascii	"Mod_var_i64_L"
	.skip 1
	.ascii	"uMod_var_i64_L"
	.skip 1
	.ascii	"Mod_param_i64_u8"
	.skip 1
	.ascii	"uMod_param_i64_u8"
	.skip 1
	.ascii	"Mod_var_i64_u8"
	.skip 1
	.ascii	"uMod_var_i64_u8"
	.skip 1
	.ascii	"Mod_param_i64_C"
	.skip 1
	.ascii	"uMod_param_i64_C"
	.skip 1
	.ascii	"Mod_var_i64_C"
	.skip 1
	.ascii	"uMod_var_i64_C"
	.skip 1
	.ascii	"Mod_param_i64_i64"
	.skip 1
	.ascii	"uMod_param_i64_i64"
	.skip 1
	.ascii	"Mod_var_i64_i64"
	.skip 1
	.ascii	"uMod_var_i64_i64"
	.skip 1
	.ascii	"Mod_param_i64_I"
	.skip 1
	.ascii	"uMod_param_i64_I"
	.skip 1
	.ascii	"Mod_var_i64_I"
	.skip 1
	.ascii	"uMod_var_i64_I"
	.skip 1
	.ascii	"Mod_param_i64_i16"
	.skip 1
	.ascii	"uMod_param_i64_i16"
	.skip 1
	.ascii	"Mod_var_i64_i16"
	.skip 1
	.ascii	"uMod_var_i64_i16"
	.skip 1
	.ascii	"Mod_param_i64_i32"
	.skip 1
	.ascii	"uMod_param_i64_i32"
	.skip 1
	.ascii	"Mod_var_i64_i32"
	.skip 1
	.ascii	"uMod_var_i64_i32"
	.skip 1
	.ascii	"Mod_param_i64_LC"
	.skip 1
	.ascii	"uMod_param_i64_LC"
	.skip 1
	.ascii	"Mod_var_i64_LC"
	.skip 1
	.ascii	"uMod_var_i64_LC"
	.skip 1
	.ascii	"Mod_param_i64_u64"
	.skip 1
	.ascii	"uMod_param_i64_u64"
	.skip 1
	.ascii	"Mod_var_i64_u64"
	.skip 1
	.ascii	"uMod_var_i64_u64"
	.skip 1
	.ascii	"Mod_param_i64_u16"
	.skip 1
	.ascii	"uMod_param_i64_u16"
	.skip 1
	.ascii	"Mod_var_i64_u16"
	.skip 1
	.ascii	"uMod_var_i64_u16"
	.skip 1
	.ascii	"Mod_param_I_u32"
	.skip 1
	.ascii	"uMod_param_I_u32"
	.skip 1
	.ascii	"Mod_var_I_u32"
	.skip 1
	.ascii	"uMod_var_I_u32"
	.skip 1
	.ascii	"Mod_param_I_i8"
	.skip 1
	.ascii	"uMod_param_I_i8"
	.skip 1
	.ascii	"Mod_var_I_i8"
	.skip 1
	.ascii	"uMod_var_I_i8"
	.skip 1
	.ascii	"Mod_param_I_L"
	.skip 1
	.ascii	"uMod_param_I_L"
	.skip 1
	.ascii	"Mod_var_I_L"
	.skip 1
	.ascii	"uMod_var_I_L"
	.skip 1
	.ascii	"Mod_param_I_u8"
	.skip 1
	.ascii	"uMod_param_I_u8"
	.skip 1
	.ascii	"Mod_var_I_u8"
	.skip 1
	.ascii	"uMod_var_I_u8"
	.skip 1
	.ascii	"Mod_param_I_C"
	.skip 1
	.ascii	"uMod_param_I_C"
	.skip 1
	.ascii	"Mod_var_I_C"
	.skip 1
	.ascii	"uMod_var_I_C"
	.skip 1
	.ascii	"Mod_param_I_i64"
	.skip 1
	.ascii	"uMod_param_I_i64"
	.skip 1
	.ascii	"Mod_var_I_i64"
	.skip 1
	.ascii	"uMod_var_I_i64"
	.skip 1
	.ascii	"Mod_param_I_I"
	.skip 1
	.ascii	"uMod_param_I_I"
	.skip 1
	.ascii	"Mod_var_I_I"
	.skip 1
	.ascii	"uMod_var_I_I"
	.skip 1
	.ascii	"Mod_param_I_i16"
	.skip 1
	.ascii	"uMod_param_I_i16"
	.skip 1
	.ascii	"Mod_var_I_i16"
	.skip 1
	.ascii	"uMod_var_I_i16"
	.skip 1
	.ascii	"Mod_param_I_i32"
	.skip 1
	.ascii	"uMod_param_I_i32"
	.skip 1
	.ascii	"Mod_var_I_i32"
	.skip 1
	.ascii	"uMod_var_I_i32"
	.skip 1
	.ascii	"Mod_param_I_LC"
	.skip 1
	.ascii	"uMod_param_I_LC"
	.skip 1
	.ascii	"Mod_var_I_LC"
	.skip 1
	.ascii	"uMod_var_I_LC"
	.skip 1
	.ascii	"Mod_param_I_u64"
	.skip 1
	.ascii	"uMod_param_I_u64"
	.skip 1
	.ascii	"Mod_var_I_u64"
	.skip 1
	.ascii	"uMod_var_I_u64"
	.skip 1
	.ascii	"Mod_param_I_u16"
	.skip 1
	.ascii	"uMod_param_I_u16"
	.skip 1
	.ascii	"Mod_var_I_u16"
	.skip 1
	.ascii	"uMod_var_I_u16"
	.skip 1
	.ascii	"Mod_param_i16_u32"
	.skip 1
	.ascii	"uMod_param_i16_u32"
	.skip 1
	.ascii	"Mod_var_i16_u32"
	.skip 1
	.ascii	"uMod_var_i16_u32"
	.skip 1
	.ascii	"Mod_param_i16_i8"
	.skip 1
	.ascii	"uMod_param_i16_i8"
	.skip 1
	.ascii	"Mod_var_i16_i8"
	.skip 1
	.ascii	"uMod_var_i16_i8"
	.skip 1
	.ascii	"Mod_param_i16_L"
	.skip 1
	.ascii	"uMod_param_i16_L"
	.skip 1
	.ascii	"Mod_var_i16_L"
	.skip 1
	.ascii	"uMod_var_i16_L"
	.skip 1
	.ascii	"Mod_param_i16_u8"
	.skip 1
	.ascii	"uMod_param_i16_u8"
	.skip 1
	.ascii	"Mod_var_i16_u8"
	.skip 1
	.ascii	"uMod_var_i16_u8"
	.skip 1
	.ascii	"Mod_param_i16_C"
	.skip 1
	.ascii	"uMod_param_i16_C"
	.skip 1
	.ascii	"Mod_var_i16_C"
	.skip 1
	.ascii	"uMod_var_i16_C"
	.skip 1
	.ascii	"Mod_param_i16_i64"
	.skip 1
	.ascii	"uMod_param_i16_i64"
	.skip 1
	.ascii	"Mod_var_i16_i64"
	.skip 1
	.ascii	"uMod_var_i16_i64"
	.skip 1
	.ascii	"Mod_param_i16_I"
	.skip 1
	.ascii	"uMod_param_i16_I"
	.skip 1
	.ascii	"Mod_var_i16_I"
	.skip 1
	.ascii	"uMod_var_i16_I"
	.skip 1
	.ascii	"Mod_param_i16_i16"
	.skip 1
	.ascii	"uMod_param_i16_i16"
	.skip 1
	.ascii	"Mod_var_i16_i16"
	.skip 1
	.ascii	"uMod_var_i16_i16"
	.skip 1
	.ascii	"Mod_param_i16_i32"
	.skip 1
	.ascii	"uMod_param_i16_i32"
	.skip 1
	.ascii	"Mod_var_i16_i32"
	.skip 1
	.ascii	"uMod_var_i16_i32"
	.skip 1
	.ascii	"Mod_param_i16_LC"
	.skip 1
	.ascii	"uMod_param_i16_LC"
	.skip 1
	.ascii	"Mod_var_i16_LC"
	.skip 1
	.ascii	"uMod_var_i16_LC"
	.skip 1
	.ascii	"Mod_param_i16_u64"
	.skip 1
	.ascii	"uMod_param_i16_u64"
	.skip 1
	.ascii	"Mod_var_i16_u64"
	.skip 1
	.ascii	"uMod_var_i16_u64"
	.skip 1
	.ascii	"Mod_param_i16_u16"
	.skip 1
	.ascii	"uMod_param_i16_u16"
	.skip 1
	.ascii	"Mod_var_i16_u16"
	.skip 1
	.ascii	"uMod_var_i16_u16"
	.skip 1
	.ascii	"Mod_param_i32_u32"
	.skip 1
	.ascii	"uMod_param_i32_u32"
	.skip 1
	.ascii	"Mod_var_i32_u32"
	.skip 1
	.ascii	"uMod_var_i32_u32"
	.skip 1
	.ascii	"Mod_param_i32_i8"
	.skip 1
	.ascii	"uMod_param_i32_i8"
	.skip 1
	.ascii	"Mod_var_i32_i8"
	.skip 1
	.ascii	"uMod_var_i32_i8"
	.skip 1
	.ascii	"Mod_param_i32_L"
	.skip 1
	.ascii	"uMod_param_i32_L"
	.skip 1
	.ascii	"Mod_var_i32_L"
	.skip 1
	.ascii	"uMod_var_i32_L"
	.skip 1
	.ascii	"Mod_param_i32_u8"
	.skip 1
	.ascii	"uMod_param_i32_u8"
	.skip 1
	.ascii	"Mod_var_i32_u8"
	.skip 1
	.ascii	"uMod_var_i32_u8"
	.skip 1
	.ascii	"Mod_param_i32_C"
	.skip 1
	.ascii	"uMod_param_i32_C"
	.skip 1
	.ascii	"Mod_var_i32_C"
	.skip 1
	.ascii	"uMod_var_i32_C"
	.skip 1
	.ascii	"Mod_param_i32_i64"
	.skip 1
	.ascii	"uMod_param_i32_i64"
	.skip 1
	.ascii	"Mod_var_i32_i64"
	.skip 1
	.ascii	"uMod_var_i32_i64"
	.skip 1
	.ascii	"Mod_param_i32_I"
	.skip 1
	.ascii	"uMod_param_i32_I"
	.skip 1
	.ascii	"Mod_var_i32_I"
	.skip 1
	.ascii	"uMod_var_i32_I"
	.skip 1
	.ascii	"Mod_param_i32_i16"
	.skip 1
	.ascii	"uMod_param_i32_i16"
	.skip 1
	.ascii	"Mod_var_i32_i16"
	.skip 1
	.ascii	"uMod_var_i32_i16"
	.skip 1
	.ascii	"Mod_param_i32_i32"
	.skip 1
	.ascii	"uMod_param_i32_i32"
	.skip 1
	.ascii	"Mod_var_i32_i32"
	.skip 1
	.ascii	"uMod_var_i32_i32"
	.skip 1
	.ascii	"Mod_param_i32_LC"
	.skip 1
	.ascii	"uMod_param_i32_LC"
	.skip 1
	.ascii	"Mod_var_i32_LC"
	.skip 1
	.ascii	"uMod_var_i32_LC"
	.skip 1
	.ascii	"Mod_param_i32_u64"
	.skip 1
	.ascii	"uMod_param_i32_u64"
	.skip 1
	.ascii	"Mod_var_i32_u64"
	.skip 1
	.ascii	"uMod_var_i32_u64"
	.skip 1
	.ascii	"Mod_param_i32_u16"
	.skip 1
	.ascii	"uMod_param_i32_u16"
	.skip 1
	.ascii	"Mod_var_i32_u16"
	.skip 1
	.ascii	"uMod_var_i32_u16"
	.skip 1
	.ascii	"Mod_param_LC_u32"
	.skip 1
	.ascii	"uMod_param_LC_u32"
	.skip 1
	.ascii	"Mod_var_LC_u32"
	.skip 1
	.ascii	"uMod_var_LC_u32"
	.skip 1
	.ascii	"Mod_param_LC_i8"
	.skip 1
	.ascii	"uMod_param_LC_i8"
	.skip 1
	.ascii	"Mod_var_LC_i8"
	.skip 1
	.ascii	"uMod_var_LC_i8"
	.skip 1
	.ascii	"Mod_param_LC_L"
	.skip 1
	.ascii	"uMod_param_LC_L"
	.skip 1
	.ascii	"Mod_var_LC_L"
	.skip 1
	.ascii	"uMod_var_LC_L"
	.skip 1
	.ascii	"Mod_param_LC_u8"
	.skip 1
	.ascii	"uMod_param_LC_u8"
	.skip 1
	.ascii	"Mod_var_LC_u8"
	.skip 1
	.ascii	"uMod_var_LC_u8"
	.skip 1
	.ascii	"Mod_param_LC_C"
	.skip 1
	.ascii	"uMod_param_LC_C"
	.skip 1
	.ascii	"Mod_var_LC_C"
	.skip 1
	.ascii	"uMod_var_LC_C"
	.skip 1
	.ascii	"Mod_param_LC_i64"
	.skip 1
	.ascii	"uMod_param_LC_i64"
	.skip 1
	.ascii	"Mod_var_LC_i64"
	.skip 1
	.ascii	"uMod_var_LC_i64"
	.skip 1
	.ascii	"Mod_param_LC_I"
	.skip 1
	.ascii	"uMod_param_LC_I"
	.skip 1
	.ascii	"Mod_var_LC_I"
	.skip 1
	.ascii	"uMod_var_LC_I"
	.skip 1
	.ascii	"Mod_param_LC_i16"
	.skip 1
	.ascii	"uMod_param_LC_i16"
	.skip 1
	.ascii	"Mod_var_LC_i16"
	.skip 1
	.ascii	"uMod_var_LC_i16"
	.skip 1
	.ascii	"Mod_param_LC_i32"
	.skip 1
	.ascii	"uMod_param_LC_i32"
	.skip 1
	.ascii	"Mod_var_LC_i32"
	.skip 1
	.ascii	"uMod_var_LC_i32"
	.skip 1
	.ascii	"Mod_param_LC_LC"
	.skip 1
	.ascii	"uMod_param_LC_LC"
	.skip 1
	.ascii	"Mod_var_LC_LC"
	.skip 1
	.ascii	"uMod_var_LC_LC"
	.skip 1
	.ascii	"Mod_param_LC_u64"
	.skip 1
	.ascii	"uMod_param_LC_u64"
	.skip 1
	.ascii	"Mod_var_LC_u64"
	.skip 1
	.ascii	"uMod_var_LC_u64"
	.skip 1
	.ascii	"Mod_param_LC_u16"
	.skip 1
	.ascii	"uMod_param_LC_u16"
	.skip 1
	.ascii	"Mod_var_LC_u16"
	.skip 1
	.ascii	"uMod_var_LC_u16"
	.skip 1
	.ascii	"Mod_param_u64_u32"
	.skip 1
	.ascii	"uMod_param_u64_u32"
	.skip 1
	.ascii	"Mod_var_u64_u32"
	.skip 1
	.ascii	"uMod_var_u64_u32"
	.skip 1
	.ascii	"Mod_param_u64_i8"
	.skip 1
	.ascii	"uMod_param_u64_i8"
	.skip 1
	.ascii	"Mod_var_u64_i8"
	.skip 1
	.ascii	"uMod_var_u64_i8"
	.skip 1
	.ascii	"Mod_param_u64_L"
	.skip 1
	.ascii	"uMod_param_u64_L"
	.skip 1
	.ascii	"Mod_var_u64_L"
	.skip 1
	.ascii	"uMod_var_u64_L"
	.skip 1
	.ascii	"Mod_param_u64_u8"
	.skip 1
	.ascii	"uMod_param_u64_u8"
	.skip 1
	.ascii	"Mod_var_u64_u8"
	.skip 1
	.ascii	"uMod_var_u64_u8"
	.skip 1
	.ascii	"Mod_param_u64_C"
	.skip 1
	.ascii	"uMod_param_u64_C"
	.skip 1
	.ascii	"Mod_var_u64_C"
	.skip 1
	.ascii	"uMod_var_u64_C"
	.skip 1
	.ascii	"Mod_param_u64_i64"
	.skip 1
	.ascii	"uMod_param_u64_i64"
	.skip 1
	.ascii	"Mod_var_u64_i64"
	.skip 1
	.ascii	"uMod_var_u64_i64"
	.skip 1
	.ascii	"Mod_param_u64_I"
	.skip 1
	.ascii	"uMod_param_u64_I"
	.skip 1
	.ascii	"Mod_var_u64_I"
	.skip 1
	.ascii	"uMod_var_u64_I"
	.skip 1
	.ascii	"Mod_param_u64_i16"
	.skip 1
	.ascii	"uMod_param_u64_i16"
	.skip 1
	.ascii	"Mod_var_u64_i16"
	.skip 1
	.ascii	"uMod_var_u64_i16"
	.skip 1
	.ascii	"Mod_param_u64_i32"
	.skip 1
	.ascii	"uMod_param_u64_i32"
	.skip 1
	.ascii	"Mod_var_u64_i32"
	.skip 1
	.ascii	"uMod_var_u64_i32"
	.skip 1
	.ascii	"Mod_param_u64_LC"
	.skip 1
	.ascii	"uMod_param_u64_LC"
	.skip 1
	.ascii	"Mod_var_u64_LC"
	.skip 1
	.ascii	"uMod_var_u64_LC"
	.skip 1
	.ascii	"Mod_param_u64_u64"
	.skip 1
	.ascii	"uMod_param_u64_u64"
	.skip 1
	.ascii	"Mod_var_u64_u64"
	.skip 1
	.ascii	"uMod_var_u64_u64"
	.skip 1
	.ascii	"Mod_param_u64_u16"
	.skip 1
	.ascii	"uMod_param_u64_u16"
	.skip 1
	.ascii	"Mod_var_u64_u16"
	.skip 1
	.ascii	"uMod_var_u64_u16"
	.skip 1
	.ascii	"Mod_param_u16_u32"
	.skip 1
	.ascii	"uMod_param_u16_u32"
	.skip 1
	.ascii	"Mod_var_u16_u32"
	.skip 1
	.ascii	"uMod_var_u16_u32"
	.skip 1
	.ascii	"Mod_param_u16_i8"
	.skip 1
	.ascii	"uMod_param_u16_i8"
	.skip 1
	.ascii	"Mod_var_u16_i8"
	.skip 1
	.ascii	"uMod_var_u16_i8"
	.skip 1
	.ascii	"Mod_param_u16_L"
	.skip 1
	.ascii	"uMod_param_u16_L"
	.skip 1
	.ascii	"Mod_var_u16_L"
	.skip 1
	.ascii	"uMod_var_u16_L"
	.skip 1
	.ascii	"Mod_param_u16_u8"
	.skip 1
	.ascii	"uMod_param_u16_u8"
	.skip 1
	.ascii	"Mod_var_u16_u8"
	.skip 1
	.ascii	"uMod_var_u16_u8"
	.skip 1
	.ascii	"Mod_param_u16_C"
	.skip 1
	.ascii	"uMod_param_u16_C"
	.skip 1
	.ascii	"Mod_var_u16_C"
	.skip 1
	.ascii	"uMod_var_u16_C"
	.skip 1
	.ascii	"Mod_param_u16_i64"
	.skip 1
	.ascii	"uMod_param_u16_i64"
	.skip 1
	.ascii	"Mod_var_u16_i64"
	.skip 1
	.ascii	"uMod_var_u16_i64"
	.skip 1
	.ascii	"Mod_param_u16_I"
	.skip 1
	.ascii	"uMod_param_u16_I"
	.skip 1
	.ascii	"Mod_var_u16_I"
	.skip 1
	.ascii	"uMod_var_u16_I"
	.skip 1
	.ascii	"Mod_param_u16_i16"
	.skip 1
	.ascii	"uMod_param_u16_i16"
	.skip 1
	.ascii	"Mod_var_u16_i16"
	.skip 1
	.ascii	"uMod_var_u16_i16"
	.skip 1
	.ascii	"Mod_param_u16_i32"
	.skip 1
	.ascii	"uMod_param_u16_i32"
	.skip 1
	.ascii	"Mod_var_u16_i32"
	.skip 1
	.ascii	"uMod_var_u16_i32"
	.skip 1
	.ascii	"Mod_param_u16_LC"
	.skip 1
	.ascii	"uMod_param_u16_LC"
	.skip 1
	.ascii	"Mod_var_u16_LC"
	.skip 1
	.ascii	"uMod_var_u16_LC"
	.skip 1
	.ascii	"Mod_param_u16_u64"
	.skip 1
	.ascii	"uMod_param_u16_u64"
	.skip 1
	.ascii	"Mod_var_u16_u64"
	.skip 1
	.ascii	"uMod_var_u16_u64"
	.skip 1
	.ascii	"Mod_param_u16_u16"
	.skip 1
	.ascii	"uMod_param_u16_u16"
	.skip 1
	.ascii	"Mod_var_u16_u16"
	.skip 1
	.ascii	"uMod_var_u16_u16"
	.skip 2
	.long	Mod_M3
	.long	L_1
	.long	Mod__Mod_param_f32_f32
	.long	L_1+7
	.long	Mod__Mod_var_f32_f32
	.long	L_1+25
	.long	Mod__Mod_param_u32_u32
	.long	L_1+41
	.long	Mod__uMod_param_u32_u32
	.long	L_1+59
	.long	Mod__Mod_var_u32_u32
	.long	L_1+78
	.long	Mod__uMod_var_u32_u32
	.long	L_1+94
	.long	Mod__Mod_param_u32_i8
	.long	L_1+111
	.long	Mod__uMod_param_u32_i8
	.long	L_1+128
	.long	Mod__Mod_var_u32_i8
	.long	L_1+146
	.long	Mod__uMod_var_u32_i8
	.long	L_1+161
	.long	Mod__Mod_param_u32_L
	.long	L_1+177
	.long	Mod__uMod_param_u32_L
	.long	L_1+193
	.long	Mod__Mod_var_u32_L
	.long	L_1+210
	.long	Mod__uMod_var_u32_L
	.long	L_1+224
	.long	Mod__Mod_param_u32_u8
	.long	L_1+239
	.long	Mod__uMod_param_u32_u8
	.long	L_1+256
	.long	Mod__Mod_var_u32_u8
	.long	L_1+274
	.long	Mod__uMod_var_u32_u8
	.long	L_1+289
	.long	Mod__Mod_param_u32_C
	.long	L_1+305
	.long	Mod__uMod_param_u32_C
	.long	L_1+321
	.long	Mod__Mod_var_u32_C
	.long	L_1+338
	.long	Mod__uMod_var_u32_C
	.long	L_1+352
	.long	Mod__Mod_param_u32_i64
	.long	L_1+367
	.long	Mod__uMod_param_u32_i64
	.long	L_1+385
	.long	Mod__Mod_var_u32_i64
	.long	L_1+404
	.long	Mod__uMod_var_u32_i64
	.long	L_1+420
	.long	Mod__Mod_param_u32_I
	.long	L_1+437
	.long	Mod__uMod_param_u32_I
	.long	L_1+453
	.long	Mod__Mod_var_u32_I
	.long	L_1+470
	.long	Mod__uMod_var_u32_I
	.long	L_1+484
	.long	Mod__Mod_param_u32_i16
	.long	L_1+499
	.long	Mod__uMod_param_u32_i16
	.long	L_1+517
	.long	Mod__Mod_var_u32_i16
	.long	L_1+536
	.long	Mod__uMod_var_u32_i16
	.long	L_1+552
	.long	Mod__Mod_param_u32_i32
	.long	L_1+569
	.long	Mod__uMod_param_u32_i32
	.long	L_1+587
	.long	Mod__Mod_var_u32_i32
	.long	L_1+606
	.long	Mod__uMod_var_u32_i32
	.long	L_1+622
	.long	Mod__Mod_param_u32_LC
	.long	L_1+639
	.long	Mod__uMod_param_u32_LC
	.long	L_1+656
	.long	Mod__Mod_var_u32_LC
	.long	L_1+674
	.long	Mod__uMod_var_u32_LC
	.long	L_1+689
	.long	Mod__Mod_param_u32_u64
	.long	L_1+705
	.long	Mod__uMod_param_u32_u64
	.long	L_1+723
	.long	Mod__Mod_var_u32_u64
	.long	L_1+742
	.long	Mod__uMod_var_u32_u64
	.long	L_1+758
	.long	Mod__Mod_param_u32_u16
	.long	L_1+775
	.long	Mod__uMod_param_u32_u16
	.long	L_1+793
	.long	Mod__Mod_var_u32_u16
	.long	L_1+812
	.long	Mod__uMod_var_u32_u16
	.long	L_1+828
	.long	Mod__Mod_param_i8_u32
	.long	L_1+845
	.long	Mod__uMod_param_i8_u32
	.long	L_1+862
	.long	Mod__Mod_var_i8_u32
	.long	L_1+880
	.long	Mod__uMod_var_i8_u32
	.long	L_1+895
	.long	Mod__Mod_param_i8_i8
	.long	L_1+911
	.long	Mod__uMod_param_i8_i8
	.long	L_1+927
	.long	Mod__Mod_var_i8_i8
	.long	L_1+944
	.long	Mod__uMod_var_i8_i8
	.long	L_1+958
	.long	Mod__Mod_param_i8_L
	.long	L_1+973
	.long	Mod__uMod_param_i8_L
	.long	L_1+988
	.long	Mod__Mod_var_i8_L
	.long	L_1+1004
	.long	Mod__uMod_var_i8_L
	.long	L_1+1017
	.long	Mod__Mod_param_i8_u8
	.long	L_1+1031
	.long	Mod__uMod_param_i8_u8
	.long	L_1+1047
	.long	Mod__Mod_var_i8_u8
	.long	L_1+1064
	.long	Mod__uMod_var_i8_u8
	.long	L_1+1078
	.long	Mod__Mod_param_i8_C
	.long	L_1+1093
	.long	Mod__uMod_param_i8_C
	.long	L_1+1108
	.long	Mod__Mod_var_i8_C
	.long	L_1+1124
	.long	Mod__uMod_var_i8_C
	.long	L_1+1137
	.long	Mod__Mod_param_i8_i64
	.long	L_1+1151
	.long	Mod__uMod_param_i8_i64
	.long	L_1+1168
	.long	Mod__Mod_var_i8_i64
	.long	L_1+1186
	.long	Mod__uMod_var_i8_i64
	.long	L_1+1201
	.long	Mod__Mod_param_i8_I
	.long	L_1+1217
	.long	Mod__uMod_param_i8_I
	.long	L_1+1232
	.long	Mod__Mod_var_i8_I
	.long	L_1+1248
	.long	Mod__uMod_var_i8_I
	.long	L_1+1261
	.long	Mod__Mod_param_i8_i16
	.long	L_1+1275
	.long	Mod__uMod_param_i8_i16
	.long	L_1+1292
	.long	Mod__Mod_var_i8_i16
	.long	L_1+1310
	.long	Mod__uMod_var_i8_i16
	.long	L_1+1325
	.long	Mod__Mod_param_i8_i32
	.long	L_1+1341
	.long	Mod__uMod_param_i8_i32
	.long	L_1+1358
	.long	Mod__Mod_var_i8_i32
	.long	L_1+1376
	.long	Mod__uMod_var_i8_i32
	.long	L_1+1391
	.long	Mod__Mod_param_i8_LC
	.long	L_1+1407
	.long	Mod__uMod_param_i8_LC
	.long	L_1+1423
	.long	Mod__Mod_var_i8_LC
	.long	L_1+1440
	.long	Mod__uMod_var_i8_LC
	.long	L_1+1454
	.long	Mod__Mod_param_i8_u64
	.long	L_1+1469
	.long	Mod__uMod_param_i8_u64
	.long	L_1+1486
	.long	Mod__Mod_var_i8_u64
	.long	L_1+1504
	.long	Mod__uMod_var_i8_u64
	.long	L_1+1519
	.long	Mod__Mod_param_i8_u16
	.long	L_1+1535
	.long	Mod__uMod_param_i8_u16
	.long	L_1+1552
	.long	Mod__Mod_var_i8_u16
	.long	L_1+1570
	.long	Mod__uMod_var_i8_u16
	.long	L_1+1585
	.long	Mod__Mod_param_L_u32
	.long	L_1+1601
	.long	Mod__uMod_param_L_u32
	.long	L_1+1617
	.long	Mod__Mod_var_L_u32
	.long	L_1+1634
	.long	Mod__uMod_var_L_u32
	.long	L_1+1648
	.long	Mod__Mod_param_L_i8
	.long	L_1+1663
	.long	Mod__uMod_param_L_i8
	.long	L_1+1678
	.long	Mod__Mod_var_L_i8
	.long	L_1+1694
	.long	Mod__uMod_var_L_i8
	.long	L_1+1707
	.long	Mod__Mod_param_L_L
	.long	L_1+1721
	.long	Mod__uMod_param_L_L
	.long	L_1+1735
	.long	Mod__Mod_var_L_L
	.long	L_1+1750
	.long	Mod__uMod_var_L_L
	.long	L_1+1762
	.long	Mod__Mod_param_L_u8
	.long	L_1+1775
	.long	Mod__uMod_param_L_u8
	.long	L_1+1790
	.long	Mod__Mod_var_L_u8
	.long	L_1+1806
	.long	Mod__uMod_var_L_u8
	.long	L_1+1819
	.long	Mod__Mod_param_L_C
	.long	L_1+1833
	.long	Mod__uMod_param_L_C
	.long	L_1+1847
	.long	Mod__Mod_var_L_C
	.long	L_1+1862
	.long	Mod__uMod_var_L_C
	.long	L_1+1874
	.long	Mod__Mod_param_L_i64
	.long	L_1+1887
	.long	Mod__uMod_param_L_i64
	.long	L_1+1903
	.long	Mod__Mod_var_L_i64
	.long	L_1+1920
	.long	Mod__uMod_var_L_i64
	.long	L_1+1934
	.long	Mod__Mod_param_L_I
	.long	L_1+1949
	.long	Mod__uMod_param_L_I
	.long	L_1+1963
	.long	Mod__Mod_var_L_I
	.long	L_1+1978
	.long	Mod__uMod_var_L_I
	.long	L_1+1990
	.long	Mod__Mod_param_L_i16
	.long	L_1+2003
	.long	Mod__uMod_param_L_i16
	.long	L_1+2019
	.long	Mod__Mod_var_L_i16
	.long	L_1+2036
	.long	Mod__uMod_var_L_i16
	.long	L_1+2050
	.long	Mod__Mod_param_L_i32
	.long	L_1+2065
	.long	Mod__uMod_param_L_i32
	.long	L_1+2081
	.long	Mod__Mod_var_L_i32
	.long	L_1+2098
	.long	Mod__uMod_var_L_i32
	.long	L_1+2112
	.long	Mod__Mod_param_L_LC
	.long	L_1+2127
	.long	Mod__uMod_param_L_LC
	.long	L_1+2142
	.long	Mod__Mod_var_L_LC
	.long	L_1+2158
	.long	Mod__uMod_var_L_LC
	.long	L_1+2171
	.long	Mod__Mod_param_L_u64
	.long	L_1+2185
	.long	Mod__uMod_param_L_u64
	.long	L_1+2201
	.long	Mod__Mod_var_L_u64
	.long	L_1+2218
	.long	Mod__uMod_var_L_u64
	.long	L_1+2232
	.long	Mod__Mod_param_L_u16
	.long	L_1+2247
	.long	Mod__uMod_param_L_u16
	.long	L_1+2263
	.long	Mod__Mod_var_L_u16
	.long	L_1+2280
	.long	Mod__uMod_var_L_u16
	.long	L_1+2294
	.long	Mod__Mod_param_u8_u32
	.long	L_1+2309
	.long	Mod__uMod_param_u8_u32
	.long	L_1+2326
	.long	Mod__Mod_var_u8_u32
	.long	L_1+2344
	.long	Mod__uMod_var_u8_u32
	.long	L_1+2359
	.long	Mod__Mod_param_u8_i8
	.long	L_1+2375
	.long	Mod__uMod_param_u8_i8
	.long	L_1+2391
	.long	Mod__Mod_var_u8_i8
	.long	L_1+2408
	.long	Mod__uMod_var_u8_i8
	.long	L_1+2422
	.long	Mod__Mod_param_u8_L
	.long	L_1+2437
	.long	Mod__uMod_param_u8_L
	.long	L_1+2452
	.long	Mod__Mod_var_u8_L
	.long	L_1+2468
	.long	Mod__uMod_var_u8_L
	.long	L_1+2481
	.long	Mod__Mod_param_u8_u8
	.long	L_1+2495
	.long	Mod__uMod_param_u8_u8
	.long	L_1+2511
	.long	Mod__Mod_var_u8_u8
	.long	L_1+2528
	.long	Mod__uMod_var_u8_u8
	.long	L_1+2542
	.long	Mod__Mod_param_u8_C
	.long	L_1+2557
	.long	Mod__uMod_param_u8_C
	.long	L_1+2572
	.long	Mod__Mod_var_u8_C
	.long	L_1+2588
	.long	Mod__uMod_var_u8_C
	.long	L_1+2601
	.long	Mod__Mod_param_u8_i64
	.long	L_1+2615
	.long	Mod__uMod_param_u8_i64
	.long	L_1+2632
	.long	Mod__Mod_var_u8_i64
	.long	L_1+2650
	.long	Mod__uMod_var_u8_i64
	.long	L_1+2665
	.long	Mod__Mod_param_u8_I
	.long	L_1+2681
	.long	Mod__uMod_param_u8_I
	.long	L_1+2696
	.long	Mod__Mod_var_u8_I
	.long	L_1+2712
	.long	Mod__uMod_var_u8_I
	.long	L_1+2725
	.long	Mod__Mod_param_u8_i16
	.long	L_1+2739
	.long	Mod__uMod_param_u8_i16
	.long	L_1+2756
	.long	Mod__Mod_var_u8_i16
	.long	L_1+2774
	.long	Mod__uMod_var_u8_i16
	.long	L_1+2789
	.long	Mod__Mod_param_u8_i32
	.long	L_1+2805
	.long	Mod__uMod_param_u8_i32
	.long	L_1+2822
	.long	Mod__Mod_var_u8_i32
	.long	L_1+2840
	.long	Mod__uMod_var_u8_i32
	.long	L_1+2855
	.long	Mod__Mod_param_u8_LC
	.long	L_1+2871
	.long	Mod__uMod_param_u8_LC
	.long	L_1+2887
	.long	Mod__Mod_var_u8_LC
	.long	L_1+2904
	.long	Mod__uMod_var_u8_LC
	.long	L_1+2918
	.long	Mod__Mod_param_u8_u64
	.long	L_1+2933
	.long	Mod__uMod_param_u8_u64
	.long	L_1+2950
	.long	Mod__Mod_var_u8_u64
	.long	L_1+2968
	.long	Mod__uMod_var_u8_u64
	.long	L_1+2983
	.long	Mod__Mod_param_u8_u16
	.long	L_1+2999
	.long	Mod__uMod_param_u8_u16
	.long	L_1+3016
	.long	Mod__Mod_var_u8_u16
	.long	L_1+3034
	.long	Mod__uMod_var_u8_u16
	.long	L_1+3049
	.long	Mod__Mod_param_f64_f64
	.long	L_1+3065
	.long	Mod__Mod_var_f64_f64
	.long	L_1+3083
	.long	Mod__Mod_param_C_u32
	.long	L_1+3099
	.long	Mod__uMod_param_C_u32
	.long	L_1+3115
	.long	Mod__Mod_var_C_u32
	.long	L_1+3132
	.long	Mod__uMod_var_C_u32
	.long	L_1+3146
	.long	Mod__Mod_param_C_i8
	.long	L_1+3161
	.long	Mod__uMod_param_C_i8
	.long	L_1+3176
	.long	Mod__Mod_var_C_i8
	.long	L_1+3192
	.long	Mod__uMod_var_C_i8
	.long	L_1+3205
	.long	Mod__Mod_param_C_L
	.long	L_1+3219
	.long	Mod__uMod_param_C_L
	.long	L_1+3233
	.long	Mod__Mod_var_C_L
	.long	L_1+3248
	.long	Mod__uMod_var_C_L
	.long	L_1+3260
	.long	Mod__Mod_param_C_u8
	.long	L_1+3273
	.long	Mod__uMod_param_C_u8
	.long	L_1+3288
	.long	Mod__Mod_var_C_u8
	.long	L_1+3304
	.long	Mod__uMod_var_C_u8
	.long	L_1+3317
	.long	Mod__Mod_param_C_C
	.long	L_1+3331
	.long	Mod__uMod_param_C_C
	.long	L_1+3345
	.long	Mod__Mod_var_C_C
	.long	L_1+3360
	.long	Mod__uMod_var_C_C
	.long	L_1+3372
	.long	Mod__Mod_param_C_i64
	.long	L_1+3385
	.long	Mod__uMod_param_C_i64
	.long	L_1+3401
	.long	Mod__Mod_var_C_i64
	.long	L_1+3418
	.long	Mod__uMod_var_C_i64
	.long	L_1+3432
	.long	Mod__Mod_param_C_I
	.long	L_1+3447
	.long	Mod__uMod_param_C_I
	.long	L_1+3461
	.long	Mod__Mod_var_C_I
	.long	L_1+3476
	.long	Mod__uMod_var_C_I
	.long	L_1+3488
	.long	Mod__Mod_param_C_i16
	.long	L_1+3501
	.long	Mod__uMod_param_C_i16
	.long	L_1+3517
	.long	Mod__Mod_var_C_i16
	.long	L_1+3534
	.long	Mod__uMod_var_C_i16
	.long	L_1+3548
	.long	Mod__Mod_param_C_i32
	.long	L_1+3563
	.long	Mod__uMod_param_C_i32
	.long	L_1+3579
	.long	Mod__Mod_var_C_i32
	.long	L_1+3596
	.long	Mod__uMod_var_C_i32
	.long	L_1+3610
	.long	Mod__Mod_param_C_LC
	.long	L_1+3625
	.long	Mod__uMod_param_C_LC
	.long	L_1+3640
	.long	Mod__Mod_var_C_LC
	.long	L_1+3656
	.long	Mod__uMod_var_C_LC
	.long	L_1+3669
	.long	Mod__Mod_param_C_u64
	.long	L_1+3683
	.long	Mod__uMod_param_C_u64
	.long	L_1+3699
	.long	Mod__Mod_var_C_u64
	.long	L_1+3716
	.long	Mod__uMod_var_C_u64
	.long	L_1+3730
	.long	Mod__Mod_param_C_u16
	.long	L_1+3745
	.long	Mod__uMod_param_C_u16
	.long	L_1+3761
	.long	Mod__Mod_var_C_u16
	.long	L_1+3778
	.long	Mod__uMod_var_C_u16
	.long	L_1+3792
	.long	Mod__Mod_param_i64_u32
	.long	L_1+3807
	.long	Mod__uMod_param_i64_u32
	.long	L_1+3825
	.long	Mod__Mod_var_i64_u32
	.long	L_1+3844
	.long	Mod__uMod_var_i64_u32
	.long	L_1+3860
	.long	Mod__Mod_param_i64_i8
	.long	L_1+3877
	.long	Mod__uMod_param_i64_i8
	.long	L_1+3894
	.long	Mod__Mod_var_i64_i8
	.long	L_1+3912
	.long	Mod__uMod_var_i64_i8
	.long	L_1+3927
	.long	Mod__Mod_param_i64_L
	.long	L_1+3943
	.long	Mod__uMod_param_i64_L
	.long	L_1+3959
	.long	Mod__Mod_var_i64_L
	.long	L_1+3976
	.long	Mod__uMod_var_i64_L
	.long	L_1+3990
	.long	Mod__Mod_param_i64_u8
	.long	L_1+4005
	.long	Mod__uMod_param_i64_u8
	.long	L_1+4022
	.long	Mod__Mod_var_i64_u8
	.long	L_1+4040
	.long	Mod__uMod_var_i64_u8
	.long	L_1+4055
	.long	Mod__Mod_param_i64_C
	.long	L_1+4071
	.long	Mod__uMod_param_i64_C
	.long	L_1+4087
	.long	Mod__Mod_var_i64_C
	.long	L_1+4104
	.long	Mod__uMod_var_i64_C
	.long	L_1+4118
	.long	Mod__Mod_param_i64_i64
	.long	L_1+4133
	.long	Mod__uMod_param_i64_i64
	.long	L_1+4151
	.long	Mod__Mod_var_i64_i64
	.long	L_1+4170
	.long	Mod__uMod_var_i64_i64
	.long	L_1+4186
	.long	Mod__Mod_param_i64_I
	.long	L_1+4203
	.long	Mod__uMod_param_i64_I
	.long	L_1+4219
	.long	Mod__Mod_var_i64_I
	.long	L_1+4236
	.long	Mod__uMod_var_i64_I
	.long	L_1+4250
	.long	Mod__Mod_param_i64_i16
	.long	L_1+4265
	.long	Mod__uMod_param_i64_i16
	.long	L_1+4283
	.long	Mod__Mod_var_i64_i16
	.long	L_1+4302
	.long	Mod__uMod_var_i64_i16
	.long	L_1+4318
	.long	Mod__Mod_param_i64_i32
	.long	L_1+4335
	.long	Mod__uMod_param_i64_i32
	.long	L_1+4353
	.long	Mod__Mod_var_i64_i32
	.long	L_1+4372
	.long	Mod__uMod_var_i64_i32
	.long	L_1+4388
	.long	Mod__Mod_param_i64_LC
	.long	L_1+4405
	.long	Mod__uMod_param_i64_LC
	.long	L_1+4422
	.long	Mod__Mod_var_i64_LC
	.long	L_1+4440
	.long	Mod__uMod_var_i64_LC
	.long	L_1+4455
	.long	Mod__Mod_param_i64_u64
	.long	L_1+4471
	.long	Mod__uMod_param_i64_u64
	.long	L_1+4489
	.long	Mod__Mod_var_i64_u64
	.long	L_1+4508
	.long	Mod__uMod_var_i64_u64
	.long	L_1+4524
	.long	Mod__Mod_param_i64_u16
	.long	L_1+4541
	.long	Mod__uMod_param_i64_u16
	.long	L_1+4559
	.long	Mod__Mod_var_i64_u16
	.long	L_1+4578
	.long	Mod__uMod_var_i64_u16
	.long	L_1+4594
	.long	Mod__Mod_param_I_u32
	.long	L_1+4611
	.long	Mod__uMod_param_I_u32
	.long	L_1+4627
	.long	Mod__Mod_var_I_u32
	.long	L_1+4644
	.long	Mod__uMod_var_I_u32
	.long	L_1+4658
	.long	Mod__Mod_param_I_i8
	.long	L_1+4673
	.long	Mod__uMod_param_I_i8
	.long	L_1+4688
	.long	Mod__Mod_var_I_i8
	.long	L_1+4704
	.long	Mod__uMod_var_I_i8
	.long	L_1+4717
	.long	Mod__Mod_param_I_L
	.long	L_1+4731
	.long	Mod__uMod_param_I_L
	.long	L_1+4745
	.long	Mod__Mod_var_I_L
	.long	L_1+4760
	.long	Mod__uMod_var_I_L
	.long	L_1+4772
	.long	Mod__Mod_param_I_u8
	.long	L_1+4785
	.long	Mod__uMod_param_I_u8
	.long	L_1+4800
	.long	Mod__Mod_var_I_u8
	.long	L_1+4816
	.long	Mod__uMod_var_I_u8
	.long	L_1+4829
	.long	Mod__Mod_param_I_C
	.long	L_1+4843
	.long	Mod__uMod_param_I_C
	.long	L_1+4857
	.long	Mod__Mod_var_I_C
	.long	L_1+4872
	.long	Mod__uMod_var_I_C
	.long	L_1+4884
	.long	Mod__Mod_param_I_i64
	.long	L_1+4897
	.long	Mod__uMod_param_I_i64
	.long	L_1+4913
	.long	Mod__Mod_var_I_i64
	.long	L_1+4930
	.long	Mod__uMod_var_I_i64
	.long	L_1+4944
	.long	Mod__Mod_param_I_I
	.long	L_1+4959
	.long	Mod__uMod_param_I_I
	.long	L_1+4973
	.long	Mod__Mod_var_I_I
	.long	L_1+4988
	.long	Mod__uMod_var_I_I
	.long	L_1+5000
	.long	Mod__Mod_param_I_i16
	.long	L_1+5013
	.long	Mod__uMod_param_I_i16
	.long	L_1+5029
	.long	Mod__Mod_var_I_i16
	.long	L_1+5046
	.long	Mod__uMod_var_I_i16
	.long	L_1+5060
	.long	Mod__Mod_param_I_i32
	.long	L_1+5075
	.long	Mod__uMod_param_I_i32
	.long	L_1+5091
	.long	Mod__Mod_var_I_i32
	.long	L_1+5108
	.long	Mod__uMod_var_I_i32
	.long	L_1+5122
	.long	Mod__Mod_param_I_LC
	.long	L_1+5137
	.long	Mod__uMod_param_I_LC
	.long	L_1+5152
	.long	Mod__Mod_var_I_LC
	.long	L_1+5168
	.long	Mod__uMod_var_I_LC
	.long	L_1+5181
	.long	Mod__Mod_param_I_u64
	.long	L_1+5195
	.long	Mod__uMod_param_I_u64
	.long	L_1+5211
	.long	Mod__Mod_var_I_u64
	.long	L_1+5228
	.long	Mod__uMod_var_I_u64
	.long	L_1+5242
	.long	Mod__Mod_param_I_u16
	.long	L_1+5257
	.long	Mod__uMod_param_I_u16
	.long	L_1+5273
	.long	Mod__Mod_var_I_u16
	.long	L_1+5290
	.long	Mod__uMod_var_I_u16
	.long	L_1+5304
	.long	Mod__Mod_param_i16_u32
	.long	L_1+5319
	.long	Mod__uMod_param_i16_u32
	.long	L_1+5337
	.long	Mod__Mod_var_i16_u32
	.long	L_1+5356
	.long	Mod__uMod_var_i16_u32
	.long	L_1+5372
	.long	Mod__Mod_param_i16_i8
	.long	L_1+5389
	.long	Mod__uMod_param_i16_i8
	.long	L_1+5406
	.long	Mod__Mod_var_i16_i8
	.long	L_1+5424
	.long	Mod__uMod_var_i16_i8
	.long	L_1+5439
	.long	Mod__Mod_param_i16_L
	.long	L_1+5455
	.long	Mod__uMod_param_i16_L
	.long	L_1+5471
	.long	Mod__Mod_var_i16_L
	.long	L_1+5488
	.long	Mod__uMod_var_i16_L
	.long	L_1+5502
	.long	Mod__Mod_param_i16_u8
	.long	L_1+5517
	.long	Mod__uMod_param_i16_u8
	.long	L_1+5534
	.long	Mod__Mod_var_i16_u8
	.long	L_1+5552
	.long	Mod__uMod_var_i16_u8
	.long	L_1+5567
	.long	Mod__Mod_param_i16_C
	.long	L_1+5583
	.long	Mod__uMod_param_i16_C
	.long	L_1+5599
	.long	Mod__Mod_var_i16_C
	.long	L_1+5616
	.long	Mod__uMod_var_i16_C
	.long	L_1+5630
	.long	Mod__Mod_param_i16_i64
	.long	L_1+5645
	.long	Mod__uMod_param_i16_i64
	.long	L_1+5663
	.long	Mod__Mod_var_i16_i64
	.long	L_1+5682
	.long	Mod__uMod_var_i16_i64
	.long	L_1+5698
	.long	Mod__Mod_param_i16_I
	.long	L_1+5715
	.long	Mod__uMod_param_i16_I
	.long	L_1+5731
	.long	Mod__Mod_var_i16_I
	.long	L_1+5748
	.long	Mod__uMod_var_i16_I
	.long	L_1+5762
	.long	Mod__Mod_param_i16_i16
	.long	L_1+5777
	.long	Mod__uMod_param_i16_i16
	.long	L_1+5795
	.long	Mod__Mod_var_i16_i16
	.long	L_1+5814
	.long	Mod__uMod_var_i16_i16
	.long	L_1+5830
	.long	Mod__Mod_param_i16_i32
	.long	L_1+5847
	.long	Mod__uMod_param_i16_i32
	.long	L_1+5865
	.long	Mod__Mod_var_i16_i32
	.long	L_1+5884
	.long	Mod__uMod_var_i16_i32
	.long	L_1+5900
	.long	Mod__Mod_param_i16_LC
	.long	L_1+5917
	.long	Mod__uMod_param_i16_LC
	.long	L_1+5934
	.long	Mod__Mod_var_i16_LC
	.long	L_1+5952
	.long	Mod__uMod_var_i16_LC
	.long	L_1+5967
	.long	Mod__Mod_param_i16_u64
	.long	L_1+5983
	.long	Mod__uMod_param_i16_u64
	.long	L_1+6001
	.long	Mod__Mod_var_i16_u64
	.long	L_1+6020
	.long	Mod__uMod_var_i16_u64
	.long	L_1+6036
	.long	Mod__Mod_param_i16_u16
	.long	L_1+6053
	.long	Mod__uMod_param_i16_u16
	.long	L_1+6071
	.long	Mod__Mod_var_i16_u16
	.long	L_1+6090
	.long	Mod__uMod_var_i16_u16
	.long	L_1+6106
	.long	Mod__Mod_param_i32_u32
	.long	L_1+6123
	.long	Mod__uMod_param_i32_u32
	.long	L_1+6141
	.long	Mod__Mod_var_i32_u32
	.long	L_1+6160
	.long	Mod__uMod_var_i32_u32
	.long	L_1+6176
	.long	Mod__Mod_param_i32_i8
	.long	L_1+6193
	.long	Mod__uMod_param_i32_i8
	.long	L_1+6210
	.long	Mod__Mod_var_i32_i8
	.long	L_1+6228
	.long	Mod__uMod_var_i32_i8
	.long	L_1+6243
	.long	Mod__Mod_param_i32_L
	.long	L_1+6259
	.long	Mod__uMod_param_i32_L
	.long	L_1+6275
	.long	Mod__Mod_var_i32_L
	.long	L_1+6292
	.long	Mod__uMod_var_i32_L
	.long	L_1+6306
	.long	Mod__Mod_param_i32_u8
	.long	L_1+6321
	.long	Mod__uMod_param_i32_u8
	.long	L_1+6338
	.long	Mod__Mod_var_i32_u8
	.long	L_1+6356
	.long	Mod__uMod_var_i32_u8
	.long	L_1+6371
	.long	Mod__Mod_param_i32_C
	.long	L_1+6387
	.long	Mod__uMod_param_i32_C
	.long	L_1+6403
	.long	Mod__Mod_var_i32_C
	.long	L_1+6420
	.long	Mod__uMod_var_i32_C
	.long	L_1+6434
	.long	Mod__Mod_param_i32_i64
	.long	L_1+6449
	.long	Mod__uMod_param_i32_i64
	.long	L_1+6467
	.long	Mod__Mod_var_i32_i64
	.long	L_1+6486
	.long	Mod__uMod_var_i32_i64
	.long	L_1+6502
	.long	Mod__Mod_param_i32_I
	.long	L_1+6519
	.long	Mod__uMod_param_i32_I
	.long	L_1+6535
	.long	Mod__Mod_var_i32_I
	.long	L_1+6552
	.long	Mod__uMod_var_i32_I
	.long	L_1+6566
	.long	Mod__Mod_param_i32_i16
	.long	L_1+6581
	.long	Mod__uMod_param_i32_i16
	.long	L_1+6599
	.long	Mod__Mod_var_i32_i16
	.long	L_1+6618
	.long	Mod__uMod_var_i32_i16
	.long	L_1+6634
	.long	Mod__Mod_param_i32_i32
	.long	L_1+6651
	.long	Mod__uMod_param_i32_i32
	.long	L_1+6669
	.long	Mod__Mod_var_i32_i32
	.long	L_1+6688
	.long	Mod__uMod_var_i32_i32
	.long	L_1+6704
	.long	Mod__Mod_param_i32_LC
	.long	L_1+6721
	.long	Mod__uMod_param_i32_LC
	.long	L_1+6738
	.long	Mod__Mod_var_i32_LC
	.long	L_1+6756
	.long	Mod__uMod_var_i32_LC
	.long	L_1+6771
	.long	Mod__Mod_param_i32_u64
	.long	L_1+6787
	.long	Mod__uMod_param_i32_u64
	.long	L_1+6805
	.long	Mod__Mod_var_i32_u64
	.long	L_1+6824
	.long	Mod__uMod_var_i32_u64
	.long	L_1+6840
	.long	Mod__Mod_param_i32_u16
	.long	L_1+6857
	.long	Mod__uMod_param_i32_u16
	.long	L_1+6875
	.long	Mod__Mod_var_i32_u16
	.long	L_1+6894
	.long	Mod__uMod_var_i32_u16
	.long	L_1+6910
	.long	Mod__Mod_param_LC_u32
	.long	L_1+6927
	.long	Mod__uMod_param_LC_u32
	.long	L_1+6944
	.long	Mod__Mod_var_LC_u32
	.long	L_1+6962
	.long	Mod__uMod_var_LC_u32
	.long	L_1+6977
	.long	Mod__Mod_param_LC_i8
	.long	L_1+6993
	.long	Mod__uMod_param_LC_i8
	.long	L_1+7009
	.long	Mod__Mod_var_LC_i8
	.long	L_1+7026
	.long	Mod__uMod_var_LC_i8
	.long	L_1+7040
	.long	Mod__Mod_param_LC_L
	.long	L_1+7055
	.long	Mod__uMod_param_LC_L
	.long	L_1+7070
	.long	Mod__Mod_var_LC_L
	.long	L_1+7086
	.long	Mod__uMod_var_LC_L
	.long	L_1+7099
	.long	Mod__Mod_param_LC_u8
	.long	L_1+7113
	.long	Mod__uMod_param_LC_u8
	.long	L_1+7129
	.long	Mod__Mod_var_LC_u8
	.long	L_1+7146
	.long	Mod__uMod_var_LC_u8
	.long	L_1+7160
	.long	Mod__Mod_param_LC_C
	.long	L_1+7175
	.long	Mod__uMod_param_LC_C
	.long	L_1+7190
	.long	Mod__Mod_var_LC_C
	.long	L_1+7206
	.long	Mod__uMod_var_LC_C
	.long	L_1+7219
	.long	Mod__Mod_param_LC_i64
	.long	L_1+7233
	.long	Mod__uMod_param_LC_i64
	.long	L_1+7250
	.long	Mod__Mod_var_LC_i64
	.long	L_1+7268
	.long	Mod__uMod_var_LC_i64
	.long	L_1+7283
	.long	Mod__Mod_param_LC_I
	.long	L_1+7299
	.long	Mod__uMod_param_LC_I
	.long	L_1+7314
	.long	Mod__Mod_var_LC_I
	.long	L_1+7330
	.long	Mod__uMod_var_LC_I
	.long	L_1+7343
	.long	Mod__Mod_param_LC_i16
	.long	L_1+7357
	.long	Mod__uMod_param_LC_i16
	.long	L_1+7374
	.long	Mod__Mod_var_LC_i16
	.long	L_1+7392
	.long	Mod__uMod_var_LC_i16
	.long	L_1+7407
	.long	Mod__Mod_param_LC_i32
	.long	L_1+7423
	.long	Mod__uMod_param_LC_i32
	.long	L_1+7440
	.long	Mod__Mod_var_LC_i32
	.long	L_1+7458
	.long	Mod__uMod_var_LC_i32
	.long	L_1+7473
	.long	Mod__Mod_param_LC_LC
	.long	L_1+7489
	.long	Mod__uMod_param_LC_LC
	.long	L_1+7505
	.long	Mod__Mod_var_LC_LC
	.long	L_1+7522
	.long	Mod__uMod_var_LC_LC
	.long	L_1+7536
	.long	Mod__Mod_param_LC_u64
	.long	L_1+7551
	.long	Mod__uMod_param_LC_u64
	.long	L_1+7568
	.long	Mod__Mod_var_LC_u64
	.long	L_1+7586
	.long	Mod__uMod_var_LC_u64
	.long	L_1+7601
	.long	Mod__Mod_param_LC_u16
	.long	L_1+7617
	.long	Mod__uMod_param_LC_u16
	.long	L_1+7634
	.long	Mod__Mod_var_LC_u16
	.long	L_1+7652
	.long	Mod__uMod_var_LC_u16
	.long	L_1+7667
	.long	Mod__Mod_param_u64_u32
	.long	L_1+7683
	.long	Mod__uMod_param_u64_u32
	.long	L_1+7701
	.long	Mod__Mod_var_u64_u32
	.long	L_1+7720
	.long	Mod__uMod_var_u64_u32
	.long	L_1+7736
	.long	Mod__Mod_param_u64_i8
	.long	L_1+7753
	.long	Mod__uMod_param_u64_i8
	.long	L_1+7770
	.long	Mod__Mod_var_u64_i8
	.long	L_1+7788
	.long	Mod__uMod_var_u64_i8
	.long	L_1+7803
	.long	Mod__Mod_param_u64_L
	.long	L_1+7819
	.long	Mod__uMod_param_u64_L
	.long	L_1+7835
	.long	Mod__Mod_var_u64_L
	.long	L_1+7852
	.long	Mod__uMod_var_u64_L
	.long	L_1+7866
	.long	Mod__Mod_param_u64_u8
	.long	L_1+7881
	.long	Mod__uMod_param_u64_u8
	.long	L_1+7898
	.long	Mod__Mod_var_u64_u8
	.long	L_1+7916
	.long	Mod__uMod_var_u64_u8
	.long	L_1+7931
	.long	Mod__Mod_param_u64_C
	.long	L_1+7947
	.long	Mod__uMod_param_u64_C
	.long	L_1+7963
	.long	Mod__Mod_var_u64_C
	.long	L_1+7980
	.long	Mod__uMod_var_u64_C
	.long	L_1+7994
	.long	Mod__Mod_param_u64_i64
	.long	L_1+8009
	.long	Mod__uMod_param_u64_i64
	.long	L_1+8027
	.long	Mod__Mod_var_u64_i64
	.long	L_1+8046
	.long	Mod__uMod_var_u64_i64
	.long	L_1+8062
	.long	Mod__Mod_param_u64_I
	.long	L_1+8079
	.long	Mod__uMod_param_u64_I
	.long	L_1+8095
	.long	Mod__Mod_var_u64_I
	.long	L_1+8112
	.long	Mod__uMod_var_u64_I
	.long	L_1+8126
	.long	Mod__Mod_param_u64_i16
	.long	L_1+8141
	.long	Mod__uMod_param_u64_i16
	.long	L_1+8159
	.long	Mod__Mod_var_u64_i16
	.long	L_1+8178
	.long	Mod__uMod_var_u64_i16
	.long	L_1+8194
	.long	Mod__Mod_param_u64_i32
	.long	L_1+8211
	.long	Mod__uMod_param_u64_i32
	.long	L_1+8229
	.long	Mod__Mod_var_u64_i32
	.long	L_1+8248
	.long	Mod__uMod_var_u64_i32
	.long	L_1+8264
	.long	Mod__Mod_param_u64_LC
	.long	L_1+8281
	.long	Mod__uMod_param_u64_LC
	.long	L_1+8298
	.long	Mod__Mod_var_u64_LC
	.long	L_1+8316
	.long	Mod__uMod_var_u64_LC
	.long	L_1+8331
	.long	Mod__Mod_param_u64_u64
	.long	L_1+8347
	.long	Mod__uMod_param_u64_u64
	.long	L_1+8365
	.long	Mod__Mod_var_u64_u64
	.long	L_1+8384
	.long	Mod__uMod_var_u64_u64
	.long	L_1+8400
	.long	Mod__Mod_param_u64_u16
	.long	L_1+8417
	.long	Mod__uMod_param_u64_u16
	.long	L_1+8435
	.long	Mod__Mod_var_u64_u16
	.long	L_1+8454
	.long	Mod__uMod_var_u64_u16
	.long	L_1+8470
	.long	Mod__Mod_param_u16_u32
	.long	L_1+8487
	.long	Mod__uMod_param_u16_u32
	.long	L_1+8505
	.long	Mod__Mod_var_u16_u32
	.long	L_1+8524
	.long	Mod__uMod_var_u16_u32
	.long	L_1+8540
	.long	Mod__Mod_param_u16_i8
	.long	L_1+8557
	.long	Mod__uMod_param_u16_i8
	.long	L_1+8574
	.long	Mod__Mod_var_u16_i8
	.long	L_1+8592
	.long	Mod__uMod_var_u16_i8
	.long	L_1+8607
	.long	Mod__Mod_param_u16_L
	.long	L_1+8623
	.long	Mod__uMod_param_u16_L
	.long	L_1+8639
	.long	Mod__Mod_var_u16_L
	.long	L_1+8656
	.long	Mod__uMod_var_u16_L
	.long	L_1+8670
	.long	Mod__Mod_param_u16_u8
	.long	L_1+8685
	.long	Mod__uMod_param_u16_u8
	.long	L_1+8702
	.long	Mod__Mod_var_u16_u8
	.long	L_1+8720
	.long	Mod__uMod_var_u16_u8
	.long	L_1+8735
	.long	Mod__Mod_param_u16_C
	.long	L_1+8751
	.long	Mod__uMod_param_u16_C
	.long	L_1+8767
	.long	Mod__Mod_var_u16_C
	.long	L_1+8784
	.long	Mod__uMod_var_u16_C
	.long	L_1+8798
	.long	Mod__Mod_param_u16_i64
	.long	L_1+8813
	.long	Mod__uMod_param_u16_i64
	.long	L_1+8831
	.long	Mod__Mod_var_u16_i64
	.long	L_1+8850
	.long	Mod__uMod_var_u16_i64
	.long	L_1+8866
	.long	Mod__Mod_param_u16_I
	.long	L_1+8883
	.long	Mod__uMod_param_u16_I
	.long	L_1+8899
	.long	Mod__Mod_var_u16_I
	.long	L_1+8916
	.long	Mod__uMod_var_u16_I
	.long	L_1+8930
	.long	Mod__Mod_param_u16_i16
	.long	L_1+8945
	.long	Mod__uMod_param_u16_i16
	.long	L_1+8963
	.long	Mod__Mod_var_u16_i16
	.long	L_1+8982
	.long	Mod__uMod_var_u16_i16
	.long	L_1+8998
	.long	Mod__Mod_param_u16_i32
	.long	L_1+9015
	.long	Mod__uMod_param_u16_i32
	.long	L_1+9033
	.long	Mod__Mod_var_u16_i32
	.long	L_1+9052
	.long	Mod__uMod_var_u16_i32
	.long	L_1+9068
	.long	Mod__Mod_param_u16_LC
	.long	L_1+9085
	.long	Mod__uMod_param_u16_LC
	.long	L_1+9102
	.long	Mod__Mod_var_u16_LC
	.long	L_1+9120
	.long	Mod__uMod_var_u16_LC
	.long	L_1+9135
	.long	Mod__Mod_param_u16_u64
	.long	L_1+9151
	.long	Mod__uMod_param_u16_u64
	.long	L_1+9169
	.long	Mod__Mod_var_u16_u64
	.long	L_1+9188
	.long	Mod__uMod_var_u16_u64
	.long	L_1+9204
	.long	Mod__Mod_param_u16_u16
	.long	L_1+9221
	.long	Mod__uMod_param_u16_u16
	.long	L_1+9239
	.long	Mod__Mod_var_u16_u16
	.long	L_1+9258
	.long	Mod__uMod_var_u16_u16
	.long	L_1+9274
	.skip 4
	.ascii	"../SOLsun/Mod.m3"
	.skip 8
	.align 8
	.type	MM_Mod, #object
	.size	MM_Mod, 208
MM_Mod:
	.long	L_1+13944
	.skip 16
	.long	L_1+9292
	.skip 12
	.long	MM_Mod+148
	.skip 4
	.long	Mod_M3
	.long	3
	.half	273
	.skip 2
	.long	0
	.long	274
	.long	0
	.long	275
	.long	276
	.half	277
	.skip 2
	.long	278
	.skip 4
	.long	0
	.long	279
	.long	280
	.skip 4
	.long	1081185411
	.long	309237645
	.byte	27
	.skip 7
	.long	0
	.long	284
	.byte	29
	.skip 3
	.long	286
	.long	1133487325
	.skip 12
	.long	Mod_I3
	.long	MM_Mod+160
	.skip 4
	.long	Long_I3
	.long	MM_Mod+172
	.skip 4
	.long	Word_I3
	.long	MM_Mod+184
	.skip 4
	.long	Cstdint_I3
	.long	MM_Mod+196
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
.LLEFDE1:
.LLSFDE3:
	.uaword	.LLEFDE3-.LLASFDE3
.LLASFDE3:
	.uaword	.LLASFDE3-.LLframe1
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
.LLEFDE3:
.LLSFDE5:
	.uaword	.LLEFDE5-.LLASFDE5
.LLASFDE5:
	.uaword	.LLASFDE5-.LLframe1
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
.LLEFDE5:
.LLSFDE7:
	.uaword	.LLEFDE7-.LLASFDE7
.LLASFDE7:
	.uaword	.LLASFDE7-.LLframe1
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
.LLEFDE7:
.LLSFDE9:
	.uaword	.LLEFDE9-.LLASFDE9
.LLASFDE9:
	.uaword	.LLASFDE9-.LLframe1
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
.LLEFDE9:
.LLSFDE11:
	.uaword	.LLEFDE11-.LLASFDE11
.LLASFDE11:
	.uaword	.LLASFDE11-.LLframe1
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
.LLEFDE11:
.LLSFDE13:
	.uaword	.LLEFDE13-.LLASFDE13
.LLASFDE13:
	.uaword	.LLASFDE13-.LLframe1
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
.LLEFDE13:
.LLSFDE15:
	.uaword	.LLEFDE15-.LLASFDE15
.LLASFDE15:
	.uaword	.LLASFDE15-.LLframe1
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
.LLEFDE15:
.LLSFDE17:
	.uaword	.LLEFDE17-.LLASFDE17
.LLASFDE17:
	.uaword	.LLASFDE17-.LLframe1
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
.LLEFDE17:
.LLSFDE19:
	.uaword	.LLEFDE19-.LLASFDE19
.LLASFDE19:
	.uaword	.LLASFDE19-.LLframe1
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
.LLEFDE19:
.LLSFDE21:
	.uaword	.LLEFDE21-.LLASFDE21
.LLASFDE21:
	.uaword	.LLASFDE21-.LLframe1
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
.LLEFDE21:
.LLSFDE23:
	.uaword	.LLEFDE23-.LLASFDE23
.LLASFDE23:
	.uaword	.LLASFDE23-.LLframe1
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
.LLEFDE23:
.LLSFDE25:
	.uaword	.LLEFDE25-.LLASFDE25
.LLASFDE25:
	.uaword	.LLASFDE25-.LLframe1
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
.LLEFDE25:
.LLSFDE27:
	.uaword	.LLEFDE27-.LLASFDE27
.LLASFDE27:
	.uaword	.LLASFDE27-.LLframe1
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
.LLEFDE27:
.LLSFDE29:
	.uaword	.LLEFDE29-.LLASFDE29
.LLASFDE29:
	.uaword	.LLASFDE29-.LLframe1
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
.LLEFDE29:
.LLSFDE31:
	.uaword	.LLEFDE31-.LLASFDE31
.LLASFDE31:
	.uaword	.LLASFDE31-.LLframe1
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
.LLEFDE31:
.LLSFDE33:
	.uaword	.LLEFDE33-.LLASFDE33
.LLASFDE33:
	.uaword	.LLASFDE33-.LLframe1
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
.LLEFDE33:
.LLSFDE35:
	.uaword	.LLEFDE35-.LLASFDE35
.LLASFDE35:
	.uaword	.LLASFDE35-.LLframe1
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
.LLEFDE35:
.LLSFDE37:
	.uaword	.LLEFDE37-.LLASFDE37
.LLASFDE37:
	.uaword	.LLASFDE37-.LLframe1
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
.LLEFDE37:
.LLSFDE39:
	.uaword	.LLEFDE39-.LLASFDE39
.LLASFDE39:
	.uaword	.LLASFDE39-.LLframe1
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
.LLEFDE39:
.LLSFDE41:
	.uaword	.LLEFDE41-.LLASFDE41
.LLASFDE41:
	.uaword	.LLASFDE41-.LLframe1
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
.LLEFDE41:
.LLSFDE43:
	.uaword	.LLEFDE43-.LLASFDE43
.LLASFDE43:
	.uaword	.LLASFDE43-.LLframe1
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
.LLEFDE43:
.LLSFDE45:
	.uaword	.LLEFDE45-.LLASFDE45
.LLASFDE45:
	.uaword	.LLASFDE45-.LLframe1
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
.LLEFDE45:
.LLSFDE47:
	.uaword	.LLEFDE47-.LLASFDE47
.LLASFDE47:
	.uaword	.LLASFDE47-.LLframe1
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
.LLEFDE47:
.LLSFDE49:
	.uaword	.LLEFDE49-.LLASFDE49
.LLASFDE49:
	.uaword	.LLASFDE49-.LLframe1
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
.LLEFDE49:
.LLSFDE51:
	.uaword	.LLEFDE51-.LLASFDE51
.LLASFDE51:
	.uaword	.LLASFDE51-.LLframe1
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
.LLEFDE51:
.LLSFDE53:
	.uaword	.LLEFDE53-.LLASFDE53
.LLASFDE53:
	.uaword	.LLASFDE53-.LLframe1
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
.LLEFDE53:
.LLSFDE55:
	.uaword	.LLEFDE55-.LLASFDE55
.LLASFDE55:
	.uaword	.LLASFDE55-.LLframe1
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
.LLEFDE55:
.LLSFDE57:
	.uaword	.LLEFDE57-.LLASFDE57
.LLASFDE57:
	.uaword	.LLASFDE57-.LLframe1
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
.LLEFDE57:
.LLSFDE59:
	.uaword	.LLEFDE59-.LLASFDE59
.LLASFDE59:
	.uaword	.LLASFDE59-.LLframe1
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
.LLEFDE59:
.LLSFDE61:
	.uaword	.LLEFDE61-.LLASFDE61
.LLASFDE61:
	.uaword	.LLASFDE61-.LLframe1
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
.LLEFDE61:
.LLSFDE63:
	.uaword	.LLEFDE63-.LLASFDE63
.LLASFDE63:
	.uaword	.LLASFDE63-.LLframe1
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
.LLEFDE63:
.LLSFDE65:
	.uaword	.LLEFDE65-.LLASFDE65
.LLASFDE65:
	.uaword	.LLASFDE65-.LLframe1
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
.LLEFDE65:
.LLSFDE67:
	.uaword	.LLEFDE67-.LLASFDE67
.LLASFDE67:
	.uaword	.LLASFDE67-.LLframe1
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
.LLEFDE67:
.LLSFDE69:
	.uaword	.LLEFDE69-.LLASFDE69
.LLASFDE69:
	.uaword	.LLASFDE69-.LLframe1
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
.LLEFDE69:
.LLSFDE71:
	.uaword	.LLEFDE71-.LLASFDE71
.LLASFDE71:
	.uaword	.LLASFDE71-.LLframe1
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
.LLEFDE71:
.LLSFDE73:
	.uaword	.LLEFDE73-.LLASFDE73
.LLASFDE73:
	.uaword	.LLASFDE73-.LLframe1
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
.LLEFDE73:
.LLSFDE75:
	.uaword	.LLEFDE75-.LLASFDE75
.LLASFDE75:
	.uaword	.LLASFDE75-.LLframe1
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
.LLEFDE75:
.LLSFDE77:
	.uaword	.LLEFDE77-.LLASFDE77
.LLASFDE77:
	.uaword	.LLASFDE77-.LLframe1
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
.LLEFDE77:
.LLSFDE79:
	.uaword	.LLEFDE79-.LLASFDE79
.LLASFDE79:
	.uaword	.LLASFDE79-.LLframe1
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
.LLEFDE79:
.LLSFDE81:
	.uaword	.LLEFDE81-.LLASFDE81
.LLASFDE81:
	.uaword	.LLASFDE81-.LLframe1
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
.LLEFDE81:
.LLSFDE83:
	.uaword	.LLEFDE83-.LLASFDE83
.LLASFDE83:
	.uaword	.LLASFDE83-.LLframe1
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
.LLEFDE83:
.LLSFDE85:
	.uaword	.LLEFDE85-.LLASFDE85
.LLASFDE85:
	.uaword	.LLASFDE85-.LLframe1
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
.LLEFDE85:
.LLSFDE87:
	.uaword	.LLEFDE87-.LLASFDE87
.LLASFDE87:
	.uaword	.LLASFDE87-.LLframe1
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
.LLEFDE87:
.LLSFDE89:
	.uaword	.LLEFDE89-.LLASFDE89
.LLASFDE89:
	.uaword	.LLASFDE89-.LLframe1
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
.LLEFDE89:
.LLSFDE91:
	.uaword	.LLEFDE91-.LLASFDE91
.LLASFDE91:
	.uaword	.LLASFDE91-.LLframe1
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
.LLEFDE91:
.LLSFDE93:
	.uaword	.LLEFDE93-.LLASFDE93
.LLASFDE93:
	.uaword	.LLASFDE93-.LLframe1
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
.LLEFDE93:
.LLSFDE95:
	.uaword	.LLEFDE95-.LLASFDE95
.LLASFDE95:
	.uaword	.LLASFDE95-.LLframe1
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
.LLEFDE95:
.LLSFDE97:
	.uaword	.LLEFDE97-.LLASFDE97
.LLASFDE97:
	.uaword	.LLASFDE97-.LLframe1
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
.LLEFDE97:
.LLSFDE99:
	.uaword	.LLEFDE99-.LLASFDE99
.LLASFDE99:
	.uaword	.LLASFDE99-.LLframe1
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
.LLEFDE99:
.LLSFDE101:
	.uaword	.LLEFDE101-.LLASFDE101
.LLASFDE101:
	.uaword	.LLASFDE101-.LLframe1
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
.LLEFDE101:
.LLSFDE103:
	.uaword	.LLEFDE103-.LLASFDE103
.LLASFDE103:
	.uaword	.LLASFDE103-.LLframe1
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
.LLEFDE103:
.LLSFDE105:
	.uaword	.LLEFDE105-.LLASFDE105
.LLASFDE105:
	.uaword	.LLASFDE105-.LLframe1
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
.LLEFDE105:
.LLSFDE107:
	.uaword	.LLEFDE107-.LLASFDE107
.LLASFDE107:
	.uaword	.LLASFDE107-.LLframe1
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
.LLEFDE107:
.LLSFDE109:
	.uaword	.LLEFDE109-.LLASFDE109
.LLASFDE109:
	.uaword	.LLASFDE109-.LLframe1
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
.LLEFDE109:
.LLSFDE111:
	.uaword	.LLEFDE111-.LLASFDE111
.LLASFDE111:
	.uaword	.LLASFDE111-.LLframe1
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
.LLEFDE111:
.LLSFDE113:
	.uaword	.LLEFDE113-.LLASFDE113
.LLASFDE113:
	.uaword	.LLASFDE113-.LLframe1
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
.LLEFDE113:
.LLSFDE115:
	.uaword	.LLEFDE115-.LLASFDE115
.LLASFDE115:
	.uaword	.LLASFDE115-.LLframe1
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
.LLEFDE115:
.LLSFDE117:
	.uaword	.LLEFDE117-.LLASFDE117
.LLASFDE117:
	.uaword	.LLASFDE117-.LLframe1
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
.LLEFDE117:
.LLSFDE119:
	.uaword	.LLEFDE119-.LLASFDE119
.LLASFDE119:
	.uaword	.LLASFDE119-.LLframe1
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
.LLEFDE119:
.LLSFDE121:
	.uaword	.LLEFDE121-.LLASFDE121
.LLASFDE121:
	.uaword	.LLASFDE121-.LLframe1
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
.LLEFDE121:
.LLSFDE123:
	.uaword	.LLEFDE123-.LLASFDE123
.LLASFDE123:
	.uaword	.LLASFDE123-.LLframe1
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
.LLEFDE123:
.LLSFDE125:
	.uaword	.LLEFDE125-.LLASFDE125
.LLASFDE125:
	.uaword	.LLASFDE125-.LLframe1
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
.LLEFDE125:
.LLSFDE127:
	.uaword	.LLEFDE127-.LLASFDE127
.LLASFDE127:
	.uaword	.LLASFDE127-.LLframe1
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
.LLEFDE127:
.LLSFDE129:
	.uaword	.LLEFDE129-.LLASFDE129
.LLASFDE129:
	.uaword	.LLASFDE129-.LLframe1
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
.LLEFDE129:
.LLSFDE131:
	.uaword	.LLEFDE131-.LLASFDE131
.LLASFDE131:
	.uaword	.LLASFDE131-.LLframe1
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
.LLEFDE131:
.LLSFDE133:
	.uaword	.LLEFDE133-.LLASFDE133
.LLASFDE133:
	.uaword	.LLASFDE133-.LLframe1
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
.LLEFDE133:
	.ident	"GCC: (GNU) 4.5.1"
