.text
	.align 2
	.globl _RTThread__Transfer
_RTThread__Transfer:
	mflr r0
	mfcr r12
	stfd f14,-144(r1)
	stfd f15,-136(r1)
	stfd f16,-128(r1)
	stfd f17,-120(r1)
	stfd f18,-112(r1)
	stfd f19,-104(r1)
	stfd f20,-96(r1)
	stfd f21,-88(r1)
	stfd f22,-80(r1)
	stfd f23,-72(r1)
	stfd f24,-64(r1)
	stfd f25,-56(r1)
	stfd f26,-48(r1)
	stfd f27,-40(r1)
	stfd f28,-32(r1)
	stfd f29,-24(r1)
	stfd f30,-16(r1)
	stfd f31,-8(r1)
	stmw r13,-220(r1)
	stw r0,8(r1)
	stw r12,4(r1)
	stwu r1,-288(r1)
	mr r30,r1
	stw r3,312(r30)
	stw r4,316(r30)
	lwz r3,312(r30)
	bl L_setjmp$stub
	mr r0,r3
	cmpwi cr0,r0,0
	bne cr0,L2
	lwz r3,316(r30)
	li r4,1
	bl L_longjmp$stub
L2:
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	lwz r12,4(r1)
	mtlr r0
	lmw r13,-220(r1)
	lfd f14,-144(r1)
	lfd f15,-136(r1)
	lfd f16,-128(r1)
	lfd f17,-120(r1)
	lfd f18,-112(r1)
	lfd f19,-104(r1)
	lfd f20,-96(r1)
	lfd f21,-88(r1)
	lfd f22,-80(r1)
	lfd f23,-72(r1)
	lfd f24,-64(r1)
	lfd f25,-56(r1)
	lfd f26,-48(r1)
	lfd f27,-40(r1)
	lfd f28,-32(r1)
	lfd f29,-24(r1)
	lfd f30,-16(r1)
	lfd f31,-8(r1)
	mtcrf 32,r12
	mtcrf 16,r12
	mtcrf 8,r12
	blr
	.globl _ThreadF__myId
.data
	.align 2
_ThreadF__myId:
	.long	1
	.globl _RT0u__inCritical
	.align 2
_RT0u__inCritical:
	.long	0
	.globl _RTThread__handlerStack
	.align 2
_RTThread__handlerStack:
	.long	0
.picsymbol_stub
L_longjmp$stub:
	.indirect_symbol _longjmp
	mflr r0
	bcl 20,31,L0$_longjmp
L0$_longjmp:
	mflr r11
	addis r11,r11,ha16(L_longjmp$lazy_ptr-L0$_longjmp)
	mtlr r0
	lwz r12,lo16(L_longjmp$lazy_ptr-L0$_longjmp)(r11)
	mtctr r12
	addi r11,r11,lo16(L_longjmp$lazy_ptr-L0$_longjmp)
	bctr
.data
.lazy_symbol_pointer
L_longjmp$lazy_ptr:
	.indirect_symbol _longjmp
	.long dyld_stub_binding_helper
.data
.picsymbol_stub
L_setjmp$stub:
	.indirect_symbol _setjmp
	mflr r0
	bcl 20,31,L0$_setjmp
L0$_setjmp:
	mflr r11
	addis r11,r11,ha16(L_setjmp$lazy_ptr-L0$_setjmp)
	mtlr r0
	lwz r12,lo16(L_setjmp$lazy_ptr-L0$_setjmp)(r11)
	mtctr r12
	addi r11,r11,lo16(L_setjmp$lazy_ptr-L0$_setjmp)
	bctr
.data
.lazy_symbol_pointer
L_setjmp$lazy_ptr:
	.indirect_symbol _setjmp
	.long dyld_stub_binding_helper
