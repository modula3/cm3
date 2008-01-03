	.file	"setjmp.s"
	.section	.text
	.globl	_setjmp
	.type	_setjmp,"function"
_setjmp:
	st.l	%r1,0(%r16)
	st.l	%sp,4(%r16)
	st.l	%fp,8(%r16)
	st.l	%r4,12(%r16)
	st.l	%r5,16(%r16)
	st.l	%r6,20(%r16)
	st.l	%r7,24(%r16)
	st.l	%r8,28(%r16)
	st.l	%r9,32(%r16)
	st.l	%r10,36(%r16)
	st.l	%r11,40(%r16)
	st.l	%r12,44(%r16)
	st.l	%r13,48(%r16)
	st.l	%r14,52(%r16)
	st.l	%r15,56(%r16)
	fst.l	%f2,60(%r16)
	fst.l	%f3,64(%r16)
	fst.l	%f4,68(%r16)
	fst.l	%f5,72(%r16)
	fst.l	%f6,76(%r16)
	fst.l	%f7,80(%r16)
	bri	%r1
	mov	%r0,%r16

	.globl	_longjmp
	.type	_longjmp,"function"
_longjmp:
	ld.l	0(%r16),%r1
	ld.l	4(%r16),%sp
	ld.l	8(%r16),%fp
	ld.l	12(%r16),%r4
	ld.l	16(%r16),%r5
	ld.l	20(%r16),%r6
	ld.l	24(%r16),%r7
	ld.l	28(%r16),%r8
	ld.l	32(%r16),%r9
	ld.l	36(%r16),%r10
	ld.l	40(%r16),%r11
	ld.l	44(%r16),%r12
	ld.l	48(%r16),%r13
	ld.l	52(%r16),%r14
	ld.l	56(%r16),%r15
	fld.l	60(%r16),%f2
	fld.l	64(%r16),%f3
	fld.l	68(%r16),%f4
	fld.l	72(%r16),%f5
	fld.l	76(%r16),%f6
	fld.l	80(%r16),%f7
	btne	%r0,%r17,L1
	or	0x1,%r0,%r17
L1:
	bri	%r1
	mov	%r17,%r16


