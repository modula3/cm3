	.csect ._longjmp[PR]
	 .globl ._longjmp[PR]
	
	l       5, 8(3)
	l       1, 12(3)
	l	7, 248(3)
	st	7, 0(1)
	l       2, 16(3)
	bl	.jmprestfpr[PR]
	cmpi	0, 4, 0
	mtlr	5
	lm      13, 20(3)
	l	5, 96(3)
	mtcrf	0x38, 5
	mr	3, 4
	bne	0, _jmp1
	cal	3, 1(0)
_jmp1:
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0

	.csect .jmprestfpr[PR]
	 .globl .jmprestfpr[PR]

	lfd	14, 104 (3)
	lfd	15, 104 +  1 * 8 (3)
	lfd	16, 104 +  2 * 8 (3)
	lfd	17, 104 +  3 * 8 (3)
	lfd	18, 104 +  4 * 8 (3)
	lfd	19, 104 +  5 * 8 (3)
	lfd	20, 104 +  6 * 8 (3)
	lfd	21, 104 +  7 * 8 (3)
	lfd	22, 104 +  8 * 8 (3)
	lfd	23, 104 +  9 * 8 (3)
	lfd	24, 104 + 10 * 8 (3)
	lfd	25, 104 + 11 * 8 (3)
	lfd	26, 104 + 12 * 8 (3)
	lfd	27, 104 + 13 * 8 (3)
	lfd	28, 104 + 14 * 8 (3)
	lfd	29, 104 + 15 * 8 (3)
	lfd	30, 104 + 16 * 8 (3)
	lfd	31, 104 + 17 * 8 (3)

	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
