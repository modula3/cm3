	#.section .mdebug.abi64
	#.previous
	.abicalls
	.text
	.align	2
        .globl  internal_setcontext
	.globl  internal_setcontext_mips64
        .ent	internal_setcontext_mips64
internal_setcontext_mips64:
        .set	noat
	.set	noreorder
        .set	nomacro
	lui	$25,%hi(internal_setcontext)
	addiu	$25,$25,%lo(internal_setcontext)
	j	$25
        nop
	.end	internal_setcontext_mips64
        .size   internal_setcontext_mips64, .-internal_setcontext_mips64
