#
# Setjmp, _setjmp, and __setjmp are broken in glibc 2.0.7.
# They share code with sigsetjmp which needs 2 arguments, and
# this does not work well on the i386 where the caller must
# pop the stack for the arguments when the call returns.
# Indeed, their setjmp adds the missing argument and jumps to
# sigsetjmp. Upon return, however, the caller pops one argument
# and the second argument remains on the stack, making the stack
# pointer 4 bytes away from its correct position.
#
# Below is a very simple reimplementation of _setjmp, the only function
# used within the Modula-3 runtime.
#
	.text
	.align 2
 
	.globl _setjmp
_setjmp:
	movl 4(%esp), %eax		# address of env argument in %eax
	movl %ebx, 0(%eax)		# save %ebx in jmp_buf
	movl %esi, 4(%eax)		# save %esi
	movl %edi, 8(%eax)		# save %edi
	movl %ebp, 12(%eax)		# save %ebp
	leal 4(%esp), %ecx		# %esp before the call in %ecx
     	movl %ecx, 16(%eax)		# save %esp (as was before the call)
	movl 0(%esp), %ecx		# saved PC, where longjmp will return 
     	movl %ecx, 20(%eax)		# save PC
        movl $0x0, 24(%eax)		# note that signal masks not saved
	xorl %eax, %eax			# return 0
	ret


