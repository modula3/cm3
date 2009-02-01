 .file "contextasm.s"

 .text

 .global getcontext
 .global _getcontext
getcontext:
_getcontext:
 pushal
 pushl $1
 call _internal_getcontext
 addl $36, %esp
 xor %eax, %eax
 ret

 .global setcontext
 .global _setcontext
setcontext:
_setcontext:
 call _internal_setcontext
 movl 4(%esp), %eax
 movl 16(%eax), %edi
 movl 20(%eax), %esi
 movl 24(%eax), %ebp
 movl 28(%eax), %ebx
 movl 32(%eax), %edx
 movl 36(%eax), %ecx
 movl 56(%eax), %esp
 pushl 44(%eax)
 pushl 40(%eax)
 popl %eax
 retl

 .global internal_endcontext
 .global _internal_endcontext
internal_endcontext:
_internal_endcontext:
 addl -4(%ebp), %esp
 ret

 .global swapcontext
 .global _swapcontext
swapcontext:
_swapcontext:
 pushal
 pushl $0
 call _internal_getcontext
 addl $40, %esp
 jmp setcontext
