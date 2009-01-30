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
 movl $0, %eax
 ret

 .global setcontext
 .global _setcontext
setcontext:
_setcontext:
 call _internal_setcontext
 movl 4(%esp), %eax
 movl %eax, %esp
 popal
 movl 12(%eax), %esp
 jmp *32(%eax)

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
