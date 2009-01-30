 .file "contextasm.s"

 .text

 .globl getcontext
 .type getcontext, @function
getcontext:
 pushal
 pushl $1
 call internal_getcontext
 addl $36, %esp
 movl $0, %eax
 ret
 .size getcontext, .-getcontext

 .globl setcontext
 .type setcontext, @function
setcontext:
 call internal_setcontext
 movl 4(%esp), %eax
 movl %eax, %esp
 popal
 movl 12(%eax), %esp
 jmp *32(%eax)
 .size setcontext, .-setcontext

 .globl internal_endcontext
 .type internal_endcontext, @function
internal_endcontext:
 addl -4(%ebp), %esp
 ret
 .size internal_endcontext, .-internal_endcontext

 .globl swapcontext
 .type swapcontext, @function
swapcontext:
 pushal
 pushl $0
 call internal_getcontext
 addl $40, %esp
 jmp setcontext
 .size swapcontext, .-swapcontext
