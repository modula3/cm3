 .file "contextasm.s"

 .text

 .global internal_endcontext
 .global _internal_endcontext
internal_endcontext:
_internal_endcontext:
 addl -4(%ebp), %esp
 ret
