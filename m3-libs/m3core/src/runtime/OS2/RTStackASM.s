	.text
	.align 2

	.globl _RTStack__CurrentFrame
_RTStack__CurrentFrame:
		                      # STACK = ret frame
	popl %eax                     # STACK = frame
	pushl %eax
	pushl %eax                    # STACK = PC ret frame
	movl %esp, %eax            
	addl $8, %eax                 # correct for the thing we have pushed
				      # and our return address
	pushl %eax                    # STACK = SP PC ret frame
	movl 12(%esp), %eax           # grab the address were to put the values
	popl 8(%eax)                  # stuff our caller SP
	popl 0(%eax)                  # stuff our caller PC
	movl %ebp, 4(%eax)            # stuff our caller BP
	ret

	.globl _RTStack__PreviousFrame
_RTStack__PreviousFrame:
	movl 4(%esp), %eax            # eax = ^callee
	movl 4(%eax), %eax            # eax = callee.ebp
	pushl 0(%eax)                 # push bp of caller
	pushl 4(%eax)                 # push pc of caller
	addl $4, %eax
	pushl %eax		      # push sp of caller
	movl 20(%esp), %eax
	popl 8(%eax)
	popl 0(%eax)
	popl 4(%eax)
	ret

	.globl _RTStack__Unwind
_RTStack__Unwind:
	movl 4(%esp), %eax            # eax = ^to
	movl 8(%eax), %esp
	movl 4(%eax), %ebp
	jmp *(%eax)

