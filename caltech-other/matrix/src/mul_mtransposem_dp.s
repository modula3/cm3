# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "mul_mtransposem_dp.f"
	.text
..TXTST0:
# -- Begin  mul_mtransposem_dp_
# mark_begin;
       .align    2,0x90
	.globl mul_mtransposem_dp_
mul_mtransposem_dp_:
# parameter 1: 76 + %esp
# parameter 2: 80 + %esp
# parameter 3: 84 + %esp
# parameter 4: 88 + %esp
# parameter 5: 92 + %esp
# parameter 6: 96 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %esi                                          #4.18
        pushl     %ebx                                          #4.18
        subl      $60, %esp                                     #4.18
        movl      80(%esp), %edx                                #4.18
        movl      84(%esp), %esi                                #4.18
        movl      92(%esp), %ecx                                #4.18
        movl      (%ecx), %ebx                                  #4.18
        movl      96(%esp), %eax                                #4.18
        movl      (%eax), %ecx                                  #4.18
        movl      %ebx, 12(%esp)                                #4.18
        testl     %ebx, %ebx                                    #10.10
        jle       ..B1.22       # Prob 1%                       #10.10
                                # LOE edx ecx ebp esi
..B1.2:                         # Preds ..B1.1
        xorl      %eax, %eax                                    #
        movl      %ecx, %ebx                                    #12.13
        shll      $3, %ebx                                      #12.13
        movl      %ebx, 24(%esp)                                #
        movl      %esi, %edi                                    #
        subl      %ebx, %edi                                    #
        movl      %edi, 40(%esp)                                #
                                # LOE eax edx ecx ebp esi
..B1.3:                         # Preds ..B1.19 ..B1.2
        testl     %ecx, %ecx                                    #11.13
        jle       ..B1.64       # Prob 50%                      #11.13
                                # LOE eax ecx ebp
..B1.4:                         # Preds ..B1.3
        movl      24(%esp), %ebx                                #
        movl      40(%esp), %edx                                #11.13
        addl      $1, %eax                                      #
        imull     %eax, %ebx                                    #
        lea       (%edx,%ebx), %edx                             #11.13
        andl      $15, %edx                                     #11.13
        je        ..B1.7        # Prob 50%                      #11.13
                                # LOE eax edx ecx ebx ebp
..B1.5:                         # Preds ..B1.4
        testb     $7, %dl                                       #11.13
        jne       ..B1.52       # Prob 10%                      #11.13
                                # LOE eax ecx ebx ebp
..B1.6:                         # Preds ..B1.5
        movl      $1, %edx                                      #11.13
                                # LOE eax edx ecx ebx ebp
..B1.7:                         # Preds ..B1.6 ..B1.4
        lea       8(%edx), %esi                                 #11.13
        cmpl      %esi, %ecx                                    #11.13
        jl        ..B1.52       # Prob 10%                      #11.13
                                # LOE eax edx ecx ebx ebp
..B1.8:                         # Preds ..B1.7
        movl      %ecx, %esi                                    #11.13
        subl      %edx, %esi                                    #11.13
        andl      $7, %esi                                      #11.13
        negl      %esi                                          #11.13
        addl      %ecx, %esi                                    #11.13
        testl     %edx, %edx                                    #11.13
        jbe       ..B1.53       # Prob 1%                       #11.13
                                # LOE eax edx ecx ebx ebp esi
..B1.9:                         # Preds ..B1.8
        movl      40(%esp), %edi                                #
        movl      %ecx, 32(%esp)                                #12.13
        lea       (%ebx,%edi), %edi                             #
        movl      %edi, 36(%esp)                                #
        movl      %edi, 8(%esp)                                 #
        movl      %edi, %ecx                                    #12.13
        pxor      %xmm0, %xmm0                                  #12.13
        lea       (%edi,%edx,8), %edi                           #12.13
        movl      %edi, 52(%esp)                                #12.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.10:                        # Preds ..B1.10 ..B1.9
        movsd     %xmm0, (%ecx)                                 #12.13
        addl      $8, %ecx                                      #11.13
        cmpl      %edi, %ecx                                    #11.13
        jb        ..B1.10       # Prob 99%                      #11.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.11:                        # Preds ..B1.10
        movl      32(%esp), %ecx                                #
                                # LOE eax edx ecx ebx ebp esi
..B1.12:                        # Preds ..B1.11 ..B1.53
        movl      36(%esp), %edi                                #12.13
        pxor      %xmm0, %xmm0                                  #12.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.13:                        # Preds ..B1.13 ..B1.12
        movaps    %xmm0, (%edi,%edx,8)                          #12.13
        movaps    %xmm0, 16(%edi,%edx,8)                        #12.13
        movaps    %xmm0, 32(%edi,%edx,8)                        #12.13
        movaps    %xmm0, 48(%edi,%edx,8)                        #12.13
        addl      $8, %edx                                      #11.13
        cmpl      %esi, %edx                                    #11.13
        jb        ..B1.13       # Prob 99%                      #11.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.15:                        # Preds ..B1.13 ..B1.52
        cmpl      %ecx, %esi                                    #11.13
        jae       ..B1.19       # Prob 1%                       #11.13
                                # LOE eax ecx ebx ebp esi
..B1.16:                        # Preds ..B1.15
        addl      40(%esp), %ebx                                #
        pxor      %xmm0, %xmm0                                  #12.13
                                # LOE eax ecx ebx ebp esi xmm0
..B1.17:                        # Preds ..B1.17 ..B1.16
        movsd     %xmm0, (%ebx,%esi,8)                          #12.13
        addl      $1, %esi                                      #11.13
        cmpl      %ecx, %esi                                    #11.13
        jb        ..B1.17       # Prob 99%                      #11.13
                                # LOE eax ecx ebx ebp esi xmm0
..B1.19:                        # Preds ..B1.17 ..B1.15
        movl      12(%esp), %edx                                #10.10
        cmpl      %edx, %eax                                    #10.10
        jb        ..B1.3        # Prob 99%                      #10.10
                                # LOE eax ecx ebp
..B1.64:                        # Preds ..B1.19 ..B1.3
        movl      84(%esp), %esi                                #
        movl      80(%esp), %edx                                #
                                # LOE edx ecx ebp esi
..B1.22:                        # Preds ..B1.64 ..B1.1
        movl      88(%esp), %eax                                #16.10
        movl      (%eax), %ebx                                  #16.10
        movl      %ebx, 8(%esp)                                 #16.10
        testl     %ebx, %ebx                                    #16.10
        jle       ..B1.51       # Prob 1%                       #16.10
                                # LOE edx ecx ebp esi
..B1.23:                        # Preds ..B1.22
        movl      12(%esp), %ebx                                #19.41
        movl      76(%esp), %edi                                #
        movl      %ecx, 32(%esp)                                #
        xorl      %eax, %eax                                    #
        movl      %eax, 52(%esp)                                #
        movl      %ecx, %eax                                    #12.13
        movl      52(%esp), %ecx                                #
        shll      $3, %eax                                      #12.13
        subl      %eax, %edx                                    #
        subl      %eax, %esi                                    #
        movl      %esi, 28(%esp)                                #
        shll      $3, %ebx                                      #19.41
        movl      %ebx, 20(%esp)                                #
        subl      %ebx, %edi                                    #
        movl      %edi, 16(%esp)                                #
                                # LOE eax edx ecx ebp
..B1.24:                        # Preds ..B1.49 ..B1.23
        movl      12(%esp), %ebx                                #17.13
        testl     %ebx, %ebx                                    #17.13
        jle       ..B1.51       # Prob 1%                       #17.13
                                # LOE eax edx ecx ebp
..B1.25:                        # Preds ..B1.24
        movl      %edx, 44(%esp)                                #
        movl      %eax, %ebx                                    #
        movl      %eax, 48(%esp)                                #
        movl      32(%esp), %eax                                #
        xorl      %esi, %esi                                    #
        addl      $1, %ecx                                      #
        imull     %ecx, %ebx                                    #
        lea       (%edx,%ebx), %edi                             #
        movl      %edi, 56(%esp)                                #
        movl      %ebx, 40(%esp)                                #
        movl      %esi, 36(%esp)                                #
        movl      %ecx, 52(%esp)                                #
                                # LOE eax ebp
..B1.26:                        # Preds ..B1.46 ..B1.25
        testl     %eax, %eax                                    #18.16
        jle       ..B1.62       # Prob 50%                      #18.16
                                # LOE eax ebp
..B1.27:                        # Preds ..B1.26
        movl      36(%esp), %ebx                                #
        movl      48(%esp), %edx                                #
        movl      28(%esp), %ecx                                #18.16
        lea       1(%ebx), %ebx                                 #
        imull     %ebx, %edx                                    #
        movl      %edx, 24(%esp)                                #
        lea       (%ecx,%edx), %edx                             #18.16
        andl      $15, %edx                                     #18.16
        je        ..B1.30       # Prob 50%                      #18.16
                                # LOE eax edx ebx ebp
..B1.28:                        # Preds ..B1.27
        testb     $7, %dl                                       #18.16
        jne       ..B1.55       # Prob 10%                      #18.16
                                # LOE eax ebx ebp
..B1.29:                        # Preds ..B1.28
        movl      $1, %edx                                      #18.16
                                # LOE eax edx ebx ebp
..B1.30:                        # Preds ..B1.29 ..B1.27
        lea       8(%edx), %ecx                                 #18.16
        cmpl      %ecx, %eax                                    #18.16
        jl        ..B1.55       # Prob 10%                      #18.16
                                # LOE eax edx ebx ebp
..B1.31:                        # Preds ..B1.30
        movl      %eax, %ecx                                    #18.16
        subl      %edx, %ecx                                    #18.16
        andl      $7, %ecx                                      #18.16
        negl      %ecx                                          #18.16
        addl      %eax, %ecx                                    #18.16
        testl     %edx, %edx                                    #18.16
        jbe       ..B1.56       # Prob 1%                       #18.16
                                # LOE eax edx ecx ebx ebp
..B1.32:                        # Preds ..B1.31
        movl      28(%esp), %edi                                #
        movl      %eax, 32(%esp)                                #19.41
        xorl      %esi, %esi                                    #
        movl      %esi, 4(%esp)                                 #
        movl      24(%esp), %esi                                #
        lea       (%edi,%esi), %esi                             #
        movl      36(%esp), %edi                                #19.41
        movl      %esi, (%esp)                                  #
        movl      52(%esp), %esi                                #
        movl      (%esp), %eax                                  #19.41
        imull     20(%esp), %esi                                #
        addl      16(%esp), %esi                                #
        movsd     (%esi,%edi,8), %xmm0                          #19.41
        movl      4(%esp), %edi                                 #19.41
        movl      56(%esp), %esi                                #19.41
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.33:                        # Preds ..B1.33 ..B1.32
        movsd     (%esi,%edi,8), %xmm1                          #19.55
        mulsd     %xmm0, %xmm1                                  #19.53
        addsd     (%eax,%edi,8), %xmm1                          #19.16
        movsd     %xmm1, (%eax,%edi,8)                          #19.16
        addl      $1, %edi                                      #18.16
        cmpl      %edx, %edi                                    #18.16
        jb        ..B1.33       # Prob 99%                      #18.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.34:                        # Preds ..B1.33
        movl      32(%esp), %eax                                #
        movl      %esi, 56(%esp)                                #
                                # LOE eax edx ecx ebx ebp xmm0
..B1.35:                        # Preds ..B1.34 ..B1.56
        movl      40(%esp), %edi                                #18.16
        movl      44(%esp), %esi                                #18.16
        lea       (%esi,%edi), %esi                             #18.16
        lea       (%esi,%edx,8), %esi                           #18.16
        testl     $15, %esi                                     #18.16
        unpcklpd  %xmm0, %xmm0                                  #19.41
        je        ..B1.39       # Prob 60%                      #18.16
                                # LOE eax edx ecx ebx ebp xmm0
..B1.36:                        # Preds ..B1.35
        movl      (%esp), %esi                                  #
        movl      56(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.37:                        # Preds ..B1.37 ..B1.36
        movsd     (%edi,%edx,8), %xmm1                          #19.55
        movhpd    8(%edi,%edx,8), %xmm1                         #19.55
        mulpd     %xmm0, %xmm1                                  #19.53
        addpd     (%esi,%edx,8), %xmm1                          #19.16
        movsd     16(%edi,%edx,8), %xmm2                        #19.55
        movhpd    24(%edi,%edx,8), %xmm2                        #19.55
        movsd     32(%edi,%edx,8), %xmm3                        #19.55
        movhpd    40(%edi,%edx,8), %xmm3                        #19.55
        movsd     48(%edi,%edx,8), %xmm4                        #19.55
        movhpd    56(%edi,%edx,8), %xmm4                        #19.55
        movaps    %xmm1, (%esi,%edx,8)                          #19.16
        mulpd     %xmm0, %xmm2                                  #19.53
        mulpd     %xmm0, %xmm3                                  #19.53
        mulpd     %xmm0, %xmm4                                  #19.53
        addpd     16(%esi,%edx,8), %xmm2                        #19.16
        addpd     32(%esi,%edx,8), %xmm3                        #19.16
        addpd     48(%esi,%edx,8), %xmm4                        #19.16
        movaps    %xmm2, 16(%esi,%edx,8)                        #19.16
        movaps    %xmm3, 32(%esi,%edx,8)                        #19.16
        movaps    %xmm4, 48(%esi,%edx,8)                        #19.16
        addl      $8, %edx                                      #18.16
        cmpl      %ecx, %edx                                    #18.16
        jb        ..B1.37       # Prob 99%                      #18.16
        jmp       ..B1.41       # Prob 100%                     #18.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.39:                        # Preds ..B1.35
        movl      (%esp), %esi                                  #
        movl      56(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.40:                        # Preds ..B1.40 ..B1.39
        movaps    (%edi,%edx,8), %xmm1                          #19.55
        mulpd     %xmm0, %xmm1                                  #19.53
        addpd     (%esi,%edx,8), %xmm1                          #19.16
        movaps    16(%edi,%edx,8), %xmm2                        #19.55
        movaps    32(%edi,%edx,8), %xmm3                        #19.55
        movaps    48(%edi,%edx,8), %xmm4                        #19.55
        movaps    %xmm1, (%esi,%edx,8)                          #19.16
        mulpd     %xmm0, %xmm2                                  #19.53
        mulpd     %xmm0, %xmm3                                  #19.53
        mulpd     %xmm0, %xmm4                                  #19.53
        addpd     16(%esi,%edx,8), %xmm2                        #19.16
        addpd     32(%esi,%edx,8), %xmm3                        #19.16
        addpd     48(%esi,%edx,8), %xmm4                        #19.16
        movaps    %xmm2, 16(%esi,%edx,8)                        #19.16
        movaps    %xmm3, 32(%esi,%edx,8)                        #19.16
        movaps    %xmm4, 48(%esi,%edx,8)                        #19.16
        addl      $8, %edx                                      #18.16
        cmpl      %ecx, %edx                                    #18.16
        jb        ..B1.40       # Prob 99%                      #18.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.41:                        # Preds ..B1.37 ..B1.40
        movl      %edi, 56(%esp)                                #
                                # LOE eax ecx ebx ebp
..B1.42:                        # Preds ..B1.41 ..B1.55
        cmpl      %eax, %ecx                                    #18.16
        jae       ..B1.46       # Prob 1%                       #18.16
                                # LOE eax ecx ebx ebp
..B1.43:                        # Preds ..B1.42
        movl      24(%esp), %edx                                #
        addl      28(%esp), %edx                                #
        movl      52(%esp), %esi                                #
        movl      36(%esp), %edi                                #19.41
        movl      %edx, 24(%esp)                                #
        imull     20(%esp), %esi                                #
        addl      16(%esp), %esi                                #
        movsd     (%esi,%edi,8), %xmm0                          #19.41
        movl      56(%esp), %esi                                #19.41
                                # LOE eax edx ecx ebx ebp esi xmm0
..B1.44:                        # Preds ..B1.44 ..B1.43
        movsd     (%esi,%ecx,8), %xmm1                          #19.55
        mulsd     %xmm0, %xmm1                                  #19.53
        addsd     (%edx,%ecx,8), %xmm1                          #19.16
        movsd     %xmm1, (%edx,%ecx,8)                          #19.16
        addl      $1, %ecx                                      #18.16
        cmpl      %eax, %ecx                                    #18.16
        jb        ..B1.44       # Prob 99%                      #18.16
                                # LOE eax edx ecx ebx ebp esi xmm0
..B1.45:                        # Preds ..B1.44
        movl      %esi, 56(%esp)                                #
                                # LOE eax ebx ebp
..B1.46:                        # Preds ..B1.45 ..B1.42
        movl      12(%esp), %edx                                #17.13
        movl      %ebx, 36(%esp)                                #17.13
        cmpl      %edx, %ebx                                    #17.13
        jb        ..B1.26       # Prob 99%                      #17.13
                                # LOE eax ebp
..B1.62:                        # Preds ..B1.46 ..B1.26
        movl      44(%esp), %edx                                #
        movl      52(%esp), %ecx                                #
        movl      %eax, 32(%esp)                                #
        movl      48(%esp), %eax                                #
                                # LOE eax edx ecx ebp al dl cl ah dh ch
..B1.49:                        # Preds ..B1.62
        movl      8(%esp), %ebx                                 #16.10
        cmpl      %ebx, %ecx                                    #16.10
        jb        ..B1.24       # Prob 99%                      #16.10
                                # LOE eax edx ecx ebp
..B1.51:                        # Preds ..B1.24 ..B1.49 ..B1.22
        addl      $60, %esp                                     #24.7
        popl      %ebx                                          #24.7
        popl      %esi                                          #24.7
        popl      %edi                                          #24.7
        ret                                                     #24.7
                                # LOE
..B1.52:                        # Preds ..B1.7 ..B1.5           # Infreq
        xorl      %esi, %esi                                    #11.13
        jmp       ..B1.15       # Prob 100%                     #11.13
                                # LOE eax ecx ebx ebp esi
..B1.53:                        # Preds ..B1.8                  # Infreq
        movl      40(%esp), %edi                                #
        lea       (%ebx,%edi), %edi                             #
        movl      %edi, 36(%esp)                                #
        jmp       ..B1.12       # Prob 100%                     #
                                # LOE eax edx ecx ebx ebp esi
..B1.55:                        # Preds ..B1.30 ..B1.28         # Infreq
        xorl      %ecx, %ecx                                    #18.16
        jmp       ..B1.42       # Prob 100%                     #18.16
                                # LOE eax ecx ebx ebp
..B1.56:                        # Preds ..B1.31                 # Infreq
        movl      28(%esp), %esi                                #
        movl      24(%esp), %edi                                #
        lea       (%esi,%edi), %esi                             #
        movl      36(%esp), %edi                                #19.41
        movl      %esi, (%esp)                                  #
        movl      52(%esp), %esi                                #
        imull     20(%esp), %esi                                #
        addl      16(%esp), %esi                                #
        movsd     (%esi,%edi,8), %xmm0                          #19.41
        jmp       ..B1.35       # Prob 100%                     #19.41
        .align    2,0x90
                                # LOE eax edx ecx ebx ebp xmm0
# mark_end;
	.type	mul_mtransposem_dp_,@function
	.size	mul_mtransposem_dp_,.-mul_mtransposem_dp_
	.data
# -- End  mul_mtransposem_dp_
	.data
	.section .note.GNU-stack, ""
# End
