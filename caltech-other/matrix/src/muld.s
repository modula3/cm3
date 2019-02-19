# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "muld.f"
	.text
..TXTST0:
# -- Begin  muld_
# mark_begin;
       .align    2,0x90
	.globl muld_
muld_:
# parameter 1: 96 + %esp
# parameter 2: 100 + %esp
# parameter 3: 104 + %esp
# parameter 4: 108 + %esp
# parameter 5: 112 + %esp
# parameter 6: 116 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %esi                                          #4.18
        pushl     %ebx                                          #4.18
        subl      $80, %esp                                     #4.18
        movl      100(%esp), %edx                               #4.18
        movl      104(%esp), %ecx                               #4.18
        movl      108(%esp), %ebx                               #4.18
        movl      (%ebx), %ebx                                  #15.10
        movl      112(%esp), %eax                               #4.18
        movl      116(%esp), %esi                               #4.18
        movl      (%eax), %eax                                  #4.18
        movl      %eax, 28(%esp)                                #4.18
        movl      (%esi), %eax                                  #4.18
        movl      %ebx, (%esp)                                  #15.10
        testl     %ebx, %ebx                                    #15.10
        jle       ..B1.22       # Prob 1%                       #15.10
                                # LOE eax edx ecx ebp
..B1.2:                         # Preds ..B1.1
        xorl      %ebx, %ebx                                    #
        movl      %eax, %esi                                    #17.13
        shll      $3, %esi                                      #17.13
        movl      %esi, 48(%esp)                                #17.13
        movl      %ecx, %edi                                    #
        subl      %esi, %edi                                    #
        movl      %edi, 24(%esp)                                #
                                # LOE eax edx ecx ebx ebp
..B1.3:                         # Preds ..B1.19 ..B1.2
        testl     %eax, %eax                                    #16.13
        jle       ..B1.61       # Prob 50%                      #16.13
                                # LOE eax ebx ebp
..B1.4:                         # Preds ..B1.3
        movl      48(%esp), %ecx                                #
        movl      24(%esp), %edx                                #16.13
        addl      $1, %ebx                                      #
        imull     %ebx, %ecx                                    #
        lea       (%edx,%ecx), %edx                             #16.13
        andl      $15, %edx                                     #16.13
        je        ..B1.7        # Prob 50%                      #16.13
                                # LOE eax edx ecx ebx ebp
..B1.5:                         # Preds ..B1.4
        testb     $7, %dl                                       #16.13
        jne       ..B1.49       # Prob 10%                      #16.13
                                # LOE eax ecx ebx ebp
..B1.6:                         # Preds ..B1.5
        movl      $1, %edx                                      #16.13
                                # LOE eax edx ecx ebx ebp
..B1.7:                         # Preds ..B1.6 ..B1.4
        lea       8(%edx), %esi                                 #16.13
        cmpl      %esi, %eax                                    #16.13
        jl        ..B1.49       # Prob 10%                      #16.13
                                # LOE eax edx ecx ebx ebp
..B1.8:                         # Preds ..B1.7
        movl      %eax, %esi                                    #16.13
        subl      %edx, %esi                                    #16.13
        andl      $7, %esi                                      #16.13
        negl      %esi                                          #16.13
        addl      %eax, %esi                                    #16.13
        testl     %edx, %edx                                    #16.13
        jbe       ..B1.48       # Prob 1%                       #16.13
                                # LOE eax edx ecx ebx ebp esi
..B1.9:                         # Preds ..B1.8
        movl      24(%esp), %edi                                #
        movl      %eax, 4(%esp)                                 #17.13
        lea       (%edi,%ecx), %edi                             #
        movl      %edi, 20(%esp)                                #
        movl      %edi, 40(%esp)                                #
        movl      %edi, %eax                                    #17.13
        pxor      %xmm0, %xmm0                                  #17.13
        lea       (%edi,%edx,8), %edi                           #17.13
        movl      %edi, 36(%esp)                                #17.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.10:                        # Preds ..B1.10 ..B1.9
        movsd     %xmm0, (%eax)                                 #17.13
        addl      $8, %eax                                      #16.13
        cmpl      %edi, %eax                                    #16.13
        jb        ..B1.10       # Prob 99%                      #16.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.11:                        # Preds ..B1.10
        movl      4(%esp), %eax                                 #
                                # LOE eax edx ecx ebx ebp esi
..B1.12:                        # Preds ..B1.11 ..B1.48
        movl      20(%esp), %edi                                #17.13
        pxor      %xmm0, %xmm0                                  #17.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.13:                        # Preds ..B1.13 ..B1.12
        movaps    %xmm0, (%edi,%edx,8)                          #17.13
        movaps    %xmm0, 16(%edi,%edx,8)                        #17.13
        movaps    %xmm0, 32(%edi,%edx,8)                        #17.13
        movaps    %xmm0, 48(%edi,%edx,8)                        #17.13
        addl      $8, %edx                                      #16.13
        cmpl      %esi, %edx                                    #16.13
        jb        ..B1.13       # Prob 99%                      #16.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.15:                        # Preds ..B1.13 ..B1.49
        cmpl      %eax, %esi                                    #16.13
        jae       ..B1.19       # Prob 1%                       #16.13
                                # LOE eax ecx ebx ebp esi
..B1.16:                        # Preds ..B1.15
        addl      24(%esp), %ecx                                #
        pxor      %xmm0, %xmm0                                  #17.13
                                # LOE eax ecx ebx ebp esi xmm0
..B1.17:                        # Preds ..B1.17 ..B1.16
        movsd     %xmm0, (%ecx,%esi,8)                          #17.13
        addl      $1, %esi                                      #16.13
        cmpl      %eax, %esi                                    #16.13
        jb        ..B1.17       # Prob 99%                      #16.13
                                # LOE eax ecx ebx ebp esi xmm0
..B1.19:                        # Preds ..B1.17 ..B1.15
        movl      (%esp), %edx                                  #15.10
        cmpl      %edx, %ebx                                    #15.10
        jb        ..B1.3        # Prob 99%                      #15.10
                                # LOE eax ebx ebp
..B1.61:                        # Preds ..B1.19 ..B1.3
        movl      104(%esp), %ecx                               #
        movl      100(%esp), %edx                               #
                                # LOE eax edx ecx ebp
..B1.22:                        # Preds ..B1.61 ..B1.1
        movl      (%esp), %ebx                                  #15.10
        testl     %ebx, %ebx                                    #15.10
        jle       ..B1.47       # Prob 1%                       #15.10
                                # LOE eax edx ecx ebp
..B1.23:                        # Preds ..B1.22
        movl      96(%esp), %esi                                #
        movl      %eax, 4(%esp)                                 #
        xorl      %edi, %edi                                    #
        movl      %eax, %ebx                                    #17.13
        movl      %edi, 40(%esp)                                #
        movl      28(%esp), %edi                                #19.42
        movl      40(%esp), %eax                                #
        shll      $3, %edi                                      #19.42
        subl      %edi, %esi                                    #
        shll      $3, %ebx                                      #17.13
        movl      %ebx, 52(%esp)                                #
        subl      %ebx, %edx                                    #
        movl      %edx, 12(%esp)                                #
        subl      %ebx, %ecx                                    #
        movl      %ecx, 8(%esp)                                 #
                                # LOE eax ebp esi edi
..B1.24:                        # Preds ..B1.54 ..B1.45 ..B1.23
        movl      4(%esp), %edx                                 #16.13
        testl     %edx, %edx                                    #16.13
        jle       ..B1.54       # Prob 1%                       #16.13
                                # LOE eax ebp esi edi
..B1.25:                        # Preds ..B1.24
        movl      %esi, 32(%esp)                                #
        movl      %edi, %edx                                    #
        movl      %edi, 36(%esp)                                #
        xorl      %ecx, %ecx                                    #
        addl      $1, %eax                                      #
        imull     %eax, %edx                                    #
        lea       (%esi,%edx), %ebx                             #
        movl      %ebx, 44(%esp)                                #
        movl      %edx, 24(%esp)                                #
        movl      %ecx, 20(%esp)                                #
        movl      %eax, 40(%esp)                                #
                                # LOE ebp
..B1.26:                        # Preds ..B1.42 ..B1.25
        movl      28(%esp), %eax                                #18.16
        testl     %eax, %eax                                    #18.16
        jle       ..B1.59       # Prob 50%                      #18.16
                                # LOE ebp
..B1.27:                        # Preds ..B1.26
        movl      32(%esp), %eax                                #18.16
        movl      24(%esp), %edx                                #18.16
        lea       (%eax,%edx), %ecx                             #18.16
        andl      $15, %ecx                                     #18.16
        je        ..B1.30       # Prob 50%                      #18.16
                                # LOE ecx ebp
..B1.28:                        # Preds ..B1.27
        testb     $7, %cl                                       #18.16
        jne       ..B1.50       # Prob 10%                      #18.16
                                # LOE ebp
..B1.29:                        # Preds ..B1.28
        movl      $1, %ecx                                      #18.16
                                # LOE ecx ebp
..B1.30:                        # Preds ..B1.29 ..B1.27
        movl      28(%esp), %eax                                #18.16
        lea       8(%ecx), %edx                                 #18.16
        cmpl      %edx, %eax                                    #18.16
        jl        ..B1.50       # Prob 10%                      #18.16
                                # LOE eax ecx ebp al ah
..B1.31:                        # Preds ..B1.30
        movl      %eax, %edx                                    #18.16
        subl      %ecx, %edx                                    #18.16
        andl      $7, %edx                                      #18.16
        negl      %edx                                          #18.16
        addl      %eax, %edx                                    #18.16
        movl      %edx, 64(%esp)                                #18.16
        testl     %ecx, %ecx                                    #18.16
        jbe       ..B1.51       # Prob 1%                       #18.16
                                # LOE ecx ebp
..B1.32:                        # Preds ..B1.31
        movl      40(%esp), %edx                                #
        movl      20(%esp), %eax                                #
        movl      12(%esp), %edi                                #
        imull     52(%esp), %edx                                #
        movl      %eax, %esi                                    #
        shll      $3, %esi                                      #
        addl      8(%esp), %edx                                 #
        movl      %esi, 68(%esp)                                #
        lea       (%edx,%eax,8), %ebx                           #
        movl      44(%esp), %esi                                #
        movsd     (%ebx), %xmm0                                 #19.29
        movl      %ebx, 16(%esp)                                #
        xorl      %edx, %edx                                    #
        lea       (%edi,%eax,8), %eax                           #
        movl      52(%esp), %edi                                #
                                # LOE eax edx ecx ebp esi edi xmm0
..B1.33:                        # Preds ..B1.33 ..B1.32
        movsd     (%esi,%edx,8), %xmm1                          #19.42
        lea       1(%edx), %ebx                                 #
        movl      %edi, %edx                                    #19.16
        imull     %ebx, %edx                                    #19.16
        mulsd     (%edx,%eax), %xmm1                            #19.54
        movl      %ebx, %edx                                    #18.16
        cmpl      %ecx, %ebx                                    #18.16
        addsd     %xmm1, %xmm0                                  #19.16
        jb        ..B1.33       # Prob 99%                      #18.16
                                # LOE eax edx ecx ebp esi edi xmm0
..B1.34:                        # Preds ..B1.33
        movl      16(%esp), %ebx                                #
        movsd     %xmm0, (%ebx)                                 #19.29
        movl      %esi, 44(%esp)                                #
        movl      68(%esp), %esi                                #
        movl      %edi, 52(%esp)                                #
                                # LOE ecx ebx ebp esi xmm0
..B1.35:                        # Preds ..B1.34 ..B1.51
        addl      12(%esp), %esi                                #
        movl      52(%esp), %edi                                #
        movl      %ebx, 16(%esp)                                #
        movl      %esi, 68(%esp)                                #
        movl      $-2147483647, %eax                            #
        imull     %edi                                          #
        lea       3(%ecx), %eax                                 #
        pxor      %xmm1, %xmm1                                  #19.16
        addl      %edi, %edx                                    #
        sarl      $2, %edx                                      #
        sarl      $31, %edi                                     #
        subl      %edi, %edx                                    #
        movl      %edx, 48(%esp)                                #
        lea       1(%ecx), %edx                                 #
        lea       5(%ecx), %edi                                 #
        movl      %edi, 60(%esp)                                #
        lea       7(%ecx), %edi                                 #
        movl      %edi, 56(%esp)                                #
        movl      44(%esp), %edi                                #
                                # LOE eax edx ecx ebp edi xmm0 xmm1
..B1.36:                        # Preds ..B1.36 ..B1.35
        movl      52(%esp), %esi                                #19.56
        movl      %eax, 72(%esp)                                #
        movl      %esi, %ebx                                    #19.56
        imull     %edx, %ebx                                    #19.56
        addl      68(%esp), %ebx                                #19.56
        movsd     (%ebx), %xmm2                                 #19.56
        movl      %edx, 76(%esp)                                #
        movl      48(%esp), %edx                                #19.56
        movhpd    (%ebx,%edx,8), %xmm2                          #19.56
        mulpd     (%edi,%ecx,8), %xmm2                          #19.54
        addpd     %xmm2, %xmm0                                  #19.16
        movl      %esi, %ebx                                    #19.56
        imull     %eax, %ebx                                    #19.56
        movl      68(%esp), %eax                                #19.56
        addl      %eax, %ebx                                    #19.56
        movsd     (%ebx), %xmm3                                 #19.56
        movhpd    (%ebx,%edx,8), %xmm3                          #19.56
        movl      %esi, %ebx                                    #19.56
        imull     60(%esp), %ebx                                #19.56
        mulpd     16(%edi,%ecx,8), %xmm3                        #19.54
        addpd     %xmm3, %xmm1                                  #19.16
        addl      %eax, %ebx                                    #19.56
        movsd     (%ebx), %xmm4                                 #19.56
        movhpd    (%ebx,%edx,8), %xmm4                          #19.56
        mulpd     32(%edi,%ecx,8), %xmm4                        #19.54
        movl      56(%esp), %ebx                                #19.56
        movl      76(%esp), %edx                                #18.16
        addpd     %xmm4, %xmm0                                  #19.16
        imull     %ebx, %esi                                    #19.56
        addl      $8, %edx                                      #18.16
        addl      $8, %ebx                                      #18.16
        addl      $8, 60(%esp)                                  #18.16
        movl      %ebx, 56(%esp)                                #18.16
        addl      %eax, %esi                                    #19.56
        movsd     (%esi), %xmm5                                 #19.56
        movl      48(%esp), %eax                                #19.56
        movhpd    (%esi,%eax,8), %xmm5                          #19.56
        mulpd     48(%edi,%ecx,8), %xmm5                        #19.54
        addpd     %xmm5, %xmm1                                  #19.16
        movl      72(%esp), %eax                                #18.16
        movl      64(%esp), %ebx                                #18.16
        addl      $8, %eax                                      #18.16
        addl      $8, %ecx                                      #18.16
        cmpl      %ebx, %ecx                                    #18.16
        jb        ..B1.36       # Prob 99%                      #18.16
                                # LOE eax edx ecx ebp edi xmm0 xmm1
..B1.37:                        # Preds ..B1.36
        movl      16(%esp), %ebx                                #
        addpd     %xmm1, %xmm0                                  #19.16
        movaps    %xmm0, %xmm1                                  #19.16
        movl      %edi, 44(%esp)                                #
        unpckhpd  %xmm0, %xmm1                                  #19.16
        addsd     %xmm1, %xmm0                                  #19.16
        movsd     %xmm0, (%ebx)                                 #19.16
                                # LOE ebp
..B1.38:                        # Preds ..B1.37 ..B1.50
        movl      28(%esp), %edx                                #18.16
        movl      64(%esp), %eax                                #18.16
        cmpl      %edx, %eax                                    #18.16
        jae       ..B1.42       # Prob 1%                       #18.16
                                # LOE ebp
..B1.39:                        # Preds ..B1.38
        movl      40(%esp), %edx                                #
        movl      20(%esp), %ecx                                #
        movl      12(%esp), %eax                                #
        movl      64(%esp), %edi                                #19.29
        imull     52(%esp), %edx                                #
        lea       (%eax,%ecx,8), %eax                           #
        addl      8(%esp), %edx                                 #
        movl      52(%esp), %ebx                                #19.29
        movsd     (%edx,%ecx,8), %xmm0                          #19.29
        movl      44(%esp), %ecx                                #19.29
        movl      28(%esp), %esi                                #19.29
        movl      %edx, 48(%esp)                                #19.29
                                # LOE eax ecx ebx ebp esi edi xmm0
..B1.40:                        # Preds ..B1.40 ..B1.39
        movsd     (%ecx,%edi,8), %xmm1                          #19.42
        lea       1(%edi), %edx                                 #
        movl      %ebx, %edi                                    #19.16
        imull     %edx, %edi                                    #19.16
        mulsd     (%edi,%eax), %xmm1                            #19.54
        movl      %edx, %edi                                    #18.16
        cmpl      %esi, %edx                                    #18.16
        addsd     %xmm1, %xmm0                                  #19.16
        jb        ..B1.40       # Prob 99%                      #18.16
                                # LOE eax ecx ebx ebp esi edi xmm0
..B1.41:                        # Preds ..B1.40
        movl      48(%esp), %edx                                #
        movl      20(%esp), %eax                                #19.16
        movsd     %xmm0, (%edx,%eax,8)                          #19.16
        movl      %ecx, 44(%esp)                                #
        movl      %ebx, 52(%esp)                                #
                                # LOE ebp
..B1.42:                        # Preds ..B1.41 ..B1.38
        movl      20(%esp), %eax                                #16.13
        movl      4(%esp), %edx                                 #16.13
        addl      $1, %eax                                      #16.13
        movl      %eax, 20(%esp)                                #16.13
        cmpl      %edx, %eax                                    #16.13
        jb        ..B1.26       # Prob 99%                      #16.13
                                # LOE ebp
..B1.59:                        # Preds ..B1.42 ..B1.26
        movl      32(%esp), %esi                                #
        movl      36(%esp), %edi                                #
        movl      40(%esp), %eax                                #
                                # LOE eax ebp esi edi al ah
..B1.45:                        # Preds ..B1.59
        movl      (%esp), %edx                                  #15.10
        cmpl      %edx, %eax                                    #15.10
        jb        ..B1.24       # Prob 99%                      #15.10
                                # LOE eax ebp esi edi
..B1.47:                        # Preds ..B1.54 ..B1.45 ..B1.22 # Infreq
        addl      $80, %esp                                     #23.7
        popl      %ebx                                          #23.7
        popl      %esi                                          #23.7
        popl      %edi                                          #23.7
        ret                                                     #23.7
                                # LOE
..B1.48:                        # Preds ..B1.8                  # Infreq
        movl      24(%esp), %edi                                #
        lea       (%edi,%ecx), %edi                             #
        movl      %edi, 20(%esp)                                #
        jmp       ..B1.12       # Prob 100%                     #
                                # LOE eax edx ecx ebx ebp esi
..B1.49:                        # Preds ..B1.5 ..B1.7           # Infreq
        xorl      %esi, %esi                                    #16.13
        jmp       ..B1.15       # Prob 100%                     #16.13
                                # LOE eax ecx ebx ebp esi
..B1.50:                        # Preds ..B1.30 ..B1.28         # Infreq
        xorl      %eax, %eax                                    #18.16
        movl      %eax, 64(%esp)                                #18.16
        jmp       ..B1.38       # Prob 100%                     #18.16
                                # LOE ebp
..B1.51:                        # Preds ..B1.31                 # Infreq
        movl      40(%esp), %eax                                #
        movl      20(%esp), %edx                                #
        imull     52(%esp), %eax                                #
        movl      %edx, %esi                                    #
        shll      $3, %esi                                      #
        addl      8(%esp), %eax                                 #
        lea       (%eax,%edx,8), %ebx                           #
        movsd     (%ebx), %xmm0                                 #19.16
        jmp       ..B1.35       # Prob 100%                     #19.16
                                # LOE ecx ebx ebp esi xmm0
..B1.54:                        # Preds ..B1.24                 # Infreq
        movl      (%esp), %edx                                  #15.10
        addl      $1, %eax                                      #15.10
        cmpl      %edx, %eax                                    #15.10
        jb        ..B1.24       # Prob 99%                      #15.10
        jmp       ..B1.47       # Prob 100%                     #15.10
        .align    2,0x90
                                # LOE eax ebp esi edi
# mark_end;
	.type	muld_,@function
	.size	muld_,.-muld_
	.data
# -- End  muld_
	.data
	.section .note.GNU-stack, ""
# End
