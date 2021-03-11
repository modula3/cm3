# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "mulmv_sp.f"
	.text
..TXTST0:
# -- Begin  mulmv_sp_
# mark_begin;
       .align    2,0x90
	.globl mulmv_sp_
mulmv_sp_:
# parameter 1: 52 + %esp
# parameter 2: 56 + %esp
# parameter 3: 60 + %esp
# parameter 4: 64 + %esp
# parameter 5: 68 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %esi                                          #4.18
        pushl     %ebx                                          #4.18
        subl      $36, %esp                                     #4.18
        movl      52(%esp), %edi                                #4.18
        movl      56(%esp), %esi                                #4.18
        movl      64(%esp), %eax                                #4.18
        movl      (%eax), %ecx                                  #10.10
        movl      68(%esp), %edx                                #4.18
        movl      (%edx), %edx                                  #4.18
        movl      %ecx, (%esp)                                  #10.10
        testl     %ecx, %ecx                                    #10.10
        jle       ..B1.27       # Prob 1%                       #10.10
                                # LOE edx ebp esi edi
..B1.2:                         # Preds ..B1.1
        xorl      %eax, %eax                                    #
        movl      %esi, %ecx                                    #12.13
        andl      $15, %ecx                                     #12.13
        movl      %ecx, 16(%esp)                                #12.13
        movl      %edx, %ebx                                    #13.27
        shll      $2, %ebx                                      #13.27
        movl      %ebx, 12(%esp)                                #12.13
        subl      %ebx, %edi                                    #
        movl      %edi, 8(%esp)                                 #
        movl      %ecx, %edi                                    #12.13
        andl      $3, %edi                                      #12.13
        movl      %edi, 4(%esp)                                 #12.13
                                # LOE eax edx ebp esi
..B1.3:                         # Preds ..B1.25 ..B1.24 ..B1.2
        movl      %eax, %ecx                                    #
        shll      $2, %ecx                                      #
        movl      %ecx, 32(%esp)                                #
        testl     %edx, %edx                                    #12.13
        pxor      %xmm1, %xmm1                                  #11.10
        pxor      %xmm0, %xmm0                                  #11.10
        jle       ..B1.25       # Prob 50%                      #12.13
                                # LOE eax edx ecx ebp cl ch xmm0 xmm1
..B1.4:                         # Preds ..B1.3
        movl      16(%esp), %ebx                                #12.13
        testl     %ebx, %ebx                                    #12.13
        je        ..B1.7        # Prob 50%                      #12.13
                                # LOE eax edx ebx ebp bl bh xmm0 xmm1
..B1.5:                         # Preds ..B1.4
        movl      4(%esp), %ecx                                 #12.13
        testl     %ecx, %ecx                                    #12.13
        jne       ..B1.30       # Prob 10%                      #12.13
                                # LOE eax edx ebx ebp bl bh xmm0 xmm1
..B1.6:                         # Preds ..B1.5
        negl      %ebx                                          #12.13
        addl      $16, %ebx                                     #12.13
        shrl      $2, %ebx                                      #12.13
                                # LOE eax edx ebx ebp xmm0 xmm1
..B1.7:                         # Preds ..B1.6 ..B1.4
        lea       8(%ebx), %ecx                                 #12.13
        cmpl      %ecx, %edx                                    #12.13
        jl        ..B1.30       # Prob 10%                      #12.13
                                # LOE eax edx ebx ebp xmm0 xmm1
..B1.8:                         # Preds ..B1.7
        movl      %edx, %esi                                    #12.13
        subl      %ebx, %esi                                    #12.13
        andl      $7, %esi                                      #12.13
        negl      %esi                                          #12.13
        addl      %edx, %esi                                    #12.13
        testl     %ebx, %ebx                                    #12.13
        jbe       ..B1.29       # Prob 1%                       #12.13
                                # LOE eax edx ebx ebp esi xmm1
..B1.9:                         # Preds ..B1.8
        movl      12(%esp), %edi                                #
        movl      %edx, 20(%esp)                                #
        movl      56(%esp), %edx                                #
        xorl      %ecx, %ecx                                    #
        pxor      %xmm1, %xmm1                                  #12.13
        movl      %ecx, 28(%esp)                                #
        movl      8(%esp), %ecx                                 #
        addl      $1, %eax                                      #
        imull     %eax, %edi                                    #
        lea       (%ecx,%edi), %ecx                             #
        movl      %edi, 24(%esp)                                #
        movl      28(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi xmm1
..B1.10:                        # Preds ..B1.10 ..B1.9
        movss     (%ecx,%edi,4), %xmm0                          #13.27
        mulss     (%edx,%edi,4), %xmm0                          #13.33
        addl      $1, %edi                                      #12.13
        cmpl      %ebx, %edi                                    #12.13
        addss     %xmm0, %xmm1                                  #13.13
        jb        ..B1.10       # Prob 99%                      #12.13
                                # LOE eax edx ecx ebx ebp esi edi xmm1
..B1.11:                        # Preds ..B1.10
        movl      20(%esp), %edx                                #
                                # LOE eax edx ecx ebx ebp esi xmm1
..B1.12:                        # Preds ..B1.11 ..B1.29
        movl      24(%esp), %edi                                #12.13
        addl      8(%esp), %edi                                 #12.13
        lea       (%edi,%ebx,4), %edi                           #12.13
        testl     $15, %edi                                     #12.13
        je        ..B1.16       # Prob 60%                      #12.13
                                # LOE eax edx ecx ebx ebp esi xmm1
..B1.13:                        # Preds ..B1.12
        movl      56(%esp), %edi                                #13.13
        pxor      %xmm0, %xmm0                                  #13.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.14:                        # Preds ..B1.14 ..B1.13
        movups    (%ecx,%ebx,4), %xmm2                          #13.27
        movups    16(%ecx,%ebx,4), %xmm3                        #13.27
        mulps     (%edi,%ebx,4), %xmm2                          #13.33
        mulps     16(%edi,%ebx,4), %xmm3                        #13.33
        addps     %xmm2, %xmm1                                  #13.13
        addps     %xmm3, %xmm0                                  #13.13
        addl      $8, %ebx                                      #
        cmpl      %esi, %ebx                                    #12.13
        jb        ..B1.14       # Prob 99%                      #12.13
        jmp       ..B1.19       # Prob 100%                     #12.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.16:                        # Preds ..B1.12
        movl      56(%esp), %edi                                #
        pxor      %xmm0, %xmm0                                  #
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.17:                        # Preds ..B1.17 ..B1.16
        movaps    (%ecx,%ebx,4), %xmm2                          #13.27
        mulps     (%edi,%ebx,4), %xmm2                          #13.33
        movaps    16(%ecx,%ebx,4), %xmm3                        #13.27
        mulps     16(%edi,%ebx,4), %xmm3                        #13.33
        addps     %xmm2, %xmm1                                  #13.13
        addps     %xmm3, %xmm0                                  #13.13
        addl      $8, %ebx                                      #
        cmpl      %esi, %ebx                                    #12.13
        jb        ..B1.17       # Prob 99%                      #12.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.19:                        # Preds ..B1.17 ..B1.14
        addps     %xmm0, %xmm1                                  #13.13
        movaps    %xmm1, %xmm0                                  #13.13
        movhlps   %xmm1, %xmm0                                  #13.13
        addps     %xmm0, %xmm1                                  #13.13
        movaps    %xmm1, %xmm2                                  #13.13
        shufps    $245, %xmm1, %xmm2                            #13.13
        addss     %xmm2, %xmm1                                  #13.13
        movaps    %xmm1, %xmm0                                  #13.13
                                # LOE eax edx ebp esi xmm0 xmm1
..B1.20:                        # Preds ..B1.19 ..B1.30
        cmpl      %edx, %esi                                    #12.13
        jae       ..B1.32       # Prob 1%                       #12.13
                                # LOE eax edx ebp esi xmm0 xmm1
..B1.21:                        # Preds ..B1.20
        movl      12(%esp), %ecx                                #
        movl      56(%esp), %ebx                                #
        imull     %eax, %ecx                                    #
        addl      8(%esp), %ecx                                 #
                                # LOE eax edx ecx ebx ebp esi xmm1
..B1.22:                        # Preds ..B1.22 ..B1.21
        movss     (%ecx,%esi,4), %xmm0                          #13.27
        mulss     (%ebx,%esi,4), %xmm0                          #13.33
        addl      $1, %esi                                      #12.13
        cmpl      %edx, %esi                                    #12.13
        addss     %xmm0, %xmm1                                  #13.13
        jb        ..B1.22       # Prob 99%                      #12.13
                                # LOE eax edx ecx ebx ebp esi xmm1
..B1.23:                        # Preds ..B1.22
        movl      60(%esp), %ecx                                #13.13
        movl      32(%esp), %ebx                                #13.13
        movss     %xmm1, (%ecx,%ebx)                            #13.13
                                # LOE eax edx ebp
..B1.24:                        # Preds ..B1.23 ..B1.32
        movl      (%esp), %ecx                                  #10.10
        cmpl      %ecx, %eax                                    #10.10
        jb        ..B1.3        # Prob 99%                      #10.10
        jmp       ..B1.27       # Prob 100%                     #10.10
                                # LOE eax edx ebp
..B1.25:                        # Preds ..B1.3
        movl      60(%esp), %ebx                                #13.13
        movl      %ecx, %esi                                    #13.13
        movl      (%esp), %edi                                  #10.10
        xorl      %ecx, %ecx                                    #13.13
        addl      $1, %eax                                      #10.10
        cmpl      %edi, %eax                                    #10.10
        movl      %ecx, (%ebx,%esi)                             #13.13
        jb        ..B1.3        # Prob 99%                      #10.10
                                # LOE eax edx ebp
..B1.27:                        # Preds ..B1.24 ..B1.25 ..B1.1  # Infreq
        addl      $36, %esp                                     #16.7
        popl      %ebx                                          #16.7
        popl      %esi                                          #16.7
        popl      %edi                                          #16.7
        ret                                                     #16.7
                                # LOE
..B1.29:                        # Preds ..B1.8                  # Infreq
        movl      12(%esp), %ecx                                #
        movl      8(%esp), %edi                                 #
        addl      $1, %eax                                      #
        imull     %eax, %ecx                                    #
        movl      %ecx, 24(%esp)                                #
        lea       (%edi,%ecx), %ecx                             #
        jmp       ..B1.12       # Prob 100%                     #
                                # LOE eax edx ecx ebx ebp esi xmm1
..B1.30:                        # Preds ..B1.5 ..B1.7           # Infreq
        xorl      %esi, %esi                                    #12.13
        addl      $1, %eax                                      #
        jmp       ..B1.20       # Prob 100%                     #
                                # LOE eax edx ebp esi xmm0 xmm1
..B1.32:                        # Preds ..B1.20                 # Infreq
        movl      60(%esp), %ecx                                #13.13
        movl      32(%esp), %ebx                                #13.13
        movss     %xmm0, (%ecx,%ebx)                            #13.13
        jmp       ..B1.24       # Prob 100%                     #13.13
        .align    2,0x90
                                # LOE eax edx ebp
# mark_end;
	.type	mulmv_sp_,@function
	.size	mulmv_sp_,.-mulmv_sp_
	.data
# -- End  mulmv_sp_
	.data
	.section .note.GNU-stack, ""
# End
