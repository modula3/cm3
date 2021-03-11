# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "lu2_backsubstitute_sp.f"
	.text
..TXTST0:
# -- Begin  lu2_backsubstitute_sp_
# mark_begin;
       .align    2,0x90
	.globl lu2_backsubstitute_sp_
lu2_backsubstitute_sp_:
# parameter 1: 56 + %esp
# parameter 2: 60 + %esp
# parameter 3: 64 + %esp
# parameter 4: 68 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %esi                                          #4.18
        pushl     %ebx                                          #4.18
        subl      $40, %esp                                     #4.18
        movl      60(%esp), %eax                                #4.18
        movl      68(%esp), %edx                                #4.18
        movl      (%edx), %ecx                                  #4.18
        testl     %ecx, %ecx                                    #18.10
        jle       ..B1.53       # Prob 1%                       #18.10
                                # LOE eax ecx ebp
..B1.2:                         # Preds ..B1.1
        movl      56(%esp), %edi                                #
        movl      %ecx, 36(%esp)                                #
        movl      $-1, %esi                                     #
        xorl      %edx, %edx                                    #
        movl      %ecx, %ebx                                    #30.28
        movl      %esi, 32(%esp)                                #
        shll      $2, %ebx                                      #30.28
        movl      %ebx, 20(%esp)                                #
        subl      %ebx, %edi                                    #
        movl      %edi, 4(%esp)                                 #
                                # LOE eax edx ebp
..B1.3:                         # Preds ..B1.26 ..B1.2
        movl      60(%esp), %eax                                #25.15
        movl      64(%esp), %esi                                #26.10
        movl      %edx, %ecx                                    #
        shll      $2, %ecx                                      #
        movl      (%eax,%ecx), %edi                             #25.15
        movss     (%esi,%edi,4), %xmm0                          #26.10
        movl      32(%esp), %eax                                #28.17
        movl      (%esi,%ecx), %ebx                             #27.10
        movl      %ebx, (%esi,%edi,4)                           #27.10
        cmpl      $-1, %eax                                     #28.17
        je        ..B1.60       # Prob 16%                      #28.17
                                # LOE eax edx ecx ebp al ah xmm0
..B1.4:                         # Preds ..B1.3
        cmpl      %eax, %edx                                    #29.16
        jl        ..B1.25       # Prob 50%                      #29.16
                                # LOE eax edx ecx ebp al ah xmm0
..B1.5:                         # Preds ..B1.4
        movl      %eax, %esi                                    #29.16
        movl      64(%esp), %ebx                                #29.16
        movl      %edx, %eax                                    #29.16
        subl      %esi, %eax                                    #29.16
        addl      $1, %eax                                      #29.16
        lea       12(%ebx,%esi,4), %ebx                         #29.16
        andl      $15, %ebx                                     #29.16
        je        ..B1.8        # Prob 50%                      #29.16
                                # LOE eax edx ecx ebx ebp xmm0
..B1.6:                         # Preds ..B1.5
        testb     $3, %bl                                       #29.16
        jne       ..B1.54       # Prob 10%                      #29.16
                                # LOE eax edx ecx ebx ebp xmm0
..B1.7:                         # Preds ..B1.6
        negl      %ebx                                          #29.16
        addl      $16, %ebx                                     #29.16
        shrl      $2, %ebx                                      #29.16
                                # LOE eax edx ecx ebx ebp xmm0
..B1.8:                         # Preds ..B1.7 ..B1.5
        lea       8(%ebx), %esi                                 #29.16
        cmpl      %esi, %eax                                    #29.16
        jl        ..B1.54       # Prob 10%                      #29.16
                                # LOE eax edx ecx ebx ebp xmm0
..B1.9:                         # Preds ..B1.8
        movl      %eax, %esi                                    #29.16
        subl      %ebx, %esi                                    #29.16
        andl      $7, %esi                                      #29.16
        negl      %esi                                          #29.16
        addl      %eax, %esi                                    #29.16
        testl     %ebx, %ebx                                    #29.16
        movl      %esi, 12(%esp)                                #29.16
        jbe       ..B1.55       # Prob 1%                       #29.16
                                # LOE eax edx ecx ebx ebp xmm0
..B1.10:                        # Preds ..B1.9
        movl      20(%esp), %edi                                #
        xorl      %esi, %esi                                    #
        movl      %esi, 28(%esp)                                #
        addl      $1, %edx                                      #
        imull     %edx, %edi                                    #
        movl      %edx, (%esp)                                  #
        movl      32(%esp), %edx                                #
        movl      %edi, 8(%esp)                                 #
        lea       (%edi,%edx,4), %esi                           #
        addl      4(%esp), %esi                                 #
        movl      28(%esp), %edi                                #
        movl      %esi, 16(%esp)                                #
        movl      64(%esp), %esi                                #
        lea       (%esi,%edx,4), %edx                           #
        movl      %edx, 24(%esp)                                #
        movl      (%esp), %edx                                  #
        movl      24(%esp), %esi                                #
        movl      %edx, (%esp)                                  #
        movl      16(%esp), %edx                                #
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.11:                        # Preds ..B1.11 ..B1.10
        movss     -4(%edx,%edi,4), %xmm1                        #30.28
        mulss     -4(%esi,%edi,4), %xmm1                        #30.38
        addl      $1, %edi                                      #29.16
        cmpl      %ebx, %edi                                    #29.16
        subss     %xmm1, %xmm0                                  #30.16
        jb        ..B1.11       # Prob 99%                      #29.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.12:                        # Preds ..B1.11
        movl      (%esp), %edx                                  #
                                # LOE eax edx ecx ebx ebp dl dh xmm0
..B1.13:                        # Preds ..B1.12 ..B1.55
        movl      32(%esp), %edi                                #29.16
        movl      8(%esp), %esi                                 #29.16
        lea       (%esi,%edi,4), %esi                           #29.16
        addl      4(%esp), %esi                                 #29.16
        lea       12(%esi,%ebx,4), %esi                         #29.16
        testl     $15, %esi                                     #29.16
        je        ..B1.17       # Prob 60%                      #29.16
                                # LOE eax edx ecx ebx ebp dl dh xmm0
..B1.14:                        # Preds ..B1.13
        movl      12(%esp), %edi                                #29.16
        movl      24(%esp), %esi                                #29.16
        movl      %edx, (%esp)                                  #29.16
        movl      16(%esp), %edx                                #29.16
        pxor      %xmm1, %xmm1                                  #29.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.15:                        # Preds ..B1.15 ..B1.14
        movups    -4(%edx,%ebx,4), %xmm2                        #30.28
        movups    12(%edx,%ebx,4), %xmm3                        #30.28
        mulps     -4(%esi,%ebx,4), %xmm2                        #30.38
        mulps     12(%esi,%ebx,4), %xmm3                        #30.38
        addl      $8, %ebx                                      #
        subps     %xmm2, %xmm0                                  #30.16
        subps     %xmm3, %xmm1                                  #30.16
        cmpl      %edi, %ebx                                    #29.16
        jb        ..B1.15       # Prob 99%                      #29.16
        jmp       ..B1.19       # Prob 100%                     #29.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.17:                        # Preds ..B1.13
        movl      12(%esp), %edi                                #
        movl      24(%esp), %esi                                #
        movl      %edx, (%esp)                                  #
        movl      16(%esp), %edx                                #
        pxor      %xmm1, %xmm1                                  #
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.18:                        # Preds ..B1.18 ..B1.17
        movaps    -4(%edx,%ebx,4), %xmm2                        #30.28
        mulps     -4(%esi,%ebx,4), %xmm2                        #30.38
        movaps    12(%edx,%ebx,4), %xmm3                        #30.28
        mulps     12(%esi,%ebx,4), %xmm3                        #30.38
        subps     %xmm2, %xmm0                                  #30.16
        addl      $8, %ebx                                      #
        subps     %xmm3, %xmm1                                  #30.16
        cmpl      %edi, %ebx                                    #29.16
        jb        ..B1.18       # Prob 99%                      #29.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.19:                        # Preds ..B1.15 ..B1.18
        movl      (%esp), %edx                                  #
        movl      %edi, 12(%esp)                                #
                                # LOE eax edx ecx ebp dl dh xmm0 xmm1
..B1.20:                        # Preds ..B1.19
        addps     %xmm1, %xmm0                                  #29.16
        movaps    %xmm0, %xmm1                                  #29.16
        movhlps   %xmm0, %xmm1                                  #29.16
        addps     %xmm1, %xmm0                                  #29.16
        movaps    %xmm0, %xmm2                                  #29.16
        shufps    $245, %xmm0, %xmm2                            #29.16
        addss     %xmm2, %xmm0                                  #29.16
                                # LOE eax edx ecx ebp xmm0
..B1.21:                        # Preds ..B1.20 ..B1.54
        movl      12(%esp), %ebx                                #29.16
        cmpl      %eax, %ebx                                    #29.16
        jae       ..B1.26       # Prob 1%                       #29.16
                                # LOE eax edx ecx ebp xmm0
..B1.22:                        # Preds ..B1.21
        movl      20(%esp), %esi                                #
        movl      32(%esp), %ebx                                #
        imull     %edx, %esi                                    #
        lea       (%esi,%ebx,4), %edi                           #
        addl      4(%esp), %edi                                 #
        movl      64(%esp), %esi                                #
        lea       (%esi,%ebx,4), %ebx                           #
        movl      %edi, 28(%esp)                                #
        movl      %edi, %esi                                    #
        movl      12(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.23:                        # Preds ..B1.23 ..B1.22
        movss     -4(%esi,%edi,4), %xmm1                        #30.28
        mulss     -4(%ebx,%edi,4), %xmm1                        #30.38
        addl      $1, %edi                                      #29.16
        cmpl      %eax, %edi                                    #29.16
        subss     %xmm1, %xmm0                                  #30.16
        jb        ..B1.23       # Prob 99%                      #29.16
        jmp       ..B1.26       # Prob 100%                     #29.16
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.25:                        # Preds ..B1.4
        addl      $1, %edx                                      #
                                # LOE edx ecx ebp xmm0
..B1.26:                        # Preds ..B1.23 ..B1.60 ..B1.21 ..B1.25 ..B1.61
                                #      
        movl      64(%esp), %eax                                #35.10
        movss     %xmm0, (%eax,%ecx)                            #35.10
        movl      36(%esp), %ecx                                #18.10
        cmpl      %ecx, %edx                                    #18.10
        jb        ..B1.3        # Prob 99%                      #18.10
                                # LOE edx ecx ebp cl ch
..B1.27:                        # Preds ..B1.26
        testl     %ecx, %ecx                                    #38.10
        jle       ..B1.53       # Prob 1%                       #38.10
                                # LOE ecx ebp cl ch
..B1.28:                        # Preds ..B1.27
        movl      64(%esp), %ebx                                #
        movl      %ecx, 36(%esp)                                #40.13
        xorl      %edx, %edx                                    #
        lea       (%ebx,%ecx,4), %eax                           #
        movl      %ecx, %esi                                    #30.28
        movl      %eax, 24(%esp)                                #
        lea       (%ebx,%ecx,4), %eax                           #
        movl      56(%esp), %ebx                                #
        shll      $2, %esi                                      #30.28
        movl      %esi, 8(%esp)                                 #40.13
        subl      %esi, %ebx                                    #
        movl      %ebx, 4(%esp)                                 #40.13
        lea       (%ebx,%ecx,4), %edi                           #
        movl      %edi, 28(%esp)                                #
        movl      %edx, 20(%esp)                                #40.13
                                # LOE eax edx ebp
..B1.29:                        # Preds ..B1.51 ..B1.28
        movl      36(%esp), %ecx                                #40.13
        movss     -4(%eax), %xmm0                               #39.10
        movl      %ecx, %ebx                                    #40.13
        subl      %edx, %ebx                                    #40.13
        cmpl      %ebx, %ecx                                    #40.13
        jle       ..B1.50       # Prob 50%                      #40.13
                                # LOE eax edx ebx ebp xmm0
..B1.30:                        # Preds ..B1.29
        movl      24(%esp), %ecx                                #40.13
        andl      $15, %ecx                                     #40.13
        je        ..B1.33       # Prob 50%                      #40.13
                                # LOE eax edx ecx ebx ebp xmm0
..B1.31:                        # Preds ..B1.30
        testb     $3, %cl                                       #40.13
        jne       ..B1.57       # Prob 10%                      #40.13
                                # LOE eax edx ecx ebx ebp xmm0
..B1.32:                        # Preds ..B1.31
        negl      %ecx                                          #40.13
        addl      $16, %ecx                                     #40.13
        shrl      $2, %ecx                                      #40.13
                                # LOE eax edx ecx ebx ebp xmm0
..B1.33:                        # Preds ..B1.32 ..B1.30
        lea       8(%ecx), %esi                                 #40.13
        cmpl      %esi, %edx                                    #40.13
        jl        ..B1.57       # Prob 10%                      #40.13
                                # LOE eax edx ecx ebx ebp xmm0
..B1.34:                        # Preds ..B1.33
        movl      %edx, %esi                                    #40.13
        subl      %ecx, %esi                                    #40.13
        andl      $7, %esi                                      #40.13
        negl      %esi                                          #40.13
        addl      %edx, %esi                                    #40.13
        testl     %ecx, %ecx                                    #40.13
        movl      %esi, 16(%esp)                                #40.13
        jbe       ..B1.58       # Prob 1%                       #40.13
                                # LOE eax edx ecx ebx ebp xmm0
..B1.35:                        # Preds ..B1.34
        imull     8(%esp), %ebx                                 #
        movl      20(%esp), %edi                                #
        xorl      %esi, %esi                                    #
        movl      %esi, (%esp)                                  #
        lea       (%edi,%ebx), %edi                             #
        movl      28(%esp), %esi                                #
        movl      %edi, 32(%esp)                                #
        lea       (%esi,%edi), %esi                             #
        movl      (%esp), %edi                                  #
        movl      %esi, 12(%esp)                                #
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.36:                        # Preds ..B1.36 ..B1.35
        movss     (%esi,%edi,4), %xmm1                          #41.25
        mulss     (%eax,%edi,4), %xmm1                          #41.35
        addl      $1, %edi                                      #40.13
        cmpl      %ecx, %edi                                    #40.13
        subss     %xmm1, %xmm0                                  #41.13
        jb        ..B1.36       # Prob 99%                      #40.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.38:                        # Preds ..B1.36 ..B1.58
        movl      4(%esp), %esi                                 #40.13
        movl      36(%esp), %edi                                #40.13
        lea       (%esi,%edi,4), %esi                           #40.13
        addl      32(%esp), %esi                                #40.13
        lea       (%esi,%ecx,4), %esi                           #40.13
        testl     $15, %esi                                     #40.13
        je        ..B1.42       # Prob 60%                      #40.13
                                # LOE eax edx ecx ebx ebp xmm0
..B1.39:                        # Preds ..B1.38
        movl      16(%esp), %edi                                #40.13
        movl      12(%esp), %esi                                #40.13
        pxor      %xmm1, %xmm1                                  #40.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.40:                        # Preds ..B1.40 ..B1.39
        movups    (%esi,%ecx,4), %xmm2                          #41.25
        movups    16(%esi,%ecx,4), %xmm3                        #41.25
        mulps     (%eax,%ecx,4), %xmm2                          #41.35
        mulps     16(%eax,%ecx,4), %xmm3                        #41.35
        addl      $8, %ecx                                      #
        subps     %xmm2, %xmm0                                  #41.13
        subps     %xmm3, %xmm1                                  #41.13
        cmpl      %edi, %ecx                                    #40.13
        jb        ..B1.40       # Prob 99%                      #40.13
        jmp       ..B1.44       # Prob 100%                     #40.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.42:                        # Preds ..B1.38
        movl      16(%esp), %edi                                #
        movl      12(%esp), %esi                                #
        pxor      %xmm1, %xmm1                                  #
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.43:                        # Preds ..B1.43 ..B1.42
        movaps    (%esi,%ecx,4), %xmm2                          #41.25
        mulps     (%eax,%ecx,4), %xmm2                          #41.35
        movaps    16(%esi,%ecx,4), %xmm3                        #41.25
        mulps     16(%eax,%ecx,4), %xmm3                        #41.35
        subps     %xmm2, %xmm0                                  #41.13
        addl      $8, %ecx                                      #
        subps     %xmm3, %xmm1                                  #41.13
        cmpl      %edi, %ecx                                    #40.13
        jb        ..B1.43       # Prob 99%                      #40.13
                                # LOE eax edx ecx ebx ebp esi edi xmm0 xmm1
..B1.44:                        # Preds ..B1.40 ..B1.43
        movl      %edi, 16(%esp)                                #
                                # LOE eax edx ebx ebp xmm0 xmm1
..B1.45:                        # Preds ..B1.44
        addps     %xmm1, %xmm0                                  #40.13
        movaps    %xmm0, %xmm1                                  #40.13
        movhlps   %xmm0, %xmm1                                  #40.13
        addps     %xmm1, %xmm0                                  #40.13
        movaps    %xmm0, %xmm2                                  #40.13
        shufps    $245, %xmm0, %xmm2                            #40.13
        addss     %xmm2, %xmm0                                  #40.13
                                # LOE eax edx ebx ebp xmm0
..B1.46:                        # Preds ..B1.45 ..B1.57
        movl      16(%esp), %ecx                                #40.13
        cmpl      %edx, %ecx                                    #40.13
        jae       ..B1.51       # Prob 1%                       #40.13
                                # LOE eax edx ebx ebp xmm0
..B1.47:                        # Preds ..B1.46
        movl      20(%esp), %ecx                                #
        movl      16(%esp), %esi                                #
        lea       (%ecx,%ebx), %ecx                             #
        addl      28(%esp), %ecx                                #
                                # LOE eax edx ecx ebx ebp esi xmm0
..B1.48:                        # Preds ..B1.48 ..B1.47
        movss     (%ecx,%esi,4), %xmm1                          #41.25
        mulss     (%eax,%esi,4), %xmm1                          #41.35
        addl      $1, %esi                                      #40.13
        cmpl      %edx, %esi                                    #40.13
        subss     %xmm1, %xmm0                                  #41.13
        jb        ..B1.48       # Prob 99%                      #40.13
        jmp       ..B1.51       # Prob 100%                     #40.13
                                # LOE eax edx ecx ebx ebp esi xmm0
..B1.50:                        # Preds ..B1.29
        imull     8(%esp), %ebx                                 #
                                # LOE eax edx ebx ebp xmm0
..B1.51:                        # Preds ..B1.48 ..B1.46 ..B1.50
        addl      28(%esp), %ebx                                #43.10
        movl      20(%esp), %ecx                                #43.10
        divss     -4(%ecx,%ebx), %xmm0                          #43.10
        movl      36(%esp), %ebx                                #38.10
        movss     %xmm0, -4(%eax)                               #43.10
        addl      $-4, %ecx                                     #38.10
        addl      $-4, %eax                                     #38.10
        addl      $1, %edx                                      #38.10
        addl      $-4, 24(%esp)                                 #38.10
        movl      %ecx, 20(%esp)                                #38.10
        cmpl      %ebx, %edx                                    #38.10
        jb        ..B1.29       # Prob 99%                      #38.10
                                # LOE eax edx ebp
..B1.53:                        # Preds ..B1.51 ..B1.1 ..B1.27
        addl      $40, %esp                                     #45.7
        popl      %ebx                                          #45.7
        popl      %esi                                          #45.7
        popl      %edi                                          #45.7
        ret                                                     #45.7
                                # LOE
..B1.54:                        # Preds ..B1.8 ..B1.6           # Infreq
        xorl      %ebx, %ebx                                    #29.16
        movl      %ebx, 12(%esp)                                #29.16
        addl      $1, %edx                                      #
        jmp       ..B1.21       # Prob 100%                     #
                                # LOE eax edx ecx ebp xmm0
..B1.55:                        # Preds ..B1.9                  # Infreq
        movl      20(%esp), %esi                                #
        addl      $1, %edx                                      #
        movl      %edx, (%esp)                                  #
        imull     %edx, %esi                                    #
        movl      32(%esp), %edx                                #
        movl      %esi, 8(%esp)                                 #
        lea       (%esi,%edx,4), %edi                           #
        movl      64(%esp), %esi                                #
        lea       (%esi,%edx,4), %edx                           #
        addl      4(%esp), %edi                                 #
        movl      %edi, 16(%esp)                                #
        movl      %edx, 24(%esp)                                #
        movl      (%esp), %edx                                  #
        jmp       ..B1.13       # Prob 100%                     #
                                # LOE eax edx ecx ebx ebp dl dh xmm0
..B1.57:                        # Preds ..B1.33 ..B1.31         # Infreq
        imull     8(%esp), %ebx                                 #
        xorl      %ecx, %ecx                                    #40.13
        movl      %ecx, 16(%esp)                                #40.13
        jmp       ..B1.46       # Prob 100%                     #40.13
                                # LOE eax edx ebx ebp xmm0
..B1.58:                        # Preds ..B1.34                 # Infreq
        imull     8(%esp), %ebx                                 #
        movl      20(%esp), %esi                                #
        movl      28(%esp), %edi                                #
        lea       (%esi,%ebx), %esi                             #
        movl      %esi, 32(%esp)                                #
        lea       (%edi,%esi), %esi                             #
        movl      %esi, 12(%esp)                                #
        jmp       ..B1.38       # Prob 100%                     #
                                # LOE eax edx ecx ebx ebp xmm0
..B1.60:                        # Preds ..B1.3                  # Infreq
        addl      $1, %edx                                      #
        pxor      %xmm1, %xmm1                                  #32.23
        ucomiss   %xmm1, %xmm0                                  #32.23
        jp        ..B1.61       # Prob 0%                       #32.23
        je        ..B1.26       # Prob 16%                      #32.23
                                # LOE edx ecx ebp xmm0
..B1.61:                        # Preds ..B1.60                 # Infreq
        movl      %edx, 32(%esp)                                #33.13
        jmp       ..B1.26       # Prob 100%                     #33.13
        .align    2,0x90
                                # LOE edx ecx ebp xmm0
# mark_end;
	.type	lu2_backsubstitute_sp_,@function
	.size	lu2_backsubstitute_sp_,.-lu2_backsubstitute_sp_
	.data
# -- End  lu2_backsubstitute_sp_
	.data
	.section .note.GNU-stack, ""
# End
