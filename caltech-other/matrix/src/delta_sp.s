# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "delta_sp.f"
	.text
..TXTST0:
# -- Begin  delta_sp_
# mark_begin;
       .align    2,0x90
	.globl delta_sp_
delta_sp_:
# parameter 1: 16 + %esp
# parameter 2: 20 + %esp
# parameter 3: 24 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %esi                                          #4.18
        subl      $8, %esp                                      #4.18
        movl      24(%esp), %eax                                #4.18
        movl      (%eax), %esi                                  #9.10
        testl     %esi, %esi                                    #9.10
        jle       ..B1.21       # Prob 50%                      #9.10
                                # LOE ebx ebp esi edi
..B1.2:                         # Preds ..B1.1
        movl      16(%esp), %edx                                #9.10
        andl      $63, %edx                                     #9.10
        je        ..B1.5        # Prob 50%                      #9.10
                                # LOE edx ebx ebp esi edi
..B1.3:                         # Preds ..B1.2
        testb     $3, %dl                                       #9.10
        jne       ..B1.22       # Prob 10%                      #9.10
                                # LOE edx ebx ebp esi edi
..B1.4:                         # Preds ..B1.3
        negl      %edx                                          #9.10
        addl      $64, %edx                                     #9.10
        shrl      $2, %edx                                      #9.10
                                # LOE edx ebx ebp esi edi
..B1.5:                         # Preds ..B1.4 ..B1.2
        lea       16(%edx), %eax                                #9.10
        cmpl      %eax, %esi                                    #9.10
        jl        ..B1.22       # Prob 10%                      #9.10
                                # LOE edx ebx ebp esi edi
..B1.6:                         # Preds ..B1.5
        movl      %esi, %ecx                                    #9.10
        subl      %edx, %ecx                                    #9.10
        andl      $15, %ecx                                     #9.10
        negl      %ecx                                          #9.10
        addl      %esi, %ecx                                    #9.10
        testl     %edx, %edx                                    #9.10
        jbe       ..B1.10       # Prob 1%                       #9.10
                                # LOE edx ecx ebx ebp esi edi
..B1.7:                         # Preds ..B1.6
        movl      16(%esp), %eax                                #10.17
        movss     (%eax), %xmm0                                 #10.17
        movl      %edi, 4(%esp)                                 #
        movl      16(%esp), %edi                                #
        movl      %ebx, (%esp)                                  #
        movl      20(%esp), %ebx                                #
        xorl      %eax, %eax                                    #
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.8:                         # Preds ..B1.8 ..B1.7
        movss     4(%edi,%eax,4), %xmm2                         #10.17
        movaps    %xmm2, %xmm1                                  #10.10
        subss     %xmm0, %xmm1                                  #10.10
        movss     %xmm1, (%ebx,%eax,4)                          #10.10
        movaps    %xmm2, %xmm0                                  #9.10
        addl      $1, %eax                                      #9.10
        cmpl      %edx, %eax                                    #9.10
        jb        ..B1.8        # Prob 99%                      #9.10
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.9:                         # Preds ..B1.8
        movl      4(%esp), %edi                                 #
        movl      (%esp), %ebx                                  #
                                # LOE edx ecx ebx ebp esi edi
..B1.10:                        # Preds ..B1.9 ..B1.6
        movl      20(%esp), %eax                                #9.10
        lea       (%eax,%edx,4), %eax                           #9.10
        testb     $15, %al                                      #9.10
        je        ..B1.14       # Prob 60%                      #9.10
                                # LOE edx ecx ebx ebp esi edi
..B1.11:                        # Preds ..B1.10
        movl      20(%esp), %eax                                #
        movl      %edi, 4(%esp)                                 #
        movl      16(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi
..B1.12:                        # Preds ..B1.12 ..B1.11
        movups    4(%edi,%edx,4), %xmm0                         #10.17
        movups    20(%edi,%edx,4), %xmm1                        #10.17
        movups    36(%edi,%edx,4), %xmm2                        #10.17
        movsd     52(%edi,%edx,4), %xmm4                        #10.17
        subps     (%edi,%edx,4), %xmm0                          #10.10
        movss     60(%edi,%edx,4), %xmm3                        #10.17
        movhps    64(%edi,%edx,4), %xmm3                        #10.17
        subps     16(%edi,%edx,4), %xmm1                        #10.10
        subps     32(%edi,%edx,4), %xmm2                        #10.10
        movups    %xmm0, (%eax,%edx,4)                          #10.10
        movups    %xmm1, 16(%eax,%edx,4)                        #10.10
        movups    %xmm2, 32(%eax,%edx,4)                        #10.10
        shufps    $132, %xmm3, %xmm4                            #10.17
        subps     48(%edi,%edx,4), %xmm4                        #10.10
        movups    %xmm4, 48(%eax,%edx,4)                        #10.10
        addl      $16, %edx                                     #9.10
        cmpl      %ecx, %edx                                    #9.10
        jb        ..B1.12       # Prob 99%                      #9.10
        jmp       ..B1.16       # Prob 100%                     #9.10
                                # LOE eax edx ecx ebx ebp esi edi
..B1.14:                        # Preds ..B1.10
        movl      20(%esp), %eax                                #
        movl      %edi, 4(%esp)                                 #
        movl      16(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi
..B1.15:                        # Preds ..B1.15 ..B1.14
        movups    4(%edi,%edx,4), %xmm0                         #10.17
        movups    20(%edi,%edx,4), %xmm1                        #10.17
        movups    36(%edi,%edx,4), %xmm2                        #10.17
        movsd     52(%edi,%edx,4), %xmm4                        #10.17
        subps     (%edi,%edx,4), %xmm0                          #10.10
        movss     60(%edi,%edx,4), %xmm3                        #10.17
        movhps    64(%edi,%edx,4), %xmm3                        #10.17
        subps     16(%edi,%edx,4), %xmm1                        #10.10
        subps     32(%edi,%edx,4), %xmm2                        #10.10
        movaps    %xmm0, (%eax,%edx,4)                          #10.10
        movaps    %xmm1, 16(%eax,%edx,4)                        #10.10
        movaps    %xmm2, 32(%eax,%edx,4)                        #10.10
        shufps    $132, %xmm3, %xmm4                            #10.17
        subps     48(%edi,%edx,4), %xmm4                        #10.10
        movaps    %xmm4, 48(%eax,%edx,4)                        #10.10
        addl      $16, %edx                                     #9.10
        cmpl      %ecx, %edx                                    #9.10
        jb        ..B1.15       # Prob 99%                      #9.10
                                # LOE eax edx ecx ebx ebp esi edi
..B1.16:                        # Preds ..B1.12 ..B1.15
        movl      4(%esp), %edi                                 #
                                # LOE ecx ebx ebp esi edi
..B1.17:                        # Preds ..B1.16 ..B1.22
        cmpl      %esi, %ecx                                    #9.10
        jae       ..B1.21       # Prob 1%                       #9.10
                                # LOE ecx ebx ebp esi edi
..B1.18:                        # Preds ..B1.17
        movl      20(%esp), %eax                                #
        movl      16(%esp), %edx                                #
                                # LOE eax edx ecx ebx ebp esi edi
..B1.19:                        # Preds ..B1.19 ..B1.18
        movss     4(%edx,%ecx,4), %xmm0                         #10.17
        subss     (%edx,%ecx,4), %xmm0                          #10.10
        movss     %xmm0, (%eax,%ecx,4)                          #10.10
        addl      $1, %ecx                                      #9.10
        cmpl      %esi, %ecx                                    #9.10
        jb        ..B1.19       # Prob 99%                      #9.10
                                # LOE eax edx ecx ebx ebp esi edi
..B1.21:                        # Preds ..B1.19 ..B1.17 ..B1.1
        addl      $8, %esp                                      #12.7
        popl      %esi                                          #12.7
        ret                                                     #12.7
                                # LOE
..B1.22:                        # Preds ..B1.5 ..B1.3           # Infreq
        xorl      %ecx, %ecx                                    #9.10
        jmp       ..B1.17       # Prob 100%                     #9.10
        .align    2,0x90
                                # LOE ecx ebx ebp esi edi
# mark_end;
	.type	delta_sp_,@function
	.size	delta_sp_,.-delta_sp_
	.data
# -- End  delta_sp_
	.data
	.section .note.GNU-stack, ""
# End
