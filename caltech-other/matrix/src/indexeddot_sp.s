# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "indexeddot_sp.f"
	.text
..TXTST0:
# -- Begin  indexeddot_sp_
# mark_begin;
       .align    2,0x90
	.globl indexeddot_sp_
indexeddot_sp_:
# parameter 1: 16 + %esp
# parameter 2: 20 + %esp
# parameter 3: 24 + %esp
# parameter 4: 28 + %esp
# parameter 5: 32 + %esp
..B1.1:                         # Preds ..B1.0
        subl      $12, %esp                                     #4.18
        movl      16(%esp), %ecx                                #4.18
        movl      24(%esp), %eax                                #4.18
        movl      (%eax), %edx                                  #11.10
        testl     %edx, %edx                                    #11.10
        pxor      %xmm0, %xmm0                                  #9.7
        jle       ..B1.6        # Prob 1%                       #11.10
                                # LOE edx ecx ebx ebp esi edi xmm0
..B1.2:                         # Preds ..B1.1
        movl      %ebx, 8(%esp)                                 #
        movl      %esi, 4(%esp)                                 #
        movl      28(%esp), %esi                                #
        movl      %edi, (%esp)                                  #
        movl      20(%esp), %edi                                #
        xorl      %eax, %eax                                    #
                                # LOE eax edx ecx ebp esi edi xmm0
..B1.3:                         # Preds ..B1.3 ..B1.2
        movl      (%edi,%eax,4), %ebx                           #15.10
        movss     (%ecx,%ebx,4), %xmm1                          #15.22
        mulss     (%esi,%eax,4), %xmm1                          #15.34
        addl      $1, %eax                                      #11.10
        cmpl      %edx, %eax                                    #11.10
        addss     %xmm1, %xmm0                                  #15.10
        jb        ..B1.3        # Prob 99%                      #11.10
                                # LOE eax edx ecx ebp esi edi xmm0
..B1.4:                         # Preds ..B1.3
        movl      8(%esp), %ebx                                 #
        movl      4(%esp), %esi                                 #
        movl      (%esp), %edi                                  #
        movl      32(%esp), %eax                                #15.10
        movss     %xmm0, (%eax)                                 #15.10
                                # LOE ebx ebp esi edi
..B1.5:                         # Preds ..B1.4 ..B1.6
        addl      $12, %esp                                     #17.7
        ret                                                     #17.7
                                # LOE
..B1.6:                         # Preds ..B1.1                  # Infreq
        movl      32(%esp), %edx                                #15.10
        xorl      %eax, %eax                                    #15.10
        movl      %eax, (%edx)                                  #15.10
        jmp       ..B1.5        # Prob 100%                     #15.10
        .align    2,0x90
                                # LOE ebx ebp esi edi
# mark_end;
	.type	indexeddot_sp_,@function
	.size	indexeddot_sp_,.-indexeddot_sp_
	.data
# -- End  indexeddot_sp_
	.data
	.section .note.GNU-stack, ""
# End
