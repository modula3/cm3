	.section .text
.LNDBG_TX:
# -- Machine type EFI2
# mark_description "Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 10.0    Build 20070809 %s";
# mark_description "-g -S -W1 -recursive -reentrancy threaded";
	.file "bigden.f"
	.data
	.text
..TXTST0:
# -- Begin  bigden_
# mark_begin;
       .align    2,0x90
	.globl bigden_
bigden_:
# parameter 1(n): %rdi
# parameter 2(npt): %rsi
# parameter 3(xopt): %rdx
# parameter 4(xpt): %rcx
# parameter 5(bmat): %r8
# parameter 6(zmat): %r9
# parameter 7(idz): 16 + %rbp
# parameter 8(ndim): 24 + %rbp
# parameter 9(kopt): 32 + %rbp
# parameter 10(knew): 40 + %rbp
# parameter 11(d): 48 + %rbp
# parameter 12(w): 56 + %rbp
# parameter 13(vlag): 64 + %rbp
# parameter 14(beta): 72 + %rbp
# parameter 15(s): 80 + %rbp
# parameter 16(wvec): 88 + %rbp
# parameter 17(prod): 96 + %rbp
..B1.1:                         # Preds ..B1.0
..___tag_value_bigden_.2:                                       #
..LN1:
        pushq     %rbp                                          #1.18
        movq      %rsp, %rbp                                    #1.18
..___tag_value_bigden_.9:                                       #
        subq      $880, %rsp                                    #1.18
        movq      %rbx, -432(%rbp)                              #1.18
..___tag_value_bigden_.12:                                      #
        movq      %rdi, -128(%rbp)                              #1.18
        movq      %rsi, -120(%rbp)                              #1.18
        movq      %rdx, -424(%rbp)                              #1.18
        movq      %rcx, -416(%rbp)                              #1.18
        movq      %r8, -112(%rbp)                               #1.18
        movq      %r9, -104(%rbp)                               #1.18
        movq      -120(%rbp), %rax                              #1.18
        movl      (%rax), %eax                                  #1.18
        movl      %eax, -64(%rbp)                               #1.18
        movq      24(%rbp), %rax                                #1.18
        movl      (%rax), %eax                                  #1.18
        movl      %eax, -60(%rbp)                               #1.18
        movl      -64(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -544(%rbp)                              #1.18
        movq      -544(%rbp), %rax                              #1.18
        movq      %rax, -536(%rbp)                              #1.18
        movl      -60(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -528(%rbp)                              #1.18
        movq      -528(%rbp), %rax                              #1.18
        movq      %rax, -520(%rbp)                              #1.18
        movl      -64(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -512(%rbp)                              #1.18
        movq      -512(%rbp), %rax                              #1.18
        movq      %rax, -504(%rbp)                              #1.18
        movl      -60(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -496(%rbp)                              #1.18
        movq      -496(%rbp), %rax                              #1.18
        movq      %rax, -488(%rbp)                              #1.18
        movl      -60(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -480(%rbp)                              #1.18
        movq      -480(%rbp), %rax                              #1.18
        movq      %rax, -472(%rbp)                              #1.18
..LN3:
        fldl      _2il0floatpacket.1(%rip)                      #33.7
        fstpl     -96(%rbp)                                     #33.7
..LN5:
        movq      $0x3ff0000000000000, %rax                     #34.7
        movq      %rax, -408(%rbp)                              #34.7
..LN7:
        fldl      _2il0floatpacket.2(%rip)                      #35.7
        fstpl     -400(%rbp)                                    #35.7
..LN9:
        fldl      _2il0floatpacket.3(%rip)                      #36.7
        fstpl     -392(%rbp)                                    #36.7
..LN11:
        movq      $0, -88(%rbp)                                 #37.7
..LN13:
        movsd     -408(%rbp), %xmm0                             #38.19
        movl      $1, %eax                                      #38.19
        call      atan                                          #38.19
                                # LOE xmm0
..B1.144:                       # Preds ..B1.1
        movsd     %xmm0, -440(%rbp)                             #38.19
                                # LOE
..B1.2:                         # Preds ..B1.144
..LN15:
        fldl      _2il0floatpacket.4(%rip)                      #38.18
..LN17:
        fldl      -440(%rbp)                                    #38.7
        fmulp     %st, %st(1)                                   #38.7
        fstpl     -384(%rbp)                                    #38.7
..LN19:
        movq      -120(%rbp), %rax                              #39.7
        movq      -128(%rbp), %rdx                              #39.7
        movl      (%rdx), %edx                                  #39.7
..LN21:
        negl      %edx                                          #39.15
..LN23:
        movl      (%rax), %eax                                  #39.7
        lea       -1(%rax,%rdx), %eax                           #39.7
        movl      %eax, -56(%rbp)                               #39.7
..LN25:
        movq      -120(%rbp), %rax                              #44.7
..LN27:
        movl      (%rax), %eax                                  #44.10
        movl      %eax, -208(%rbp)                              #44.10
        movl      $1, -52(%rbp)                                 #44.10
        movl      -208(%rbp), %eax                              #44.10
        testl     %eax, %eax                                    #44.10
        jle       ..B1.5        # Prob 50%                      #44.10
                                # LOE
..B1.4:                         # Preds ..B1.2 ..B1.4
..LN29:
        movq      -128(%rbp), %rax                              #45.4
        movl      -52(%rbp), %edx                               #45.4
        addl      (%rax), %edx                                  #45.4
..LN31:
        movslq    %edx, %rax                                    #45.7
..LN33:
        movq      56(%rbp), %rdx                                #45.4
..LN35:
        fldl      -88(%rbp)                                     #45.7
        fstpl     -8(%rdx,%rax,8)                               #45.7
..LN37:
        addl      $1, -52(%rbp)                                 #44.10
        movl      -52(%rbp), %eax                               #44.10
        movl      -208(%rbp), %edx                              #44.10
        cmpl      %edx, %eax                                    #44.10
        jle       ..B1.4        # Prob 50%                      #44.10
                                # LOE
..B1.5:                         # Preds ..B1.2 ..B1.4
..LN39:
        movl      -56(%rbp), %eax                               #46.10
        movl      %eax, -204(%rbp)                              #46.10
        movl      $1, -48(%rbp)                                 #46.10
        movl      -204(%rbp), %eax                              #46.10
        testl     %eax, %eax                                    #46.10
        jle       ..B1.13       # Prob 50%                      #46.10
                                # LOE
..B1.7:                         # Preds ..B1.5 ..B1.10
..LN41:
        movl      -64(%rbp), %eax                               #47.7
        movslq    %eax, %rax                                    #47.7
..LN43:
        shlq      $3, %rax                                      #47.12
..LN45:
        movl      -48(%rbp), %edx                               #47.7
..LN47:
        movslq    %edx, %rdx                                    #47.12
        imulq     %rax, %rdx                                    #47.12
..LN49:
        addq      -104(%rbp), %rdx                              #47.7
..LN51:
        movl      -64(%rbp), %eax                               #47.7
        movslq    %eax, %rax                                    #47.7
..LN53:
        shlq      $3, %rax                                      #47.12
        negq      %rax                                          #47.12
..LN55:
        addq      %rax, %rdx                                    #47.7
        movq      40(%rbp), %rax                                #47.7
        movl      (%rax), %eax                                  #47.7
..LN57:
        movslq    %eax, %rax                                    #47.12
..LN59:
        fldl      -8(%rdx,%rax,8)                               #47.7
        fstpl     -344(%rbp)                                    #47.7
..LN61:
        movq      16(%rbp), %rax                                #48.7
        movl      -48(%rbp), %edx                               #48.7
        movl      (%rax), %eax                                  #48.7
..LN63:
        cmpl      %eax, %edx                                    #48.13
        jge       ..B1.9        # Prob 50%                      #48.13
                                # LOE
..B1.8:                         # Preds ..B1.7
..LN65:
        fldl      -344(%rbp)                                    #48.23
        fchs                                                    #48.23
        fstpl     -344(%rbp)                                    #48.23
                                # LOE
..B1.9:                         # Preds ..B1.8 ..B1.7
..LN67:
        movq      -120(%rbp), %rax                              #49.7
..LN69:
        movl      (%rax), %eax                                  #49.10
        movl      %eax, -196(%rbp)                              #49.10
        movl      $1, -52(%rbp)                                 #49.10
        movl      -196(%rbp), %eax                              #49.10
        testl     %eax, %eax                                    #49.10
        jg        ..B1.12       # Prob 50%                      #49.10
                                # LOE
..B1.10:                        # Preds ..B1.9 ..B1.12
..LN71:
        addl      $1, -48(%rbp)                                 #46.10
        movl      -48(%rbp), %eax                               #46.10
        movl      -204(%rbp), %edx                              #46.10
        cmpl      %edx, %eax                                    #46.10
        jle       ..B1.7        # Prob 50%                      #46.10
        jmp       ..B1.13       # Prob 100%                     #46.10
                                # LOE
..B1.12:                        # Preds ..B1.9 ..B1.12
..LN73:
        movq      -128(%rbp), %rax                              #50.7
        movl      -52(%rbp), %edx                               #50.7
        addl      (%rax), %edx                                  #50.7
..LN75:
        movslq    %edx, %rax                                    #50.14
..LN77:
        movq      56(%rbp), %rdx                                #50.7
..LN79:
        fldl      -344(%rbp)                                    #50.14
        movl      -64(%rbp), %ecx                               #50.14
        movslq    %ecx, %rcx                                    #50.14
..LN81:
        shlq      $3, %rcx                                      #50.26
..LN83:
        movl      -48(%rbp), %ebx                               #50.14
..LN85:
        movslq    %ebx, %rbx                                    #50.26
        imulq     %rcx, %rbx                                    #50.26
..LN87:
        addq      -104(%rbp), %rbx                              #50.7
..LN89:
        movl      -64(%rbp), %ecx                               #50.14
        movslq    %ecx, %rcx                                    #50.14
..LN91:
        shlq      $3, %rcx                                      #50.26
        negq      %rcx                                          #50.26
..LN93:
        addq      %rcx, %rbx                                    #50.7
..LN95:
        movl      -52(%rbp), %ecx                               #50.14
..LN97:
        movslq    %ecx, %rcx                                    #50.26
        fldl      -8(%rbx,%rcx,8)                               #50.26
..LN99:
        fmulp     %st, %st(1)                                   #50.25
..LN101:
        fldl      -8(%rdx,%rax,8)                               #50.14
..LN103:
        faddp     %st, %st(1)                                   #50.7
..LN105:
        movq      -128(%rbp), %rax                              #50.4
        movl      -52(%rbp), %edx                               #50.4
        addl      (%rax), %edx                                  #50.4
..LN107:
        movslq    %edx, %rax                                    #50.7
..LN109:
        movq      56(%rbp), %rdx                                #50.4
..LN111:
        fstpl     -8(%rdx,%rax,8)                               #50.7
..LN113:
        addl      $1, -52(%rbp)                                 #49.10
        movl      -52(%rbp), %eax                               #49.10
        movl      -196(%rbp), %edx                              #49.10
        cmpl      %edx, %eax                                    #49.10
        jle       ..B1.12       # Prob 50%                      #49.10
        jmp       ..B1.10       # Prob 100%                     #49.10
                                # LOE
..B1.13:                        # Preds ..B1.5 ..B1.10
..LN115:
        movq      -128(%rbp), %rax                              #51.7
        movq      40(%rbp), %rdx                                #51.7
        movl      (%rdx), %edx                                  #51.7
        addl      (%rax), %edx                                  #51.7
..LN117:
        movslq    %edx, %rax                                    #51.13
..LN119:
        movq      56(%rbp), %rdx                                #51.7
        fldl      -8(%rdx,%rax,8)                               #51.7
        fstpl     -376(%rbp)                                    #51.7
..LN121:
        fldl      -88(%rbp)                                     #58.7
        fstpl     -368(%rbp)                                    #58.7
..LN123:
        fldl      -88(%rbp)                                     #59.7
        fstpl     -360(%rbp)                                    #59.7
..LN125:
        fldl      -88(%rbp)                                     #60.7
        fstpl     -464(%rbp)                                    #60.7
..LN127:
        fldl      -88(%rbp)                                     #61.7
        fstpl     -352(%rbp)                                    #61.7
..LN129:
        movq      -128(%rbp), %rax                              #62.7
..LN131:
        movl      (%rax), %eax                                  #62.10
        movl      %eax, -200(%rbp)                              #62.10
        movl      $1, -44(%rbp)                                 #62.10
        movl      -200(%rbp), %eax                              #62.10
        testl     %eax, %eax                                    #62.10
        jle       ..B1.16       # Prob 50%                      #62.10
                                # LOE
..B1.15:                        # Preds ..B1.13 ..B1.15
..LN133:
        movl      -44(%rbp), %eax                               #63.7
..LN135:
        movslq    %eax, %rax                                    #63.13
..LN137:
        movq      48(%rbp), %rdx                                #63.7
..LN139:
        fldl      -8(%rdx,%rax,8)                               #63.17
        fmul      %st(0), %st                                   #63.17
..LN141:
        fldl      -368(%rbp)                                    #63.7
        faddp     %st, %st(1)                                   #63.7
        fstpl     -368(%rbp)                                    #63.7
..LN143:
        movl      -64(%rbp), %eax                               #64.7
        movslq    %eax, %rax                                    #64.7
..LN145:
        shlq      $3, %rax                                      #64.12
..LN147:
        movl      -44(%rbp), %edx                               #64.7
..LN149:
        movslq    %edx, %rdx                                    #64.12
        imulq     %rax, %rdx                                    #64.12
..LN151:
        addq      -416(%rbp), %rdx                              #64.7
        movl      -64(%rbp), %eax                               #64.7
        movslq    %eax, %rax                                    #64.7
..LN153:
        shlq      $3, %rax                                      #64.12
        negq      %rax                                          #64.12
..LN155:
        addq      %rax, %rdx                                    #64.7
        movq      40(%rbp), %rax                                #64.7
        movl      (%rax), %eax                                  #64.7
..LN157:
        movslq    %eax, %rax                                    #64.12
        movl      -44(%rbp), %ecx                               #64.12
..LN159:
        movslq    %ecx, %rcx                                    #64.24
..LN161:
        movq      -424(%rbp), %rbx                              #64.12
        fldl      -8(%rdx,%rax,8)                               #64.12
..LN163:
        fldl      -8(%rbx,%rcx,8)                               #64.24
..LN165:
        fsubrp    %st, %st(1)                                   #64.7
        movl      -44(%rbp), %eax                               #64.7
        movslq    %eax, %rax                                    #64.7
        movq      80(%rbp), %rdx                                #64.7
        fstpl     -8(%rdx,%rax,8)                               #64.7
..LN167:
        movl      -44(%rbp), %eax                               #65.7
..LN169:
        movslq    %eax, %rax                                    #65.13
..LN171:
        movq      48(%rbp), %rdx                                #65.7
..LN173:
        fldl      -8(%rdx,%rax,8)                               #65.13
        movl      -44(%rbp), %eax                               #65.13
..LN175:
        movslq    %eax, %rax                                    #65.18
..LN177:
        movq      80(%rbp), %rdx                                #65.13
..LN179:
        fldl      -8(%rdx,%rax,8)                               #65.18
..LN181:
        fmulp     %st, %st(1)                                   #65.17
..LN183:
        fldl      -360(%rbp)                                    #65.7
        faddp     %st, %st(1)                                   #65.7
        fstpl     -360(%rbp)                                    #65.7
..LN185:
        movl      -44(%rbp), %eax                               #66.7
..LN187:
        movslq    %eax, %rax                                    #66.13
..LN189:
        movq      80(%rbp), %rdx                                #66.7
..LN191:
        fldl      -8(%rdx,%rax,8)                               #66.17
        fmul      %st(0), %st                                   #66.17
..LN193:
        fldl      -464(%rbp)                                    #66.7
        faddp     %st, %st(1)                                   #66.7
        fstpl     -464(%rbp)                                    #66.7
..LN195:
        movl      -44(%rbp), %eax                               #67.4
..LN197:
        movslq    %eax, %rax                                    #67.21
..LN199:
        movq      -424(%rbp), %rdx                              #67.4
..LN201:
        fldl      -8(%rdx,%rax,8)                               #67.28
        fmul      %st(0), %st                                   #67.28
..LN203:
        fldl      -352(%rbp)                                    #67.4
..LN205:
        faddp     %st, %st(1)                                   #67.7
        fstpl     -352(%rbp)                                    #67.7
..LN207:
        addl      $1, -44(%rbp)                                 #62.10
        movl      -44(%rbp), %eax                               #62.10
        movl      -200(%rbp), %edx                              #62.10
        cmpl      %edx, %eax                                    #62.10
        jle       ..B1.15       # Prob 50%                      #62.10
                                # LOE
..B1.16:                        # Preds ..B1.13 ..B1.15
..LN209:
        fldl      -360(%rbp)                                    #68.7
        fldl      -360(%rbp)                                    #68.7
..LN211:
        fmulp     %st, %st(1)                                   #68.13
..LN213:
        fldl      _2il0floatpacket.5(%rip)                      #68.28
..LN215:
        fldl      -368(%rbp)                                    #68.13
..LN217:
        fmulp     %st, %st(1)                                   #68.28
        fldl      -464(%rbp)                                    #68.28
..LN219:
        fmulp     %st, %st(1)                                   #68.31
..LN221:
        fxch      %st(1)                                        #68.17
        fstpl     -216(%rbp)                                    #68.17
        movsd     -216(%rbp), %xmm0                             #68.17
        fstpl     -216(%rbp)                                    #68.17
        movsd     -216(%rbp), %xmm1                             #68.17
        comisd    %xmm1, %xmm0                                  #68.17
        jbe       ..B1.29       # Prob 50%                      #68.17
                                # LOE
..B1.17:                        # Preds ..B1.16
..LN223:
        movq      40(%rbp), %rax                                #69.11
        movl      (%rax), %eax                                  #69.11
        movl      %eax, -556(%rbp)                              #69.11
..LN225:
        fldl      -360(%rbp)                                    #70.11
        fldl      -360(%rbp)                                    #70.11
..LN227:
        fmulp     %st, %st(1)                                   #70.19
        fldl      -464(%rbp)                                    #70.19
..LN229:
        fdivrp    %st, %st(1)                                   #70.11
        fstpl     -616(%rbp)                                    #70.11
..LN231:
        movq      -120(%rbp), %rax                              #71.11
..LN233:
        movl      (%rax), %eax                                  #71.14
        movl      %eax, -552(%rbp)                              #71.14
        movl      $1, -52(%rbp)                                 #71.14
        movl      -552(%rbp), %eax                              #71.14
        testl     %eax, %eax                                    #71.14
        jle       ..B1.26       # Prob 50%                      #71.14
                                # LOE
..B1.19:                        # Preds ..B1.17 ..B1.25
..LN235:
        movq      32(%rbp), %rax                                #72.11
        movl      -52(%rbp), %edx                               #72.11
        movl      (%rax), %eax                                  #72.11
..LN237:
        cmpl      %eax, %edx                                    #72.17
        je        ..B1.25       # Prob 50%                      #72.17
                                # LOE
..B1.20:                        # Preds ..B1.19
..LN239:
        fldl      -88(%rbp)                                     #73.15
        fstpl     -872(%rbp)                                    #73.15
..LN241:
        fldl      -88(%rbp)                                     #74.15
        fstpl     -864(%rbp)                                    #74.15
..LN243:
        movq      -128(%rbp), %rax                              #75.15
..LN245:
        movl      (%rax), %eax                                  #75.18
        movl      %eax, -624(%rbp)                              #75.18
        movl      $1, -44(%rbp)                                 #75.18
        movl      -624(%rbp), %eax                              #75.18
        testl     %eax, %eax                                    #75.18
        jle       ..B1.23       # Prob 50%                      #75.18
                                # LOE
..B1.22:                        # Preds ..B1.20 ..B1.22
..LN247:
        movl      -64(%rbp), %eax                               #76.15
        movslq    %eax, %rax                                    #76.15
..LN249:
        shlq      $3, %rax                                      #76.20
..LN251:
        movl      -44(%rbp), %edx                               #76.15
..LN253:
        movslq    %edx, %rdx                                    #76.20
        imulq     %rax, %rdx                                    #76.20
..LN255:
        addq      -416(%rbp), %rdx                              #76.15
..LN257:
        movl      -64(%rbp), %eax                               #76.15
        movslq    %eax, %rax                                    #76.15
..LN259:
        shlq      $3, %rax                                      #76.20
        negq      %rax                                          #76.20
..LN261:
        addq      %rax, %rdx                                    #76.15
        movl      -52(%rbp), %eax                               #76.15
..LN263:
        movslq    %eax, %rax                                    #76.20
        movl      -44(%rbp), %ecx                               #76.20
..LN265:
        movslq    %ecx, %rcx                                    #76.29
..LN267:
        movq      -424(%rbp), %rbx                              #76.20
        fldl      -8(%rdx,%rax,8)                               #76.20
..LN269:
        fldl      -8(%rbx,%rcx,8)                               #76.29
..LN271:
        fsubrp    %st, %st(1)                                   #76.15
        fstpl     -856(%rbp)                                    #76.15
..LN273:
        movl      -44(%rbp), %eax                               #77.15
..LN275:
        movslq    %eax, %rax                                    #77.29
..LN277:
        movq      48(%rbp), %rdx                                #77.15
..LN279:
        fldl      -8(%rdx,%rax,8)                               #77.29
        fldl      -856(%rbp)                                    #77.29
..LN281:
        fmulp     %st, %st(1)                                   #77.33
..LN283:
        fldl      -872(%rbp)                                    #77.15
        faddp     %st, %st(1)                                   #77.15
        fstpl     -872(%rbp)                                    #77.15
..LN285:
        fldl      -856(%rbp)                                    #78.4
        fldl      -856(%rbp)                                    #78.4
..LN287:
        fmulp     %st, %st(1)                                   #78.33
..LN289:
        fldl      -864(%rbp)                                    #78.4
..LN291:
        faddp     %st, %st(1)                                   #78.15
        fstpl     -864(%rbp)                                    #78.15
..LN293:
        addl      $1, -44(%rbp)                                 #75.18
        movl      -44(%rbp), %eax                               #75.18
        movl      -624(%rbp), %edx                              #75.18
        cmpl      %edx, %eax                                    #75.18
        jle       ..B1.22       # Prob 50%                      #75.18
                                # LOE
..B1.23:                        # Preds ..B1.20 ..B1.22
..LN295:
        fldl      -872(%rbp)                                    #79.15
        fldl      -872(%rbp)                                    #79.15
..LN297:
        fmulp     %st, %st(1)                                   #79.25
        fldl      -864(%rbp)                                    #79.25
..LN299:
        fdivrp    %st, %st(1)                                   #79.32
        fldl      -616(%rbp)                                    #79.32
..LN301:
        fxch      %st(1)                                        #79.40
        fstpl     -216(%rbp)                                    #79.40
        fldl      -216(%rbp)                                    #79.40
        fcomip    %st(1), %st                                   #79.40
        fstp      %st(0)                                        #79.40
        jae       ..B1.25       # Prob 50%                      #79.40
        jp        ..B1.25       # Prob 0%                       #79.40
                                # LOE
..B1.24:                        # Preds ..B1.23
..LN303:
        movl      -52(%rbp), %eax                               #80.19
        movl      %eax, -556(%rbp)                              #80.19
..LN305:
        fldl      -872(%rbp)                                    #81.19
        fldl      -872(%rbp)                                    #81.19
..LN307:
        fmulp     %st, %st(1)                                   #81.31
        fldl      -864(%rbp)                                    #81.31
..LN309:
        fdivrp    %st, %st(1)                                   #81.19
        fstpl     -616(%rbp)                                    #81.19
..LN311:
        fldl      -872(%rbp)                                    #82.19
        fstpl     -360(%rbp)                                    #82.19
..LN313:
        fldl      -864(%rbp)                                    #83.19
        fstpl     -464(%rbp)                                    #83.19
                                # LOE
..B1.25:                        # Preds ..B1.24 ..B1.23 ..B1.19
..LN315:
        movq      $0, -608(%rbp)                                #86.4
..LN317:
        addl      $1, -52(%rbp)                                 #71.14
        movl      -52(%rbp), %eax                               #71.14
        movl      -552(%rbp), %edx                              #71.14
        cmpl      %edx, %eax                                    #71.14
        jle       ..B1.19       # Prob 50%                      #71.14
                                # LOE
..B1.26:                        # Preds ..B1.17 ..B1.25
..LN319:
        movq      -128(%rbp), %rax                              #87.11
..LN321:
        movl      (%rax), %eax                                  #87.14
        movl      %eax, -548(%rbp)                              #87.14
        movl      $1, -44(%rbp)                                 #87.14
        movl      -548(%rbp), %eax                              #87.14
        testl     %eax, %eax                                    #87.14
        jle       ..B1.29       # Prob 50%                      #87.14
                                # LOE
..B1.28:                        # Preds ..B1.26 ..B1.28
..LN323:
        movl      -64(%rbp), %eax                               #88.11
        movslq    %eax, %rax                                    #88.11
..LN325:
        shlq      $3, %rax                                      #88.16
..LN327:
        movl      -44(%rbp), %edx                               #88.11
..LN329:
        movslq    %edx, %rdx                                    #88.16
        imulq     %rax, %rdx                                    #88.16
..LN331:
        addq      -416(%rbp), %rdx                              #88.11
..LN333:
        movl      -64(%rbp), %eax                               #88.11
        movslq    %eax, %rax                                    #88.11
..LN335:
        shlq      $3, %rax                                      #88.16
        negq      %rax                                          #88.16
..LN337:
        addq      %rax, %rdx                                    #88.11
        movl      -556(%rbp), %eax                              #88.11
..LN339:
        movslq    %eax, %rax                                    #88.16
        movl      -44(%rbp), %ecx                               #88.16
..LN341:
        movslq    %ecx, %rcx                                    #88.28
..LN343:
        movq      -424(%rbp), %rbx                              #88.16
        fldl      -8(%rdx,%rax,8)                               #88.16
..LN345:
        fldl      -8(%rbx,%rcx,8)                               #88.28
..LN347:
        fsubrp    %st, %st(1)                                   #88.11
..LN349:
        movl      -44(%rbp), %eax                               #88.4
..LN351:
        movslq    %eax, %rax                                    #88.11
..LN353:
        movq      80(%rbp), %rdx                                #88.4
..LN355:
        fstpl     -8(%rdx,%rax,8)                               #88.11
..LN357:
        addl      $1, -44(%rbp)                                 #87.14
        movl      -44(%rbp), %eax                               #87.14
        movl      -548(%rbp), %edx                              #87.14
        cmpl      %edx, %eax                                    #87.14
        jle       ..B1.28       # Prob 50%                      #87.14
                                # LOE
..B1.29:                        # Preds ..B1.26 ..B1.28 ..B1.16
..LN359:
        fldl      -368(%rbp)                                    #90.7
        fldl      -464(%rbp)                                    #90.7
..LN361:
        fmulp     %st, %st(1)                                   #90.15
        fldl      -360(%rbp)                                    #90.15
        fldl      -360(%rbp)                                    #90.15
..LN363:
        fmulp     %st, %st(1)                                   #90.21
..LN365:
        fsubrp    %st, %st(1)                                   #90.7
        fstpl     -336(%rbp)                                    #90.7
..LN367:
        movl      $0, -188(%rbp)                                #91.7
..LN369:
        fldl      -88(%rbp)                                     #92.7
        fstpl     -456(%rbp)                                    #92.7
                                # LOE
..B1.30:                        # Preds ..B1.135 ..B1.29
..LN371:
        addl      $1, -188(%rbp)                                #97.7
..LN373:
        movsd     -336(%rbp), %xmm0                             #98.16
        movl      $1, %eax                                      #98.16
        call      sqrt                                          #98.16
                                # LOE xmm0
..B1.145:                       # Preds ..B1.30
        movsd     %xmm0, -240(%rbp)                             #98.16
                                # LOE
..B1.31:                        # Preds ..B1.145
..LN375:
        fldl      -408(%rbp)                                    #98.7
        fldl      -240(%rbp)                                    #98.7
        fdivrp    %st, %st(1)                                   #98.7
        fstpl     -344(%rbp)                                    #98.7
..LN377:
        fldl      -88(%rbp)                                     #99.7
        fstpl     -328(%rbp)                                    #99.7
..LN379:
        fldl      -88(%rbp)                                     #100.7
        fstpl     -320(%rbp)                                    #100.7
..LN381:
        movq      -128(%rbp), %rax                              #101.7
..LN383:
        movl      (%rax), %eax                                  #101.10
        movl      %eax, -184(%rbp)                              #101.10
        movl      $1, -44(%rbp)                                 #101.10
        movl      -184(%rbp), %eax                              #101.10
        testl     %eax, %eax                                    #101.10
        jle       ..B1.34       # Prob 50%                      #101.10
                                # LOE
..B1.33:                        # Preds ..B1.31 ..B1.33
..LN385:
        fldl      -344(%rbp)                                    #102.7
        fldl      -368(%rbp)                                    #102.7
        movl      -44(%rbp), %eax                               #102.7
..LN387:
        movslq    %eax, %rax                                    #102.21
..LN389:
        movq      80(%rbp), %rdx                                #102.7
..LN391:
        fldl      -8(%rdx,%rax,8)                               #102.21
..LN393:
        fmulp     %st, %st(1)                                   #102.20
        fldl      -360(%rbp)                                    #102.20
        movl      -44(%rbp), %eax                               #102.20
..LN395:
        movslq    %eax, %rax                                    #102.29
..LN397:
        movq      48(%rbp), %rdx                                #102.20
..LN399:
        fldl      -8(%rdx,%rax,8)                               #102.29
..LN401:
        fmulp     %st, %st(1)                                   #102.28
..LN403:
        fsubrp    %st, %st(1)                                   #102.25
..LN405:
        fmulp     %st, %st(1)                                   #102.7
        movl      -44(%rbp), %eax                               #102.7
        movslq    %eax, %rax                                    #102.7
        movq      80(%rbp), %rdx                                #102.7
        fstpl     -8(%rdx,%rax,8)                               #102.7
..LN407:
        movl      -44(%rbp), %eax                               #103.7
..LN409:
        movslq    %eax, %rax                                    #103.19
..LN411:
        movq      -424(%rbp), %rdx                              #103.7
..LN413:
        fldl      -8(%rdx,%rax,8)                               #103.19
        movl      -44(%rbp), %eax                               #103.19
..LN415:
        movslq    %eax, %rax                                    #103.27
..LN417:
        movq      48(%rbp), %rdx                                #103.19
..LN419:
        fldl      -8(%rdx,%rax,8)                               #103.27
..LN421:
        fmulp     %st, %st(1)                                   #103.26
..LN423:
        fldl      -328(%rbp)                                    #103.7
        faddp     %st, %st(1)                                   #103.7
        fstpl     -328(%rbp)                                    #103.7
..LN425:
        movl      -44(%rbp), %eax                               #104.4
..LN427:
        movslq    %eax, %rax                                    #104.19
..LN429:
        movq      -424(%rbp), %rdx                              #104.4
..LN431:
        fldl      -8(%rdx,%rax,8)                               #104.19
        movl      -44(%rbp), %eax                               #104.19
..LN433:
        movslq    %eax, %rax                                    #104.27
..LN435:
        movq      80(%rbp), %rdx                                #104.19
..LN437:
        fldl      -8(%rdx,%rax,8)                               #104.27
..LN439:
        fmulp     %st, %st(1)                                   #104.26
..LN441:
        fldl      -320(%rbp)                                    #104.4
..LN443:
        faddp     %st, %st(1)                                   #104.7
        fstpl     -320(%rbp)                                    #104.7
..LN445:
        addl      $1, -44(%rbp)                                 #101.10
        movl      -44(%rbp), %eax                               #101.10
        movl      -184(%rbp), %edx                              #101.10
        cmpl      %edx, %eax                                    #101.10
        jle       ..B1.33       # Prob 50%                      #101.10
                                # LOE
..B1.34:                        # Preds ..B1.31 ..B1.33
..LN447:
        fldl      -96(%rbp)                                     #108.7
        fldl      -328(%rbp)                                    #108.7
..LN449:
        fmulp     %st, %st(1)                                   #108.17
        fldl      -328(%rbp)                                    #108.17
..LN451:
        fmulp     %st, %st(1)                                   #108.7
        fstpl     -312(%rbp)                                    #108.7
..LN453:
        fldl      -96(%rbp)                                     #109.7
        fldl      -320(%rbp)                                    #109.7
..LN455:
        fmulp     %st, %st(1)                                   #109.17
        fldl      -320(%rbp)                                    #109.17
..LN457:
        fmulp     %st, %st(1)                                   #109.7
        fstpl     -304(%rbp)                                    #109.7
..LN459:
        fldl      -368(%rbp)                                    #110.7
        fldl      -96(%rbp)                                     #110.7
        fldl      -368(%rbp)                                    #110.7
..LN461:
        fmulp     %st, %st(1)                                   #110.29
..LN463:
        fldl      -352(%rbp)                                    #110.7
..LN465:
        faddp     %st, %st(1)                                   #110.24
..LN467:
        fmulp     %st, %st(1)                                   #110.16
        fldl      -312(%rbp)                                    #110.16
..LN469:
        faddp     %st, %st(1)                                   #110.33
        fldl      -304(%rbp)                                    #110.33
..LN471:
        faddp     %st, %st(1)                                   #110.7
        fstpl     -848(%rbp)                                    #110.7
..LN473:
        fldl      -392(%rbp)                                    #111.7
        fldl      -328(%rbp)                                    #111.7
..LN475:
        fmulp     %st, %st(1)                                   #111.17
        fldl      -368(%rbp)                                    #111.17
..LN477:
        fmulp     %st, %st(1)                                   #111.7
        fstpl     -840(%rbp)                                    #111.7
..LN479:
        fldl      -392(%rbp)                                    #112.7
        fldl      -320(%rbp)                                    #112.7
..LN481:
        fmulp     %st, %st(1)                                   #112.17
        fldl      -368(%rbp)                                    #112.17
..LN483:
        fmulp     %st, %st(1)                                   #112.7
        fstpl     -832(%rbp)                                    #112.7
..LN485:
        fldl      -312(%rbp)                                    #113.7
        fldl      -304(%rbp)                                    #113.7
        fsubrp    %st, %st(1)                                   #113.7
        fstpl     -824(%rbp)                                    #113.7
..LN487:
        fldl      -328(%rbp)                                    #114.7
        fldl      -320(%rbp)                                    #114.7
        fmulp     %st, %st(1)                                   #114.7
        fstpl     -816(%rbp)                                    #114.7
..LN489:
        movl      $6, -44(%rbp)                                 #115.10
                                # LOE
..B1.35:                        # Preds ..B1.35 ..B1.34
..LN491:
        movl      -44(%rbp), %eax                               #116.4
        movslq    %eax, %rax                                    #116.4
..LN493:
        fldl      -88(%rbp)                                     #116.7
        fstpl     -856(%rbp,%rax,8)                             #116.7
..LN495:
        addl      $1, -44(%rbp)                                 #115.10
        movl      -44(%rbp), %eax                               #115.10
        cmpl      $9, %eax                                      #115.10
        jle       ..B1.35       # Prob 50%                      #115.10
                                # LOE
..B1.36:                        # Preds ..B1.35
..LN497:
        movq      -120(%rbp), %rax                              #120.7
..LN499:
        movl      (%rax), %eax                                  #120.10
        movl      %eax, -180(%rbp)                              #120.10
        movl      $1, -52(%rbp)                                 #120.10
        movl      -180(%rbp), %eax                              #120.10
        testl     %eax, %eax                                    #120.10
        jle       ..B1.42       # Prob 50%                      #120.10
                                # LOE
..B1.38:                        # Preds ..B1.36 ..B1.41
..LN501:
        fldl      -88(%rbp)                                     #121.7
        fstpl     -312(%rbp)                                    #121.7
..LN503:
        fldl      -88(%rbp)                                     #122.7
        fstpl     -304(%rbp)                                    #122.7
..LN505:
        fldl      -88(%rbp)                                     #123.7
        fstpl     -296(%rbp)                                    #123.7
..LN507:
        movq      -128(%rbp), %rax                              #124.7
..LN509:
        movl      (%rax), %eax                                  #124.10
        movl      %eax, -172(%rbp)                              #124.10
        movl      $1, -44(%rbp)                                 #124.10
        movl      -172(%rbp), %eax                              #124.10
        testl     %eax, %eax                                    #124.10
        jle       ..B1.41       # Prob 50%                      #124.10
                                # LOE
..B1.40:                        # Preds ..B1.38 ..B1.40
..LN511:
        movl      -64(%rbp), %eax                               #125.7
        movslq    %eax, %rax                                    #125.7
..LN513:
        shlq      $3, %rax                                      #125.19
..LN515:
        movl      -44(%rbp), %edx                               #125.7
..LN517:
        movslq    %edx, %rdx                                    #125.19
        imulq     %rax, %rdx                                    #125.19
..LN519:
        addq      -416(%rbp), %rdx                              #125.7
..LN521:
        movl      -64(%rbp), %eax                               #125.7
        movslq    %eax, %rax                                    #125.7
..LN523:
        shlq      $3, %rax                                      #125.19
        negq      %rax                                          #125.19
..LN525:
        addq      %rax, %rdx                                    #125.7
        movl      -52(%rbp), %eax                               #125.7
..LN527:
        movslq    %eax, %rax                                    #125.19
        fldl      -8(%rdx,%rax,8)                               #125.19
        movl      -44(%rbp), %eax                               #125.19
..LN529:
        movslq    %eax, %rax                                    #125.28
..LN531:
        movq      48(%rbp), %rdx                                #125.19
..LN533:
        fldl      -8(%rdx,%rax,8)                               #125.28
..LN535:
        fmulp     %st, %st(1)                                   #125.27
..LN537:
        fldl      -312(%rbp)                                    #125.7
        faddp     %st, %st(1)                                   #125.7
        fstpl     -312(%rbp)                                    #125.7
..LN539:
        movl      -64(%rbp), %eax                               #126.7
        movslq    %eax, %rax                                    #126.7
..LN541:
        shlq      $3, %rax                                      #126.19
..LN543:
        movl      -44(%rbp), %edx                               #126.7
..LN545:
        movslq    %edx, %rdx                                    #126.19
        imulq     %rax, %rdx                                    #126.19
..LN547:
        addq      -416(%rbp), %rdx                              #126.7
        movl      -64(%rbp), %eax                               #126.7
        movslq    %eax, %rax                                    #126.7
..LN549:
        shlq      $3, %rax                                      #126.19
        negq      %rax                                          #126.19
..LN551:
        addq      %rax, %rdx                                    #126.7
        movl      -52(%rbp), %eax                               #126.7
..LN553:
        movslq    %eax, %rax                                    #126.19
        fldl      -8(%rdx,%rax,8)                               #126.19
        movl      -44(%rbp), %eax                               #126.19
..LN555:
        movslq    %eax, %rax                                    #126.28
..LN557:
        movq      80(%rbp), %rdx                                #126.19
..LN559:
        fldl      -8(%rdx,%rax,8)                               #126.28
..LN561:
        fmulp     %st, %st(1)                                   #126.27
..LN563:
        fldl      -304(%rbp)                                    #126.7
        faddp     %st, %st(1)                                   #126.7
        fstpl     -304(%rbp)                                    #126.7
..LN565:
        movl      -64(%rbp), %eax                               #127.3
        movslq    %eax, %rax                                    #127.3
..LN567:
        shlq      $3, %rax                                      #127.19
..LN569:
        movl      -44(%rbp), %edx                               #127.3
..LN571:
        movslq    %edx, %rdx                                    #127.19
        imulq     %rax, %rdx                                    #127.19
..LN573:
        addq      -416(%rbp), %rdx                              #127.7
..LN575:
        movl      -64(%rbp), %eax                               #127.3
        movslq    %eax, %rax                                    #127.3
..LN577:
        shlq      $3, %rax                                      #127.19
        negq      %rax                                          #127.19
..LN579:
        addq      %rax, %rdx                                    #127.7
..LN581:
        movl      -52(%rbp), %eax                               #127.3
..LN583:
        movslq    %eax, %rax                                    #127.19
        fldl      -8(%rdx,%rax,8)                               #127.19
        movl      -44(%rbp), %eax                               #127.19
..LN585:
        movslq    %eax, %rax                                    #127.28
..LN587:
        movq      -424(%rbp), %rdx                              #127.19
..LN589:
        fldl      -8(%rdx,%rax,8)                               #127.28
..LN591:
        fmulp     %st, %st(1)                                   #127.27
..LN593:
        fldl      -296(%rbp)                                    #127.3
..LN595:
        faddp     %st, %st(1)                                   #127.7
        fstpl     -296(%rbp)                                    #127.7
..LN597:
        addl      $1, -44(%rbp)                                 #124.10
        movl      -44(%rbp), %eax                               #124.10
        movl      -172(%rbp), %edx                              #124.10
        cmpl      %edx, %eax                                    #124.10
        jle       ..B1.40       # Prob 50%                      #124.10
                                # LOE
..B1.41:                        # Preds ..B1.38 ..B1.40
..LN599:
        fldl      -400(%rbp)                                    #128.7
        fldl      -312(%rbp)                                    #128.7
        fldl      -312(%rbp)                                    #128.7
..LN601:
        fmulp     %st, %st(1)                                   #128.29
        fldl      -304(%rbp)                                    #128.29
        fldl      -304(%rbp)                                    #128.29
..LN603:
        fmulp     %st, %st(1)                                   #128.41
..LN605:
        faddp     %st, %st(1)                                   #128.35
..LN607:
        fmulp     %st, %st(1)                                   #128.7
        movl      -60(%rbp), %eax                               #128.7
..LN609:
        movslq    %eax, %rax                                    #128.7
        movq      88(%rbp), %rdx                                #128.7
        lea       (%rdx,%rax,8), %rax                           #128.7
        movl      -60(%rbp), %edx                               #128.7
        movslq    %edx, %rdx                                    #128.7
        shlq      $3, %rdx                                      #128.7
        negq      %rdx                                          #128.7
        addq      %rdx, %rax                                    #128.7
        movl      -52(%rbp), %edx                               #128.7
        movslq    %edx, %rdx                                    #128.7
        fstpl     -8(%rax,%rdx,8)                               #128.7
..LN611:
        fldl      -312(%rbp)                                    #129.7
        fldl      -296(%rbp)                                    #129.7
        fmulp     %st, %st(1)                                   #129.7
        movl      -60(%rbp), %eax                               #129.7
        movslq    %eax, %rax                                    #129.7
        shlq      $4, %rax                                      #129.7
        addq      88(%rbp), %rax                                #129.7
        movl      -60(%rbp), %edx                               #129.7
        movslq    %edx, %rdx                                    #129.7
        shlq      $3, %rdx                                      #129.7
        negq      %rdx                                          #129.7
        addq      %rdx, %rax                                    #129.7
        movl      -52(%rbp), %edx                               #129.7
        movslq    %edx, %rdx                                    #129.7
        fstpl     -8(%rax,%rdx,8)                               #129.7
..LN613:
        fldl      -304(%rbp)                                    #130.7
        fldl      -296(%rbp)                                    #130.7
        fmulp     %st, %st(1)                                   #130.7
        movl      -60(%rbp), %eax                               #130.7
        movslq    %eax, %rax                                    #130.7
        shlq      $3, %rax                                      #130.7
        lea       (%rax,%rax,2), %rax                           #130.7
        addq      88(%rbp), %rax                                #130.7
        movl      -60(%rbp), %edx                               #130.7
        movslq    %edx, %rdx                                    #130.7
        shlq      $3, %rdx                                      #130.7
        negq      %rdx                                          #130.7
        addq      %rdx, %rax                                    #130.7
        movl      -52(%rbp), %edx                               #130.7
        movslq    %edx, %rdx                                    #130.7
        fstpl     -8(%rax,%rdx,8)                               #130.7
..LN615:
        fldl      -400(%rbp)                                    #131.7
        fldl      -312(%rbp)                                    #131.7
        fldl      -312(%rbp)                                    #131.7
..LN617:
        fmulp     %st, %st(1)                                   #131.29
        fldl      -304(%rbp)                                    #131.29
        fldl      -304(%rbp)                                    #131.29
..LN619:
        fmulp     %st, %st(1)                                   #131.41
..LN621:
        fsubrp    %st, %st(1)                                   #131.35
..LN623:
        fmulp     %st, %st(1)                                   #131.7
        movl      -60(%rbp), %eax                               #131.7
        movslq    %eax, %rax                                    #131.7
        shlq      $5, %rax                                      #131.7
        addq      88(%rbp), %rax                                #131.7
        movl      -60(%rbp), %edx                               #131.7
        movslq    %edx, %rdx                                    #131.7
        shlq      $3, %rdx                                      #131.7
        negq      %rdx                                          #131.7
        addq      %rdx, %rax                                    #131.7
        movl      -52(%rbp), %edx                               #131.7
        movslq    %edx, %rdx                                    #131.7
        fstpl     -8(%rax,%rdx,8)                               #131.7
..LN625:
        fldl      -96(%rbp)                                     #132.7
        fldl      -312(%rbp)                                    #132.7
..LN627:
        fmulp     %st, %st(1)                                   #132.21
        fldl      -304(%rbp)                                    #132.21
..LN629:
        fmulp     %st, %st(1)                                   #132.7
..LN631:
        movl      -60(%rbp), %eax                               #132.3
        movslq    %eax, %rax                                    #132.3
..LN633:
        shlq      $3, %rax                                      #132.7
        lea       (%rax,%rax,4), %rax                           #132.7
        addq      88(%rbp), %rax                                #132.7
..LN635:
        movl      -60(%rbp), %edx                               #132.3
        movslq    %edx, %rdx                                    #132.3
..LN637:
        shlq      $3, %rdx                                      #132.7
        negq      %rdx                                          #132.7
        addq      %rdx, %rax                                    #132.7
..LN639:
        movl      -52(%rbp), %edx                               #132.3
..LN641:
        movslq    %edx, %rdx                                    #132.7
        fstpl     -8(%rax,%rdx,8)                               #132.7
..LN643:
        addl      $1, -52(%rbp)                                 #120.10
        movl      -52(%rbp), %eax                               #120.10
        movl      -180(%rbp), %edx                              #120.10
        cmpl      %edx, %eax                                    #120.10
        jle       ..B1.38       # Prob 50%                      #120.10
                                # LOE
..B1.42:                        # Preds ..B1.36 ..B1.41
..LN645:
        movq      -128(%rbp), %rax                              #133.7
..LN647:
        movl      (%rax), %eax                                  #133.10
        movl      %eax, -176(%rbp)                              #133.10
        movl      $1, -44(%rbp)                                 #133.10
        movl      -176(%rbp), %eax                              #133.10
        testl     %eax, %eax                                    #133.10
        jle       ..B1.45       # Prob 50%                      #133.10
                                # LOE
..B1.44:                        # Preds ..B1.42 ..B1.44
..LN649:
        movq      -120(%rbp), %rax                              #134.7
        movl      (%rax), %eax                                  #134.7
        addl      -44(%rbp), %eax                               #134.7
        movl      %eax, -168(%rbp)                              #134.7
..LN651:
        movl      -60(%rbp), %eax                               #135.7
        movslq    %eax, %rax                                    #135.7
        movq      88(%rbp), %rdx                                #135.7
        lea       (%rdx,%rax,8), %rax                           #135.7
        movl      -60(%rbp), %edx                               #135.7
        movslq    %edx, %rdx                                    #135.7
        shlq      $3, %rdx                                      #135.7
        negq      %rdx                                          #135.7
        addq      %rdx, %rax                                    #135.7
        movl      -168(%rbp), %edx                              #135.7
        movslq    %edx, %rdx                                    #135.7
        fldl      -88(%rbp)                                     #135.7
        fstpl     -8(%rax,%rdx,8)                               #135.7
..LN653:
        movl      -44(%rbp), %eax                               #136.7
..LN655:
        movslq    %eax, %rax                                    #136.18
..LN657:
        movq      48(%rbp), %rdx                                #136.7
        movl      -60(%rbp), %ecx                               #136.7
        movslq    %ecx, %rcx                                    #136.7
        shlq      $4, %rcx                                      #136.7
        addq      88(%rbp), %rcx                                #136.7
        movl      -60(%rbp), %ebx                               #136.7
        movslq    %ebx, %rbx                                    #136.7
        shlq      $3, %rbx                                      #136.7
        negq      %rbx                                          #136.7
        addq      %rbx, %rcx                                    #136.7
        movl      -168(%rbp), %ebx                              #136.7
        movslq    %ebx, %rbx                                    #136.7
        fldl      -8(%rdx,%rax,8)                               #136.7
        fstpl     -8(%rcx,%rbx,8)                               #136.7
..LN659:
        movl      -44(%rbp), %eax                               #137.7
..LN661:
        movslq    %eax, %rax                                    #137.18
..LN663:
        movq      80(%rbp), %rdx                                #137.7
        movl      -60(%rbp), %ecx                               #137.7
        movslq    %ecx, %rcx                                    #137.7
        shlq      $3, %rcx                                      #137.7
        lea       (%rcx,%rcx,2), %rcx                           #137.7
        addq      88(%rbp), %rcx                                #137.7
        movl      -60(%rbp), %ebx                               #137.7
        movslq    %ebx, %rbx                                    #137.7
        shlq      $3, %rbx                                      #137.7
        negq      %rbx                                          #137.7
        addq      %rbx, %rcx                                    #137.7
        movl      -168(%rbp), %ebx                              #137.7
        movslq    %ebx, %rbx                                    #137.7
        fldl      -8(%rdx,%rax,8)                               #137.7
        fstpl     -8(%rcx,%rbx,8)                               #137.7
..LN665:
        movl      -60(%rbp), %eax                               #138.7
        movslq    %eax, %rax                                    #138.7
        shlq      $5, %rax                                      #138.7
        addq      88(%rbp), %rax                                #138.7
        movl      -60(%rbp), %edx                               #138.7
        movslq    %edx, %rdx                                    #138.7
        shlq      $3, %rdx                                      #138.7
        negq      %rdx                                          #138.7
        addq      %rdx, %rax                                    #138.7
        movl      -168(%rbp), %edx                              #138.7
        movslq    %edx, %rdx                                    #138.7
        fldl      -88(%rbp)                                     #138.7
        fstpl     -8(%rax,%rdx,8)                               #138.7
..LN667:
        movl      -60(%rbp), %eax                               #139.3
        movslq    %eax, %rax                                    #139.3
..LN669:
        shlq      $3, %rax                                      #139.7
        lea       (%rax,%rax,4), %rax                           #139.7
        addq      88(%rbp), %rax                                #139.7
..LN671:
        movl      -60(%rbp), %edx                               #139.3
        movslq    %edx, %rdx                                    #139.3
..LN673:
        shlq      $3, %rdx                                      #139.7
        negq      %rdx                                          #139.7
        addq      %rdx, %rax                                    #139.7
..LN675:
        movl      -168(%rbp), %edx                              #139.3
..LN677:
        movslq    %edx, %rdx                                    #139.7
        fldl      -88(%rbp)                                     #139.7
        fstpl     -8(%rax,%rdx,8)                               #139.7
..LN679:
        addl      $1, -44(%rbp)                                 #133.10
        movl      -44(%rbp), %eax                               #133.10
        movl      -176(%rbp), %edx                              #133.10
        cmpl      %edx, %eax                                    #133.10
        jle       ..B1.44       # Prob 50%                      #133.10
                                # LOE
..B1.45:                        # Preds ..B1.42 ..B1.44
..LN681:
        movl      $1, -40(%rbp)                                 #143.10
                                # LOE
..B1.46:                        # Preds ..B1.71 ..B1.45
..LN683:
        movq      -120(%rbp), %rax                              #144.7
        movl      (%rax), %eax                                  #144.7
        movl      %eax, -36(%rbp)                               #144.7
..LN685:
        movl      -40(%rbp), %eax                               #145.7
..LN687:
        cmpl      $2, %eax                                      #145.14
        je        ..B1.48       # Prob 50%                      #145.14
                                # LOE
..B1.47:                        # Preds ..B1.46
        movl      -40(%rbp), %eax                               #145.14
..LN689:
        cmpl      $3, %eax                                      #145.29
        jne       ..B1.49       # Prob 50%                      #145.29
                                # LOE
..B1.48:                        # Preds ..B1.46 ..B1.47
..LN691:
        movq      24(%rbp), %rax                                #145.37
        movl      (%rax), %eax                                  #145.37
        movl      %eax, -36(%rbp)                               #145.37
                                # LOE
..B1.49:                        # Preds ..B1.48 ..B1.47
..LN693:
        movq      -120(%rbp), %rax                              #146.7
..LN695:
        movl      (%rax), %eax                                  #146.10
        movl      %eax, -32(%rbp)                               #146.10
        movl      $1, -52(%rbp)                                 #146.10
        movl      -32(%rbp), %eax                               #146.10
        testl     %eax, %eax                                    #146.10
        jle       ..B1.52       # Prob 50%                      #146.10
                                # LOE
..B1.51:                        # Preds ..B1.49 ..B1.51
..LN697:
        movl      -60(%rbp), %eax                               #147.3
        movslq    %eax, %rax                                    #147.3
..LN699:
        shlq      $3, %rax                                      #147.7
..LN701:
        movl      -40(%rbp), %edx                               #147.3
..LN703:
        movslq    %edx, %rdx                                    #147.7
        imulq     %rax, %rdx                                    #147.7
        addq      96(%rbp), %rdx                                #147.7
..LN705:
        movl      -60(%rbp), %eax                               #147.3
        movslq    %eax, %rax                                    #147.3
..LN707:
        shlq      $3, %rax                                      #147.7
        negq      %rax                                          #147.7
        addq      %rax, %rdx                                    #147.7
..LN709:
        movl      -52(%rbp), %eax                               #147.3
..LN711:
        movslq    %eax, %rax                                    #147.7
        fldl      -88(%rbp)                                     #147.7
        fstpl     -8(%rdx,%rax,8)                               #147.7
..LN713:
        addl      $1, -52(%rbp)                                 #146.10
        movl      -52(%rbp), %eax                               #146.10
        movl      -32(%rbp), %edx                               #146.10
        cmpl      %edx, %eax                                    #146.10
        jle       ..B1.51       # Prob 50%                      #146.10
                                # LOE
..B1.52:                        # Preds ..B1.49 ..B1.51
..LN715:
        movl      -56(%rbp), %eax                               #148.10
        movl      %eax, -28(%rbp)                               #148.10
        movl      $1, -48(%rbp)                                 #148.10
        movl      -28(%rbp), %eax                               #148.10
        testl     %eax, %eax                                    #148.10
        jle       ..B1.63       # Prob 50%                      #148.10
                                # LOE
..B1.54:                        # Preds ..B1.52 ..B1.60
..LN717:
        fldl      -88(%rbp)                                     #149.7
        fstpl     -80(%rbp)                                     #149.7
..LN719:
        movq      -120(%rbp), %rax                              #150.7
..LN721:
        movl      (%rax), %eax                                  #150.10
        movl      %eax, -20(%rbp)                               #150.10
        movl      $1, -52(%rbp)                                 #150.10
        movl      -20(%rbp), %eax                               #150.10
        testl     %eax, %eax                                    #150.10
        jle       ..B1.57       # Prob 50%                      #150.10
                                # LOE
..B1.56:                        # Preds ..B1.54 ..B1.56
..LN723:
        movl      -64(%rbp), %eax                               #151.3
        movslq    %eax, %rax                                    #151.3
..LN725:
        shlq      $3, %rax                                      #151.15
..LN727:
        movl      -48(%rbp), %edx                               #151.3
..LN729:
        movslq    %edx, %rdx                                    #151.15
        imulq     %rax, %rdx                                    #151.15
..LN731:
        addq      -104(%rbp), %rdx                              #151.7
..LN733:
        movl      -64(%rbp), %eax                               #151.3
        movslq    %eax, %rax                                    #151.3
..LN735:
        shlq      $3, %rax                                      #151.15
        negq      %rax                                          #151.15
..LN737:
        addq      %rax, %rdx                                    #151.7
..LN739:
        movl      -52(%rbp), %eax                               #151.3
..LN741:
        movslq    %eax, %rax                                    #151.15
        fldl      -8(%rdx,%rax,8)                               #151.15
        movl      -60(%rbp), %eax                               #151.15
        movslq    %eax, %rax                                    #151.15
..LN743:
        shlq      $3, %rax                                      #151.25
..LN745:
        movl      -40(%rbp), %edx                               #151.15
..LN747:
        movslq    %edx, %rdx                                    #151.25
        imulq     %rax, %rdx                                    #151.25
..LN749:
        addq      88(%rbp), %rdx                                #151.7
..LN751:
        movl      -60(%rbp), %eax                               #151.15
        movslq    %eax, %rax                                    #151.15
..LN753:
        shlq      $3, %rax                                      #151.25
        negq      %rax                                          #151.25
..LN755:
        addq      %rax, %rdx                                    #151.7
..LN757:
        movl      -52(%rbp), %eax                               #151.15
..LN759:
        movslq    %eax, %rax                                    #151.25
        fldl      -8(%rdx,%rax,8)                               #151.25
..LN761:
        fmulp     %st, %st(1)                                   #151.24
..LN763:
        fldl      -80(%rbp)                                     #151.3
..LN765:
        faddp     %st, %st(1)                                   #151.7
        fstpl     -80(%rbp)                                     #151.7
..LN767:
        addl      $1, -52(%rbp)                                 #150.10
        movl      -52(%rbp), %eax                               #150.10
        movl      -20(%rbp), %edx                               #150.10
        cmpl      %edx, %eax                                    #150.10
        jle       ..B1.56       # Prob 50%                      #150.10
                                # LOE
..B1.57:                        # Preds ..B1.54 ..B1.56
..LN769:
        movq      16(%rbp), %rax                                #152.7
        movl      -48(%rbp), %edx                               #152.7
        movl      (%rax), %eax                                  #152.7
..LN771:
        cmpl      %eax, %edx                                    #152.13
        jge       ..B1.59       # Prob 50%                      #152.13
                                # LOE
..B1.58:                        # Preds ..B1.57
..LN773:
        fldl      -80(%rbp)                                     #152.23
        fchs                                                    #152.23
        fstpl     -80(%rbp)                                     #152.23
                                # LOE
..B1.59:                        # Preds ..B1.58 ..B1.57
..LN775:
        movq      -120(%rbp), %rax                              #153.7
..LN777:
        movl      (%rax), %eax                                  #153.10
        movl      %eax, -12(%rbp)                               #153.10
        movl      $1, -52(%rbp)                                 #153.10
        movl      -12(%rbp), %eax                               #153.10
        testl     %eax, %eax                                    #153.10
        jg        ..B1.62       # Prob 50%                      #153.10
                                # LOE
..B1.60:                        # Preds ..B1.59 ..B1.62
..LN779:
        addl      $1, -48(%rbp)                                 #148.10
        movl      -48(%rbp), %eax                               #148.10
        movl      -28(%rbp), %edx                               #148.10
        cmpl      %edx, %eax                                    #148.10
        jle       ..B1.54       # Prob 50%                      #148.10
        jmp       ..B1.63       # Prob 100%                     #148.10
                                # LOE
..B1.62:                        # Preds ..B1.59 ..B1.62
..LN781:
        movl      -60(%rbp), %eax                               #154.7
        movslq    %eax, %rax                                    #154.7
..LN783:
        shlq      $3, %rax                                      #154.18
..LN785:
        movl      -40(%rbp), %edx                               #154.7
..LN787:
        movslq    %edx, %rdx                                    #154.18
        imulq     %rax, %rdx                                    #154.18
..LN789:
        addq      96(%rbp), %rdx                                #154.7
..LN791:
        movl      -60(%rbp), %eax                               #154.7
        movslq    %eax, %rax                                    #154.7
..LN793:
        shlq      $3, %rax                                      #154.18
        negq      %rax                                          #154.18
..LN795:
        addq      %rax, %rdx                                    #154.7
        movl      -52(%rbp), %eax                               #154.7
..LN797:
        movslq    %eax, %rax                                    #154.18
        fldl      -80(%rbp)                                     #154.18
        movl      -64(%rbp), %ecx                               #154.18
        movslq    %ecx, %rcx                                    #154.18
..LN799:
        shlq      $3, %rcx                                      #154.33
..LN801:
        movl      -48(%rbp), %ebx                               #154.18
..LN803:
        movslq    %ebx, %rbx                                    #154.33
        imulq     %rcx, %rbx                                    #154.33
..LN805:
        addq      -104(%rbp), %rbx                              #154.7
..LN807:
        movl      -64(%rbp), %ecx                               #154.18
        movslq    %ecx, %rcx                                    #154.18
..LN809:
        shlq      $3, %rcx                                      #154.33
        negq      %rcx                                          #154.33
..LN811:
        addq      %rcx, %rbx                                    #154.7
..LN813:
        movl      -52(%rbp), %ecx                               #154.18
..LN815:
        movslq    %ecx, %rcx                                    #154.33
        fldl      -8(%rbx,%rcx,8)                               #154.33
..LN817:
        fmulp     %st, %st(1)                                   #154.32
..LN819:
        fldl      -8(%rdx,%rax,8)                               #154.18
..LN821:
        faddp     %st, %st(1)                                   #154.7
..LN823:
        movl      -60(%rbp), %eax                               #154.3
        movslq    %eax, %rax                                    #154.3
..LN825:
        shlq      $3, %rax                                      #154.7
..LN827:
        movl      -40(%rbp), %edx                               #154.3
..LN829:
        movslq    %edx, %rdx                                    #154.7
        imulq     %rax, %rdx                                    #154.7
        addq      96(%rbp), %rdx                                #154.7
..LN831:
        movl      -60(%rbp), %eax                               #154.3
        movslq    %eax, %rax                                    #154.3
..LN833:
        shlq      $3, %rax                                      #154.7
        negq      %rax                                          #154.7
        addq      %rax, %rdx                                    #154.7
..LN835:
        movl      -52(%rbp), %eax                               #154.3
..LN837:
        movslq    %eax, %rax                                    #154.7
        fstpl     -8(%rdx,%rax,8)                               #154.7
..LN839:
        addl      $1, -52(%rbp)                                 #153.10
        movl      -52(%rbp), %eax                               #153.10
        movl      -12(%rbp), %edx                               #153.10
        cmpl      %edx, %eax                                    #153.10
        jle       ..B1.62       # Prob 50%                      #153.10
        jmp       ..B1.60       # Prob 100%                     #153.10
                                # LOE
..B1.63:                        # Preds ..B1.52 ..B1.60
..LN841:
        movq      24(%rbp), %rax                                #155.7
        movl      -36(%rbp), %edx                               #155.7
        movl      (%rax), %eax                                  #155.7
..LN843:
        cmpl      %eax, %edx                                    #155.14
        jne       ..B1.70       # Prob 50%                      #155.14
                                # LOE
..B1.64:                        # Preds ..B1.63
..LN845:
        movq      -120(%rbp), %rax                              #156.11
..LN847:
        movl      (%rax), %eax                                  #156.14
        movl      %eax, -164(%rbp)                              #156.14
        movl      $1, -52(%rbp)                                 #156.14
        movl      -164(%rbp), %eax                              #156.14
        testl     %eax, %eax                                    #156.14
        jle       ..B1.70       # Prob 50%                      #156.14
                                # LOE
..B1.66:                        # Preds ..B1.64 ..B1.69
..LN849:
        fldl      -88(%rbp)                                     #157.11
        fstpl     -80(%rbp)                                     #157.11
..LN851:
        movq      -128(%rbp), %rax                              #158.11
..LN853:
        movl      (%rax), %eax                                  #158.14
        movl      %eax, -160(%rbp)                              #158.14
        movl      $1, -48(%rbp)                                 #158.14
        movl      -160(%rbp), %eax                              #158.14
        testl     %eax, %eax                                    #158.14
        jle       ..B1.69       # Prob 50%                      #158.14
                                # LOE
..B1.68:                        # Preds ..B1.66 ..B1.68
..LN855:
        movl      -60(%rbp), %eax                               #159.3
        movslq    %eax, %rax                                    #159.3
..LN857:
        shlq      $3, %rax                                      #159.19
..LN859:
        movl      -48(%rbp), %edx                               #159.3
..LN861:
        movslq    %edx, %rdx                                    #159.19
        imulq     %rax, %rdx                                    #159.19
..LN863:
        addq      -112(%rbp), %rdx                              #159.11
..LN865:
        movl      -60(%rbp), %eax                               #159.3
        movslq    %eax, %rax                                    #159.3
..LN867:
        shlq      $3, %rax                                      #159.19
        negq      %rax                                          #159.19
..LN869:
        addq      %rax, %rdx                                    #159.11
..LN871:
        movl      -52(%rbp), %eax                               #159.3
..LN873:
        movslq    %eax, %rax                                    #159.19
        fldl      -8(%rdx,%rax,8)                               #159.19
        movl      -60(%rbp), %eax                               #159.19
        movslq    %eax, %rax                                    #159.19
..LN875:
        shlq      $3, %rax                                      #159.29
..LN877:
        movl      -40(%rbp), %edx                               #159.19
..LN879:
        movslq    %edx, %rdx                                    #159.29
        imulq     %rax, %rdx                                    #159.29
..LN881:
        addq      88(%rbp), %rdx                                #159.11
..LN883:
        movl      -60(%rbp), %eax                               #159.19
        movslq    %eax, %rax                                    #159.19
..LN885:
        shlq      $3, %rax                                      #159.29
        negq      %rax                                          #159.29
..LN887:
        addq      %rax, %rdx                                    #159.11
..LN889:
        movq      -120(%rbp), %rax                              #159.19
        movl      -48(%rbp), %ecx                               #159.19
        addl      (%rax), %ecx                                  #159.19
..LN891:
        movslq    %ecx, %rax                                    #159.29
        fldl      -8(%rdx,%rax,8)                               #159.29
..LN893:
        fmulp     %st, %st(1)                                   #159.28
..LN895:
        fldl      -80(%rbp)                                     #159.3
..LN897:
        faddp     %st, %st(1)                                   #159.11
        fstpl     -80(%rbp)                                     #159.11
..LN899:
        addl      $1, -48(%rbp)                                 #158.14
        movl      -48(%rbp), %eax                               #158.14
        movl      -160(%rbp), %edx                              #158.14
        cmpl      %edx, %eax                                    #158.14
        jle       ..B1.68       # Prob 50%                      #158.14
                                # LOE
..B1.69:                        # Preds ..B1.66 ..B1.68
..LN901:
        movl      -60(%rbp), %eax                               #160.11
        movslq    %eax, %rax                                    #160.11
..LN903:
        shlq      $3, %rax                                      #160.22
..LN905:
        movl      -40(%rbp), %edx                               #160.11
..LN907:
        movslq    %edx, %rdx                                    #160.22
        imulq     %rax, %rdx                                    #160.22
..LN909:
        addq      96(%rbp), %rdx                                #160.11
..LN911:
        movl      -60(%rbp), %eax                               #160.11
        movslq    %eax, %rax                                    #160.11
..LN913:
        shlq      $3, %rax                                      #160.22
        negq      %rax                                          #160.22
..LN915:
        addq      %rax, %rdx                                    #160.11
        movl      -52(%rbp), %eax                               #160.11
..LN917:
        movslq    %eax, %rax                                    #160.22
        fldl      -8(%rdx,%rax,8)                               #160.22
        fldl      -80(%rbp)                                     #160.22
..LN919:
        faddp     %st, %st(1)                                   #160.11
..LN921:
        movl      -60(%rbp), %eax                               #160.3
        movslq    %eax, %rax                                    #160.3
..LN923:
        shlq      $3, %rax                                      #160.11
..LN925:
        movl      -40(%rbp), %edx                               #160.3
..LN927:
        movslq    %edx, %rdx                                    #160.11
        imulq     %rax, %rdx                                    #160.11
        addq      96(%rbp), %rdx                                #160.11
..LN929:
        movl      -60(%rbp), %eax                               #160.3
        movslq    %eax, %rax                                    #160.3
..LN931:
        shlq      $3, %rax                                      #160.11
        negq      %rax                                          #160.11
        addq      %rax, %rdx                                    #160.11
..LN933:
        movl      -52(%rbp), %eax                               #160.3
..LN935:
        movslq    %eax, %rax                                    #160.11
        fstpl     -8(%rdx,%rax,8)                               #160.11
..LN937:
        addl      $1, -52(%rbp)                                 #156.14
        movl      -52(%rbp), %eax                               #156.14
        movl      -164(%rbp), %edx                              #156.14
        cmpl      %edx, %eax                                    #156.14
        jle       ..B1.66       # Prob 50%                      #156.14
                                # LOE
..B1.70:                        # Preds ..B1.64 ..B1.69 ..B1.63
..LN939:
        movq      -128(%rbp), %rax                              #162.7
..LN941:
        movl      (%rax), %eax                                  #162.10
        movl      %eax, -24(%rbp)                               #162.10
        movl      $1, -48(%rbp)                                 #162.10
        movl      -24(%rbp), %eax                               #162.10
        testl     %eax, %eax                                    #162.10
        jg        ..B1.73       # Prob 50%                      #162.10
                                # LOE
..B1.71:                        # Preds ..B1.70 ..B1.76
..LN943:
        addl      $1, -40(%rbp)                                 #143.10
        movl      -40(%rbp), %eax                               #143.10
        cmpl      $5, %eax                                      #143.10
        jle       ..B1.46       # Prob 50%                      #143.10
        jmp       ..B1.77       # Prob 100%                     #143.10
                                # LOE
..B1.73:                        # Preds ..B1.70 ..B1.76
..LN945:
        fldl      -88(%rbp)                                     #163.7
        fstpl     -80(%rbp)                                     #163.7
..LN947:
        movl      -36(%rbp), %eax                               #164.10
        movl      %eax, -16(%rbp)                               #164.10
        movl      $1, -44(%rbp)                                 #164.10
        movl      -16(%rbp), %eax                               #164.10
        testl     %eax, %eax                                    #164.10
        jle       ..B1.76       # Prob 50%                      #164.10
                                # LOE
..B1.75:                        # Preds ..B1.73 ..B1.75
..LN949:
        movl      -60(%rbp), %eax                               #165.3
        movslq    %eax, %rax                                    #165.3
..LN951:
        shlq      $3, %rax                                      #165.15
..LN953:
        movl      -48(%rbp), %edx                               #165.3
..LN955:
        movslq    %edx, %rdx                                    #165.15
        imulq     %rax, %rdx                                    #165.15
..LN957:
        addq      -112(%rbp), %rdx                              #165.7
..LN959:
        movl      -60(%rbp), %eax                               #165.3
        movslq    %eax, %rax                                    #165.3
..LN961:
        shlq      $3, %rax                                      #165.15
        negq      %rax                                          #165.15
..LN963:
        addq      %rax, %rdx                                    #165.7
..LN965:
        movl      -44(%rbp), %eax                               #165.3
..LN967:
        movslq    %eax, %rax                                    #165.15
        fldl      -8(%rdx,%rax,8)                               #165.15
        movl      -60(%rbp), %eax                               #165.15
        movslq    %eax, %rax                                    #165.15
..LN969:
        shlq      $3, %rax                                      #165.25
..LN971:
        movl      -40(%rbp), %edx                               #165.15
..LN973:
        movslq    %edx, %rdx                                    #165.25
        imulq     %rax, %rdx                                    #165.25
..LN975:
        addq      88(%rbp), %rdx                                #165.7
..LN977:
        movl      -60(%rbp), %eax                               #165.15
        movslq    %eax, %rax                                    #165.15
..LN979:
        shlq      $3, %rax                                      #165.25
        negq      %rax                                          #165.25
..LN981:
        addq      %rax, %rdx                                    #165.7
..LN983:
        movl      -44(%rbp), %eax                               #165.15
..LN985:
        movslq    %eax, %rax                                    #165.25
        fldl      -8(%rdx,%rax,8)                               #165.25
..LN987:
        fmulp     %st, %st(1)                                   #165.24
..LN989:
        fldl      -80(%rbp)                                     #165.3
..LN991:
        faddp     %st, %st(1)                                   #165.7
        fstpl     -80(%rbp)                                     #165.7
..LN993:
        addl      $1, -44(%rbp)                                 #164.10
        movl      -44(%rbp), %eax                               #164.10
        movl      -16(%rbp), %edx                               #164.10
        cmpl      %edx, %eax                                    #164.10
        jle       ..B1.75       # Prob 50%                      #164.10
                                # LOE
..B1.76:                        # Preds ..B1.73 ..B1.75
..LN995:
        movl      -60(%rbp), %eax                               #166.3
        movslq    %eax, %rax                                    #166.3
..LN997:
        shlq      $3, %rax                                      #166.7
..LN999:
        movl      -40(%rbp), %edx                               #166.3
..LN1001:
        movslq    %edx, %rdx                                    #166.7
        imulq     %rax, %rdx                                    #166.7
        addq      96(%rbp), %rdx                                #166.7
..LN1003:
        movl      -60(%rbp), %eax                               #166.3
        movslq    %eax, %rax                                    #166.3
..LN1005:
        shlq      $3, %rax                                      #166.7
        negq      %rax                                          #166.7
        addq      %rax, %rdx                                    #166.7
..LN1007:
        movq      -120(%rbp), %rax                              #166.3
        movl      -48(%rbp), %ecx                               #166.3
        addl      (%rax), %ecx                                  #166.3
..LN1009:
        movslq    %ecx, %rax                                    #166.7
        fldl      -80(%rbp)                                     #166.7
        fstpl     -8(%rdx,%rax,8)                               #166.7
..LN1011:
        addl      $1, -48(%rbp)                                 #162.10
        movl      -48(%rbp), %eax                               #162.10
        movl      -24(%rbp), %edx                               #162.10
        cmpl      %edx, %eax                                    #162.10
        jle       ..B1.73       # Prob 50%                      #162.10
        jmp       ..B1.71       # Prob 100%                     #162.10
                                # LOE
..B1.77:                        # Preds ..B1.71
..LN1013:
        movq      24(%rbp), %rax                                #170.7
..LN1015:
        movl      (%rax), %eax                                  #170.10
        movl      %eax, -156(%rbp)                              #170.10
        movl      $1, -52(%rbp)                                 #170.10
        movl      -156(%rbp), %eax                              #170.10
        testl     %eax, %eax                                    #170.10
        jle       ..B1.82       # Prob 50%                      #170.10
                                # LOE
..B1.79:                        # Preds ..B1.77 ..B1.146
..LN1017:
        fldl      -88(%rbp)                                     #171.7
        fstpl     -80(%rbp)                                     #171.7
..LN1019:
        movl      $1, -44(%rbp)                                 #172.10
                                # LOE
..B1.80:                        # Preds ..B1.80 ..B1.79
..LN1021:
        fldl      -96(%rbp)                                     #173.7
        movl      -60(%rbp), %eax                               #173.7
        movslq    %eax, %rax                                    #173.7
..LN1023:
        shlq      $3, %rax                                      #173.19
..LN1025:
        movl      -44(%rbp), %edx                               #173.7
..LN1027:
        movslq    %edx, %rdx                                    #173.19
        imulq     %rax, %rdx                                    #173.19
..LN1029:
        addq      96(%rbp), %rdx                                #173.7
        movl      -60(%rbp), %eax                               #173.7
..LN1031:
        movslq    %eax, %rax                                    #173.7
..LN1033:
        shlq      $3, %rax                                      #173.19
        negq      %rax                                          #173.19
..LN1035:
        addq      %rax, %rdx                                    #173.7
        movl      -52(%rbp), %eax                               #173.7
..LN1037:
        movslq    %eax, %rax                                    #173.19
        fldl      -8(%rdx,%rax,8)                               #173.19
..LN1039:
        fmulp     %st, %st(1)                                   #173.18
        movl      -60(%rbp), %eax                               #173.18
        movslq    %eax, %rax                                    #173.18
..LN1041:
        shlq      $3, %rax                                      #173.29
..LN1043:
        movl      -44(%rbp), %edx                               #173.18
..LN1045:
        movslq    %edx, %rdx                                    #173.29
        imulq     %rax, %rdx                                    #173.29
..LN1047:
        addq      88(%rbp), %rdx                                #173.7
..LN1049:
        movl      -60(%rbp), %eax                               #173.18
        movslq    %eax, %rax                                    #173.18
..LN1051:
        shlq      $3, %rax                                      #173.29
        negq      %rax                                          #173.29
..LN1053:
        addq      %rax, %rdx                                    #173.7
..LN1055:
        movl      -52(%rbp), %eax                               #173.18
..LN1057:
        movslq    %eax, %rax                                    #173.29
        fldl      -8(%rdx,%rax,8)                               #173.29
..LN1059:
        fmulp     %st, %st(1)                                   #173.7
        movl      -44(%rbp), %eax                               #173.7
        movslq    %eax, %rax                                    #173.7
        fstpl     -784(%rbp,%rax,8)                             #173.7
..LN1061:
        movl      -44(%rbp), %eax                               #174.3
        movslq    %eax, %rax                                    #174.3
        fldl      -80(%rbp)                                     #174.3
..LN1063:
        fldl      -784(%rbp,%rax,8)                             #174.15
..LN1065:
        faddp     %st, %st(1)                                   #174.7
        fstpl     -80(%rbp)                                     #174.7
..LN1067:
        addl      $1, -44(%rbp)                                 #172.10
        movl      -44(%rbp), %eax                               #172.10
        cmpl      $5, %eax                                      #172.10
        jle       ..B1.80       # Prob 50%                      #172.10
                                # LOE
..B1.81:                        # Preds ..B1.80
..LN1069:
        fldl      -848(%rbp)                                    #175.14
..LN1071:
        fldl      -776(%rbp)                                    #175.21
..LN1073:
        fsubrp    %st, %st(1)                                   #175.20
        fldl      -80(%rbp)                                     #175.20
..LN1075:
        fsubrp    %st, %st(1)                                   #175.7
        fstpl     -848(%rbp)                                    #175.7
..LN1077:
        movl      -60(%rbp), %eax                               #176.7
        movslq    %eax, %rax                                    #176.7
        movq      96(%rbp), %rdx                                #176.7
        lea       (%rdx,%rax,8), %rax                           #176.7
        movl      -60(%rbp), %edx                               #176.7
        movslq    %edx, %rdx                                    #176.7
..LN1079:
        shlq      $3, %rdx                                      #176.13
        negq      %rdx                                          #176.13
..LN1081:
        addq      %rdx, %rax                                    #176.7
        movl      -52(%rbp), %edx                               #176.7
..LN1083:
        movslq    %edx, %rdx                                    #176.13
        fldl      -8(%rax,%rdx,8)                               #176.13
        movl      -60(%rbp), %eax                               #176.13
        movslq    %eax, %rax                                    #176.13
..LN1085:
        shlq      $4, %rax                                      #176.23
..LN1087:
        addq      88(%rbp), %rax                                #176.7
..LN1089:
        movl      -60(%rbp), %edx                               #176.13
        movslq    %edx, %rdx                                    #176.13
..LN1091:
        shlq      $3, %rdx                                      #176.23
        negq      %rdx                                          #176.23
..LN1093:
        addq      %rdx, %rax                                    #176.7
..LN1095:
        movl      -52(%rbp), %edx                               #176.13
..LN1097:
        movslq    %edx, %rdx                                    #176.23
        fldl      -8(%rax,%rdx,8)                               #176.23
..LN1099:
        fmulp     %st, %st(1)                                   #176.22
        movl      -60(%rbp), %eax                               #176.22
        movslq    %eax, %rax                                    #176.22
..LN1101:
        shlq      $4, %rax                                      #176.33
..LN1103:
        addq      96(%rbp), %rax                                #176.7
..LN1105:
        movl      -60(%rbp), %edx                               #176.22
        movslq    %edx, %rdx                                    #176.22
..LN1107:
        shlq      $3, %rdx                                      #176.33
        negq      %rdx                                          #176.33
..LN1109:
        addq      %rdx, %rax                                    #176.7
..LN1111:
        movl      -52(%rbp), %edx                               #176.22
..LN1113:
        movslq    %edx, %rdx                                    #176.33
        fldl      -8(%rax,%rdx,8)                               #176.33
        movl      -60(%rbp), %eax                               #176.33
        movslq    %eax, %rax                                    #176.33
        movq      88(%rbp), %rdx                                #176.33
..LN1115:
        lea       (%rdx,%rax,8), %rax                           #176.7
..LN1117:
        movl      -60(%rbp), %edx                               #176.33
        movslq    %edx, %rdx                                    #176.33
..LN1119:
        shlq      $3, %rdx                                      #176.43
        negq      %rdx                                          #176.43
..LN1121:
        addq      %rdx, %rax                                    #176.7
..LN1123:
        movl      -52(%rbp), %edx                               #176.33
..LN1125:
        movslq    %edx, %rdx                                    #176.43
        fldl      -8(%rax,%rdx,8)                               #176.43
..LN1127:
        fmulp     %st, %st(1)                                   #176.42
..LN1129:
        faddp     %st, %st(1)                                   #176.7
        fstpl     -312(%rbp)                                    #176.7
..LN1131:
        movl      -60(%rbp), %eax                               #177.7
        movslq    %eax, %rax                                    #177.7
..LN1133:
        shlq      $4, %rax                                      #177.13
..LN1135:
        addq      96(%rbp), %rax                                #177.7
        movl      -60(%rbp), %edx                               #177.7
        movslq    %edx, %rdx                                    #177.7
..LN1137:
        shlq      $3, %rdx                                      #177.13
        negq      %rdx                                          #177.13
..LN1139:
        addq      %rdx, %rax                                    #177.7
        movl      -52(%rbp), %edx                               #177.7
..LN1141:
        movslq    %edx, %rdx                                    #177.13
        fldl      -8(%rax,%rdx,8)                               #177.13
        movl      -60(%rbp), %eax                               #177.13
        movslq    %eax, %rax                                    #177.13
..LN1143:
        shlq      $5, %rax                                      #177.23
..LN1145:
        addq      88(%rbp), %rax                                #177.7
..LN1147:
        movl      -60(%rbp), %edx                               #177.13
        movslq    %edx, %rdx                                    #177.13
..LN1149:
        shlq      $3, %rdx                                      #177.23
        negq      %rdx                                          #177.23
..LN1151:
        addq      %rdx, %rax                                    #177.7
..LN1153:
        movl      -52(%rbp), %edx                               #177.13
..LN1155:
        movslq    %edx, %rdx                                    #177.23
        fldl      -8(%rax,%rdx,8)                               #177.23
..LN1157:
        fmulp     %st, %st(1)                                   #177.22
        movl      -60(%rbp), %eax                               #177.22
        movslq    %eax, %rax                                    #177.22
..LN1159:
        shlq      $5, %rax                                      #177.33
..LN1161:
        addq      96(%rbp), %rax                                #177.7
..LN1163:
        movl      -60(%rbp), %edx                               #177.22
        movslq    %edx, %rdx                                    #177.22
..LN1165:
        shlq      $3, %rdx                                      #177.33
        negq      %rdx                                          #177.33
..LN1167:
        addq      %rdx, %rax                                    #177.7
..LN1169:
        movl      -52(%rbp), %edx                               #177.22
..LN1171:
        movslq    %edx, %rdx                                    #177.33
        fldl      -8(%rax,%rdx,8)                               #177.33
        movl      -60(%rbp), %eax                               #177.33
        movslq    %eax, %rax                                    #177.33
..LN1173:
        shlq      $4, %rax                                      #177.43
..LN1175:
        addq      88(%rbp), %rax                                #177.7
..LN1177:
        movl      -60(%rbp), %edx                               #177.33
        movslq    %edx, %rdx                                    #177.33
..LN1179:
        shlq      $3, %rdx                                      #177.43
        negq      %rdx                                          #177.43
..LN1181:
        addq      %rdx, %rax                                    #177.7
..LN1183:
        movl      -52(%rbp), %edx                               #177.33
..LN1185:
        movslq    %edx, %rdx                                    #177.43
        fldl      -8(%rax,%rdx,8)                               #177.43
..LN1187:
        fmulp     %st, %st(1)                                   #177.42
..LN1189:
        faddp     %st, %st(1)                                   #177.7
        fstpl     -304(%rbp)                                    #177.7
..LN1191:
        movl      -60(%rbp), %eax                               #178.7
        movslq    %eax, %rax                                    #178.7
..LN1193:
        shlq      $3, %rax                                      #178.13
        lea       (%rax,%rax,2), %rax                           #178.13
..LN1195:
        addq      96(%rbp), %rax                                #178.7
        movl      -60(%rbp), %edx                               #178.7
        movslq    %edx, %rdx                                    #178.7
..LN1197:
        shlq      $3, %rdx                                      #178.13
        negq      %rdx                                          #178.13
..LN1199:
        addq      %rdx, %rax                                    #178.7
        movl      -52(%rbp), %edx                               #178.7
..LN1201:
        movslq    %edx, %rdx                                    #178.13
        fldl      -8(%rax,%rdx,8)                               #178.13
        movl      -60(%rbp), %eax                               #178.13
        movslq    %eax, %rax                                    #178.13
..LN1203:
        shlq      $3, %rax                                      #178.23
        lea       (%rax,%rax,4), %rax                           #178.23
..LN1205:
        addq      88(%rbp), %rax                                #178.7
..LN1207:
        movl      -60(%rbp), %edx                               #178.13
        movslq    %edx, %rdx                                    #178.13
..LN1209:
        shlq      $3, %rdx                                      #178.23
        negq      %rdx                                          #178.23
..LN1211:
        addq      %rdx, %rax                                    #178.7
..LN1213:
        movl      -52(%rbp), %edx                               #178.13
..LN1215:
        movslq    %edx, %rdx                                    #178.23
        fldl      -8(%rax,%rdx,8)                               #178.23
..LN1217:
        fmulp     %st, %st(1)                                   #178.22
        movl      -60(%rbp), %eax                               #178.22
        movslq    %eax, %rax                                    #178.22
..LN1219:
        shlq      $3, %rax                                      #178.33
        lea       (%rax,%rax,4), %rax                           #178.33
..LN1221:
        addq      96(%rbp), %rax                                #178.7
..LN1223:
        movl      -60(%rbp), %edx                               #178.22
        movslq    %edx, %rdx                                    #178.22
..LN1225:
        shlq      $3, %rdx                                      #178.33
        negq      %rdx                                          #178.33
..LN1227:
        addq      %rdx, %rax                                    #178.7
..LN1229:
        movl      -52(%rbp), %edx                               #178.22
..LN1231:
        movslq    %edx, %rdx                                    #178.33
        fldl      -8(%rax,%rdx,8)                               #178.33
        movl      -60(%rbp), %eax                               #178.33
        movslq    %eax, %rax                                    #178.33
..LN1233:
        shlq      $3, %rax                                      #178.43
        lea       (%rax,%rax,2), %rax                           #178.43
..LN1235:
        addq      88(%rbp), %rax                                #178.7
..LN1237:
        movl      -60(%rbp), %edx                               #178.33
        movslq    %edx, %rdx                                    #178.33
..LN1239:
        shlq      $3, %rdx                                      #178.43
        negq      %rdx                                          #178.43
..LN1241:
        addq      %rdx, %rax                                    #178.7
..LN1243:
        movl      -52(%rbp), %edx                               #178.33
..LN1245:
        movslq    %edx, %rdx                                    #178.43
        fldl      -8(%rax,%rdx,8)                               #178.43
..LN1247:
        fmulp     %st, %st(1)                                   #178.42
..LN1249:
        faddp     %st, %st(1)                                   #178.7
        fstpl     -296(%rbp)                                    #178.7
..LN1251:
        fldl      -840(%rbp)                                    #179.14
        fldl      -312(%rbp)                                    #179.14
..LN1253:
        fsubrp    %st, %st(1)                                   #179.20
        fldl      -96(%rbp)                                     #179.20
        fldl      -304(%rbp)                                    #179.20
        fldl      -296(%rbp)                                    #179.20
..LN1255:
        faddp     %st, %st(1)                                   #179.38
..LN1257:
        fmulp     %st, %st(1)                                   #179.31
..LN1259:
        fsubrp    %st, %st(1)                                   #179.7
        fstpl     -840(%rbp)                                    #179.7
..LN1261:
        fldl      -96(%rbp)                                     #180.14
        fldl      -304(%rbp)                                    #180.14
        fldl      -296(%rbp)                                    #180.14
..LN1263:
        fsubrp    %st, %st(1)                                   #180.32
..LN1265:
        fmulp     %st, %st(1)                                   #180.25
..LN1267:
        fldl      -808(%rbp)                                    #180.14
..LN1269:
        fsubp     %st, %st(1)                                   #180.7
        fstpl     -808(%rbp)                                    #180.7
..LN1271:
        movl      -60(%rbp), %eax                               #181.7
        movslq    %eax, %rax                                    #181.7
        movq      96(%rbp), %rdx                                #181.7
        lea       (%rdx,%rax,8), %rax                           #181.7
        movl      -60(%rbp), %edx                               #181.7
        movslq    %edx, %rdx                                    #181.7
..LN1273:
        shlq      $3, %rdx                                      #181.13
        negq      %rdx                                          #181.13
..LN1275:
        addq      %rdx, %rax                                    #181.7
        movl      -52(%rbp), %edx                               #181.7
..LN1277:
        movslq    %edx, %rdx                                    #181.13
        fldl      -8(%rax,%rdx,8)                               #181.13
        movl      -60(%rbp), %eax                               #181.13
        movslq    %eax, %rax                                    #181.13
..LN1279:
        shlq      $3, %rax                                      #181.23
        lea       (%rax,%rax,2), %rax                           #181.23
..LN1281:
        addq      88(%rbp), %rax                                #181.7
..LN1283:
        movl      -60(%rbp), %edx                               #181.13
        movslq    %edx, %rdx                                    #181.13
..LN1285:
        shlq      $3, %rdx                                      #181.23
        negq      %rdx                                          #181.23
..LN1287:
        addq      %rdx, %rax                                    #181.7
..LN1289:
        movl      -52(%rbp), %edx                               #181.13
..LN1291:
        movslq    %edx, %rdx                                    #181.23
        fldl      -8(%rax,%rdx,8)                               #181.23
..LN1293:
        fmulp     %st, %st(1)                                   #181.22
        movl      -60(%rbp), %eax                               #181.22
        movslq    %eax, %rax                                    #181.22
..LN1295:
        shlq      $3, %rax                                      #181.33
        lea       (%rax,%rax,2), %rax                           #181.33
..LN1297:
        addq      96(%rbp), %rax                                #181.7
..LN1299:
        movl      -60(%rbp), %edx                               #181.22
        movslq    %edx, %rdx                                    #181.22
..LN1301:
        shlq      $3, %rdx                                      #181.33
        negq      %rdx                                          #181.33
..LN1303:
        addq      %rdx, %rax                                    #181.7
..LN1305:
        movl      -52(%rbp), %edx                               #181.22
..LN1307:
        movslq    %edx, %rdx                                    #181.33
        fldl      -8(%rax,%rdx,8)                               #181.33
        movl      -60(%rbp), %eax                               #181.33
        movslq    %eax, %rax                                    #181.33
        movq      88(%rbp), %rdx                                #181.33
..LN1309:
        lea       (%rdx,%rax,8), %rax                           #181.7
..LN1311:
        movl      -60(%rbp), %edx                               #181.33
        movslq    %edx, %rdx                                    #181.33
..LN1313:
        shlq      $3, %rdx                                      #181.43
        negq      %rdx                                          #181.43
..LN1315:
        addq      %rdx, %rax                                    #181.7
..LN1317:
        movl      -52(%rbp), %edx                               #181.33
..LN1319:
        movslq    %edx, %rdx                                    #181.43
        fldl      -8(%rax,%rdx,8)                               #181.43
..LN1321:
        fmulp     %st, %st(1)                                   #181.42
..LN1323:
        faddp     %st, %st(1)                                   #181.7
        fstpl     -312(%rbp)                                    #181.7
..LN1325:
        movl      -60(%rbp), %eax                               #182.7
        movslq    %eax, %rax                                    #182.7
..LN1327:
        shlq      $4, %rax                                      #182.13
..LN1329:
        addq      96(%rbp), %rax                                #182.7
        movl      -60(%rbp), %edx                               #182.7
        movslq    %edx, %rdx                                    #182.7
..LN1331:
        shlq      $3, %rdx                                      #182.13
        negq      %rdx                                          #182.13
..LN1333:
        addq      %rdx, %rax                                    #182.7
        movl      -52(%rbp), %edx                               #182.7
..LN1335:
        movslq    %edx, %rdx                                    #182.13
        fldl      -8(%rax,%rdx,8)                               #182.13
        movl      -60(%rbp), %eax                               #182.13
        movslq    %eax, %rax                                    #182.13
..LN1337:
        shlq      $3, %rax                                      #182.23
        lea       (%rax,%rax,4), %rax                           #182.23
..LN1339:
        addq      88(%rbp), %rax                                #182.7
..LN1341:
        movl      -60(%rbp), %edx                               #182.13
        movslq    %edx, %rdx                                    #182.13
..LN1343:
        shlq      $3, %rdx                                      #182.23
        negq      %rdx                                          #182.23
..LN1345:
        addq      %rdx, %rax                                    #182.7
..LN1347:
        movl      -52(%rbp), %edx                               #182.13
..LN1349:
        movslq    %edx, %rdx                                    #182.23
        fldl      -8(%rax,%rdx,8)                               #182.23
..LN1351:
        fmulp     %st, %st(1)                                   #182.22
        movl      -60(%rbp), %eax                               #182.22
        movslq    %eax, %rax                                    #182.22
..LN1353:
        shlq      $3, %rax                                      #182.33
        lea       (%rax,%rax,4), %rax                           #182.33
..LN1355:
        addq      96(%rbp), %rax                                #182.7
..LN1357:
        movl      -60(%rbp), %edx                               #182.22
        movslq    %edx, %rdx                                    #182.22
..LN1359:
        shlq      $3, %rdx                                      #182.33
        negq      %rdx                                          #182.33
..LN1361:
        addq      %rdx, %rax                                    #182.7
..LN1363:
        movl      -52(%rbp), %edx                               #182.22
..LN1365:
        movslq    %edx, %rdx                                    #182.33
        fldl      -8(%rax,%rdx,8)                               #182.33
        movl      -60(%rbp), %eax                               #182.33
        movslq    %eax, %rax                                    #182.33
..LN1367:
        shlq      $4, %rax                                      #182.43
..LN1369:
        addq      88(%rbp), %rax                                #182.7
..LN1371:
        movl      -60(%rbp), %edx                               #182.33
        movslq    %edx, %rdx                                    #182.33
..LN1373:
        shlq      $3, %rdx                                      #182.43
        negq      %rdx                                          #182.43
..LN1375:
        addq      %rdx, %rax                                    #182.7
..LN1377:
        movl      -52(%rbp), %edx                               #182.33
..LN1379:
        movslq    %edx, %rdx                                    #182.43
        fldl      -8(%rax,%rdx,8)                               #182.43
..LN1381:
        fmulp     %st, %st(1)                                   #182.42
..LN1383:
        faddp     %st, %st(1)                                   #182.7
        fstpl     -304(%rbp)                                    #182.7
..LN1385:
        movl      -60(%rbp), %eax                               #183.7
        movslq    %eax, %rax                                    #183.7
..LN1387:
        shlq      $3, %rax                                      #183.13
        lea       (%rax,%rax,2), %rax                           #183.13
..LN1389:
        addq      96(%rbp), %rax                                #183.7
        movl      -60(%rbp), %edx                               #183.7
        movslq    %edx, %rdx                                    #183.7
..LN1391:
        shlq      $3, %rdx                                      #183.13
        negq      %rdx                                          #183.13
..LN1393:
        addq      %rdx, %rax                                    #183.7
        movl      -52(%rbp), %edx                               #183.7
..LN1395:
        movslq    %edx, %rdx                                    #183.13
        fldl      -8(%rax,%rdx,8)                               #183.13
        movl      -60(%rbp), %eax                               #183.13
        movslq    %eax, %rax                                    #183.13
..LN1397:
        shlq      $5, %rax                                      #183.23
..LN1399:
        addq      88(%rbp), %rax                                #183.7
..LN1401:
        movl      -60(%rbp), %edx                               #183.13
        movslq    %edx, %rdx                                    #183.13
..LN1403:
        shlq      $3, %rdx                                      #183.23
        negq      %rdx                                          #183.23
..LN1405:
        addq      %rdx, %rax                                    #183.7
..LN1407:
        movl      -52(%rbp), %edx                               #183.13
..LN1409:
        movslq    %edx, %rdx                                    #183.23
        fldl      -8(%rax,%rdx,8)                               #183.23
..LN1411:
        fmulp     %st, %st(1)                                   #183.22
        movl      -60(%rbp), %eax                               #183.22
        movslq    %eax, %rax                                    #183.22
..LN1413:
        shlq      $5, %rax                                      #183.33
..LN1415:
        addq      96(%rbp), %rax                                #183.7
..LN1417:
        movl      -60(%rbp), %edx                               #183.22
        movslq    %edx, %rdx                                    #183.22
..LN1419:
        shlq      $3, %rdx                                      #183.33
        negq      %rdx                                          #183.33
..LN1421:
        addq      %rdx, %rax                                    #183.7
..LN1423:
        movl      -52(%rbp), %edx                               #183.22
..LN1425:
        movslq    %edx, %rdx                                    #183.33
        fldl      -8(%rax,%rdx,8)                               #183.33
        movl      -60(%rbp), %eax                               #183.33
        movslq    %eax, %rax                                    #183.33
..LN1427:
        shlq      $3, %rax                                      #183.43
        lea       (%rax,%rax,2), %rax                           #183.43
..LN1429:
        addq      88(%rbp), %rax                                #183.7
..LN1431:
        movl      -60(%rbp), %edx                               #183.33
        movslq    %edx, %rdx                                    #183.33
..LN1433:
        shlq      $3, %rdx                                      #183.43
        negq      %rdx                                          #183.43
..LN1435:
        addq      %rdx, %rax                                    #183.7
..LN1437:
        movl      -52(%rbp), %edx                               #183.33
..LN1439:
        movslq    %edx, %rdx                                    #183.43
        fldl      -8(%rax,%rdx,8)                               #183.43
..LN1441:
        fmulp     %st, %st(1)                                   #183.42
..LN1443:
        faddp     %st, %st(1)                                   #183.7
        fstpl     -296(%rbp)                                    #183.7
..LN1445:
        fldl      -832(%rbp)                                    #184.14
        fldl      -312(%rbp)                                    #184.14
..LN1447:
        fsubrp    %st, %st(1)                                   #184.20
        fldl      -96(%rbp)                                     #184.20
        fldl      -304(%rbp)                                    #184.20
        fldl      -296(%rbp)                                    #184.20
..LN1449:
        fsubrp    %st, %st(1)                                   #184.38
..LN1451:
        fmulp     %st, %st(1)                                   #184.31
..LN1453:
        fsubrp    %st, %st(1)                                   #184.7
        fstpl     -832(%rbp)                                    #184.7
..LN1455:
        fldl      -96(%rbp)                                     #185.14
        fldl      -304(%rbp)                                    #185.14
        fldl      -296(%rbp)                                    #185.14
..LN1457:
        faddp     %st, %st(1)                                   #185.32
..LN1459:
        fmulp     %st, %st(1)                                   #185.25
..LN1461:
        fldl      -800(%rbp)                                    #185.14
..LN1463:
        fsubp     %st, %st(1)                                   #185.7
        fstpl     -800(%rbp)                                    #185.7
..LN1465:
        movl      -60(%rbp), %eax                               #186.7
        movslq    %eax, %rax                                    #186.7
        movq      96(%rbp), %rdx                                #186.7
        lea       (%rdx,%rax,8), %rax                           #186.7
        movl      -60(%rbp), %edx                               #186.7
        movslq    %edx, %rdx                                    #186.7
..LN1467:
        shlq      $3, %rdx                                      #186.13
        negq      %rdx                                          #186.13
..LN1469:
        addq      %rdx, %rax                                    #186.7
        movl      -52(%rbp), %edx                               #186.7
..LN1471:
        movslq    %edx, %rdx                                    #186.13
        fldl      -8(%rax,%rdx,8)                               #186.13
        movl      -60(%rbp), %eax                               #186.13
        movslq    %eax, %rax                                    #186.13
..LN1473:
        shlq      $5, %rax                                      #186.23
..LN1475:
        addq      88(%rbp), %rax                                #186.7
..LN1477:
        movl      -60(%rbp), %edx                               #186.13
        movslq    %edx, %rdx                                    #186.13
..LN1479:
        shlq      $3, %rdx                                      #186.23
        negq      %rdx                                          #186.23
..LN1481:
        addq      %rdx, %rax                                    #186.7
..LN1483:
        movl      -52(%rbp), %edx                               #186.13
..LN1485:
        movslq    %edx, %rdx                                    #186.23
        fldl      -8(%rax,%rdx,8)                               #186.23
..LN1487:
        fmulp     %st, %st(1)                                   #186.22
        fstpt     -568(%rbp)                                    #186.22
        movl      -60(%rbp), %eax                               #186.22
        movslq    %eax, %rax                                    #186.22
..LN1489:
        shlq      $5, %rax                                      #186.33
..LN1491:
        addq      96(%rbp), %rax                                #186.7
..LN1493:
        movl      -60(%rbp), %edx                               #186.22
        movslq    %edx, %rdx                                    #186.22
..LN1495:
        shlq      $3, %rdx                                      #186.33
        negq      %rdx                                          #186.33
..LN1497:
        addq      %rdx, %rax                                    #186.7
        movq      %rax, -224(%rbp)                              #186.7
..LN1499:
        movl      -52(%rbp), %eax                               #186.22
        movl      %eax, -132(%rbp)                              #186.22
                                # LOE
..B1.146:                       # Preds ..B1.81
..LN1501:
        movl      -132(%rbp), %eax                              #186.33
        movslq    %eax, %rax                                    #186.33
        movq      -224(%rbp), %rdx                              #186.33
        fldl      -8(%rdx,%rax,8)                               #186.33
        movl      -60(%rbp), %eax                               #186.33
        movslq    %eax, %rax                                    #186.33
        movq      88(%rbp), %rdx                                #186.33
..LN1503:
        lea       (%rdx,%rax,8), %rax                           #186.7
..LN1505:
        movl      -60(%rbp), %edx                               #186.33
        movslq    %edx, %rdx                                    #186.33
..LN1507:
        shlq      $3, %rdx                                      #186.43
        negq      %rdx                                          #186.43
..LN1509:
        addq      %rdx, %rax                                    #186.7
..LN1511:
        movl      -52(%rbp), %edx                               #186.33
..LN1513:
        movslq    %edx, %rdx                                    #186.43
        fldl      -8(%rax,%rdx,8)                               #186.43
..LN1515:
        fmulp     %st, %st(1)                                   #186.42
..LN1517:
        fldt      -568(%rbp)                                    #186.7
        faddp     %st, %st(1)                                   #186.7
        fstpl     -312(%rbp)                                    #186.7
..LN1519:
        fldl      -824(%rbp)                                    #187.14
        fldl      -312(%rbp)                                    #187.14
..LN1521:
        fsubrp    %st, %st(1)                                   #187.20
..LN1523:
        fldl      -768(%rbp)                                    #187.27
..LN1525:
        fsubrp    %st, %st(1)                                   #187.26
..LN1527:
        fldl      -760(%rbp)                                    #187.34
..LN1529:
        faddp     %st, %st(1)                                   #187.7
        fstpl     -824(%rbp)                                    #187.7
..LN1531:
        movl      -60(%rbp), %eax                               #188.7
        movslq    %eax, %rax                                    #188.7
        movq      96(%rbp), %rdx                                #188.7
        lea       (%rdx,%rax,8), %rax                           #188.7
        movl      -60(%rbp), %edx                               #188.7
        movslq    %edx, %rdx                                    #188.7
..LN1533:
        shlq      $3, %rdx                                      #188.13
        negq      %rdx                                          #188.13
..LN1535:
        addq      %rdx, %rax                                    #188.7
        movl      -52(%rbp), %edx                               #188.7
..LN1537:
        movslq    %edx, %rdx                                    #188.13
        fldl      -8(%rax,%rdx,8)                               #188.13
        movl      -60(%rbp), %eax                               #188.13
        movslq    %eax, %rax                                    #188.13
..LN1539:
        shlq      $3, %rax                                      #188.23
        lea       (%rax,%rax,4), %rax                           #188.23
..LN1541:
        addq      88(%rbp), %rax                                #188.7
..LN1543:
        movl      -60(%rbp), %edx                               #188.13
        movslq    %edx, %rdx                                    #188.13
..LN1545:
        shlq      $3, %rdx                                      #188.23
        negq      %rdx                                          #188.23
..LN1547:
        addq      %rdx, %rax                                    #188.7
..LN1549:
        movl      -52(%rbp), %edx                               #188.13
..LN1551:
        movslq    %edx, %rdx                                    #188.23
        fldl      -8(%rax,%rdx,8)                               #188.23
..LN1553:
        fmulp     %st, %st(1)                                   #188.22
        movl      -60(%rbp), %eax                               #188.22
        movslq    %eax, %rax                                    #188.22
..LN1555:
        shlq      $3, %rax                                      #188.33
        lea       (%rax,%rax,4), %rax                           #188.33
..LN1557:
        addq      96(%rbp), %rax                                #188.7
..LN1559:
        movl      -60(%rbp), %edx                               #188.22
        movslq    %edx, %rdx                                    #188.22
..LN1561:
        shlq      $3, %rdx                                      #188.33
        negq      %rdx                                          #188.33
..LN1563:
        addq      %rdx, %rax                                    #188.7
..LN1565:
        movl      -52(%rbp), %edx                               #188.22
..LN1567:
        movslq    %edx, %rdx                                    #188.33
        fldl      -8(%rax,%rdx,8)                               #188.33
        movl      -60(%rbp), %eax                               #188.33
        movslq    %eax, %rax                                    #188.33
        movq      88(%rbp), %rdx                                #188.33
..LN1569:
        lea       (%rdx,%rax,8), %rax                           #188.7
..LN1571:
        movl      -60(%rbp), %edx                               #188.33
        movslq    %edx, %rdx                                    #188.33
..LN1573:
        shlq      $3, %rdx                                      #188.43
        negq      %rdx                                          #188.43
..LN1575:
        addq      %rdx, %rax                                    #188.7
..LN1577:
        movl      -52(%rbp), %edx                               #188.33
..LN1579:
        movslq    %edx, %rdx                                    #188.43
        fldl      -8(%rax,%rdx,8)                               #188.43
..LN1581:
        fmulp     %st, %st(1)                                   #188.42
..LN1583:
        faddp     %st, %st(1)                                   #188.7
        fstpl     -312(%rbp)                                    #188.7
..LN1585:
        movl      -60(%rbp), %eax                               #189.7
        movslq    %eax, %rax                                    #189.7
..LN1587:
        shlq      $4, %rax                                      #189.13
..LN1589:
        addq      96(%rbp), %rax                                #189.7
        movl      -60(%rbp), %edx                               #189.7
        movslq    %edx, %rdx                                    #189.7
..LN1591:
        shlq      $3, %rdx                                      #189.13
        negq      %rdx                                          #189.13
..LN1593:
        addq      %rdx, %rax                                    #189.7
        movl      -52(%rbp), %edx                               #189.7
..LN1595:
        movslq    %edx, %rdx                                    #189.13
        fldl      -8(%rax,%rdx,8)                               #189.13
        movl      -60(%rbp), %eax                               #189.13
        movslq    %eax, %rax                                    #189.13
..LN1597:
        shlq      $3, %rax                                      #189.23
        lea       (%rax,%rax,2), %rax                           #189.23
..LN1599:
        addq      88(%rbp), %rax                                #189.7
..LN1601:
        movl      -60(%rbp), %edx                               #189.13
        movslq    %edx, %rdx                                    #189.13
..LN1603:
        shlq      $3, %rdx                                      #189.23
        negq      %rdx                                          #189.23
..LN1605:
        addq      %rdx, %rax                                    #189.7
..LN1607:
        movl      -52(%rbp), %edx                               #189.13
..LN1609:
        movslq    %edx, %rdx                                    #189.23
        fldl      -8(%rax,%rdx,8)                               #189.23
..LN1611:
        fmulp     %st, %st(1)                                   #189.22
        movl      -60(%rbp), %eax                               #189.22
        movslq    %eax, %rax                                    #189.22
..LN1613:
        shlq      $3, %rax                                      #189.33
        lea       (%rax,%rax,2), %rax                           #189.33
..LN1615:
        addq      96(%rbp), %rax                                #189.7
..LN1617:
        movl      -60(%rbp), %edx                               #189.22
        movslq    %edx, %rdx                                    #189.22
..LN1619:
        shlq      $3, %rdx                                      #189.33
        negq      %rdx                                          #189.33
..LN1621:
        addq      %rdx, %rax                                    #189.7
..LN1623:
        movl      -52(%rbp), %edx                               #189.22
..LN1625:
        movslq    %edx, %rdx                                    #189.33
        fldl      -8(%rax,%rdx,8)                               #189.33
        movl      -60(%rbp), %eax                               #189.33
        movslq    %eax, %rax                                    #189.33
..LN1627:
        shlq      $4, %rax                                      #189.43
..LN1629:
        addq      88(%rbp), %rax                                #189.7
..LN1631:
        movl      -60(%rbp), %edx                               #189.33
        movslq    %edx, %rdx                                    #189.33
..LN1633:
        shlq      $3, %rdx                                      #189.43
        negq      %rdx                                          #189.43
..LN1635:
        addq      %rdx, %rax                                    #189.7
..LN1637:
        movl      -52(%rbp), %edx                               #189.33
..LN1639:
        movslq    %edx, %rdx                                    #189.43
        fldl      -8(%rax,%rdx,8)                               #189.43
..LN1641:
        fmulp     %st, %st(1)                                   #189.42
..LN1643:
        faddp     %st, %st(1)                                   #189.7
        fstpl     -304(%rbp)                                    #189.7
..LN1645:
        fldl      -816(%rbp)                                    #190.14
        fldl      -312(%rbp)                                    #190.14
..LN1647:
        fsubrp    %st, %st(1)                                   #190.20
        fldl      -96(%rbp)                                     #190.20
        fldl      -304(%rbp)                                    #190.20
..LN1649:
        fmulp     %st, %st(1)                                   #190.31
..LN1651:
        fsubrp    %st, %st(1)                                   #190.7
        fstpl     -816(%rbp)                                    #190.7
..LN1653:
        fldl      -792(%rbp)                                    #191.14
..LN1655:
        fldl      -752(%rbp)                                    #191.21
..LN1657:
        fsubrp    %st, %st(1)                                   #191.20
..LN1659:
        fldl      -744(%rbp)                                    #191.28
..LN1661:
        faddp     %st, %st(1)                                   #191.7
        fstpl     -792(%rbp)                                    #191.7
..LN1663:
        movl      -60(%rbp), %eax                               #192.7
        movslq    %eax, %rax                                    #192.7
..LN1665:
        shlq      $5, %rax                                      #192.13
..LN1667:
        addq      96(%rbp), %rax                                #192.7
        movl      -60(%rbp), %edx                               #192.7
        movslq    %edx, %rdx                                    #192.7
..LN1669:
        shlq      $3, %rdx                                      #192.13
        negq      %rdx                                          #192.13
..LN1671:
        addq      %rdx, %rax                                    #192.7
        movl      -52(%rbp), %edx                               #192.7
..LN1673:
        movslq    %edx, %rdx                                    #192.13
        fldl      -8(%rax,%rdx,8)                               #192.13
        movl      -60(%rbp), %eax                               #192.13
        movslq    %eax, %rax                                    #192.13
..LN1675:
        shlq      $3, %rax                                      #192.23
        lea       (%rax,%rax,4), %rax                           #192.23
..LN1677:
        addq      88(%rbp), %rax                                #192.7
..LN1679:
        movl      -60(%rbp), %edx                               #192.13
        movslq    %edx, %rdx                                    #192.13
..LN1681:
        shlq      $3, %rdx                                      #192.23
        negq      %rdx                                          #192.23
..LN1683:
        addq      %rdx, %rax                                    #192.7
..LN1685:
        movl      -52(%rbp), %edx                               #192.13
..LN1687:
        movslq    %edx, %rdx                                    #192.23
        fldl      -8(%rax,%rdx,8)                               #192.23
..LN1689:
        fmulp     %st, %st(1)                                   #192.22
        movl      -60(%rbp), %eax                               #192.22
        movslq    %eax, %rax                                    #192.22
..LN1691:
        shlq      $3, %rax                                      #192.33
        lea       (%rax,%rax,4), %rax                           #192.33
..LN1693:
        addq      96(%rbp), %rax                                #192.7
..LN1695:
        movl      -60(%rbp), %edx                               #192.22
        movslq    %edx, %rdx                                    #192.22
..LN1697:
        shlq      $3, %rdx                                      #192.33
        negq      %rdx                                          #192.33
..LN1699:
        addq      %rdx, %rax                                    #192.7
..LN1701:
        movl      -52(%rbp), %edx                               #192.22
..LN1703:
        movslq    %edx, %rdx                                    #192.33
        fldl      -8(%rax,%rdx,8)                               #192.33
        movl      -60(%rbp), %eax                               #192.33
        movslq    %eax, %rax                                    #192.33
..LN1705:
        shlq      $5, %rax                                      #192.43
..LN1707:
        addq      88(%rbp), %rax                                #192.7
..LN1709:
        movl      -60(%rbp), %edx                               #192.33
        movslq    %edx, %rdx                                    #192.33
..LN1711:
        shlq      $3, %rdx                                      #192.43
        negq      %rdx                                          #192.43
..LN1713:
        addq      %rdx, %rax                                    #192.7
..LN1715:
        movl      -52(%rbp), %edx                               #192.33
..LN1717:
        movslq    %edx, %rdx                                    #192.43
        fldl      -8(%rax,%rdx,8)                               #192.43
..LN1719:
        fmulp     %st, %st(1)                                   #192.42
..LN1721:
        faddp     %st, %st(1)                                   #192.7
        fstpl     -312(%rbp)                                    #192.7
..LN1723:
        fldl      -96(%rbp)                                     #193.14
        fldl      -312(%rbp)                                    #193.14
..LN1725:
        fmulp     %st, %st(1)                                   #193.25
..LN1727:
        fldl      -784(%rbp)                                    #193.14
..LN1729:
        fsubp     %st, %st(1)                                   #193.7
        fstpl     -784(%rbp)                                    #193.7
..LN1731:
        addl      $1, -52(%rbp)                                 #170.10
        movl      -52(%rbp), %eax                               #170.10
        movl      -156(%rbp), %edx                              #170.10
        cmpl      %edx, %eax                                    #170.10
        jle       ..B1.79       # Prob 50%                      #170.10
                                # LOE
..B1.82:                        # Preds ..B1.146 ..B1.77
..LN1733:
        fldl      -88(%rbp)                                     #197.7
        fstpl     -80(%rbp)                                     #197.7
..LN1735:
        movl      $1, -44(%rbp)                                 #198.10
                                # LOE
..B1.83:                        # Preds ..B1.83 ..B1.82
..LN1737:
        movl      -60(%rbp), %eax                               #199.7
        movslq    %eax, %rax                                    #199.7
..LN1739:
        shlq      $3, %rax                                      #199.19
..LN1741:
        movl      -44(%rbp), %edx                               #199.7
..LN1743:
        movslq    %edx, %rdx                                    #199.19
        imulq     %rax, %rdx                                    #199.19
..LN1745:
        addq      96(%rbp), %rdx                                #199.31
..LN1747:
        movl      -60(%rbp), %eax                               #199.7
        movslq    %eax, %rax                                    #199.7
..LN1749:
        shlq      $3, %rax                                      #199.19
        negq      %rax                                          #199.19
..LN1751:
        addq      %rax, %rdx                                    #199.31
..LN1753:
        movq      40(%rbp), %rax                                #199.7
        movl      (%rax), %eax                                  #199.7
..LN1755:
        movslq    %eax, %rax                                    #199.19
..LN1757:
        fldl      -8(%rdx,%rax,8)                               #199.31
..LN1759:
        fldl      -96(%rbp)                                     #199.7
..LN1761:
        fxch      %st(1)                                        #199.31
        fmul      %st(0), %st                                   #199.31
..LN1763:
        fmulp     %st, %st(1)                                   #199.7
        movl      -44(%rbp), %eax                               #199.7
        movslq    %eax, %rax                                    #199.7
        fstpl     -784(%rbp,%rax,8)                             #199.7
..LN1765:
        movl      -44(%rbp), %eax                               #200.3
        movslq    %eax, %rax                                    #200.3
        fldl      -80(%rbp)                                     #200.3
..LN1767:
        fldl      -784(%rbp,%rax,8)                             #200.15
..LN1769:
        faddp     %st, %st(1)                                   #200.7
        fstpl     -80(%rbp)                                     #200.7
..LN1771:
        addl      $1, -44(%rbp)                                 #198.10
        movl      -44(%rbp), %eax                               #198.10
        cmpl      $5, %eax                                      #198.10
        jle       ..B1.83       # Prob 50%                      #198.10
                                # LOE
..B1.84:                        # Preds ..B1.83
..LN1773:
        fldl      -376(%rbp)                                    #201.7
..LN1775:
        fldl      -848(%rbp)                                    #201.22
..LN1777:
        fmulp     %st, %st(1)                                   #201.21
..LN1779:
        fldl      -776(%rbp)                                    #201.29
..LN1781:
        faddp     %st, %st(1)                                   #201.28
        fldl      -80(%rbp)                                     #201.28
..LN1783:
        faddp     %st, %st(1)                                   #201.7
        fstpl     -704(%rbp)                                    #201.7
..LN1785:
        fldl      -392(%rbp)                                    #202.7
        movl      -60(%rbp), %eax                               #202.7
        movslq    %eax, %rax                                    #202.7
        movq      96(%rbp), %rdx                                #202.7
        lea       (%rdx,%rax,8), %rax                           #202.7
        movl      -60(%rbp), %edx                               #202.7
        movslq    %edx, %rdx                                    #202.7
..LN1787:
        shlq      $3, %rdx                                      #202.17
        negq      %rdx                                          #202.17
..LN1789:
        addq      %rdx, %rax                                    #202.7
        movq      40(%rbp), %rdx                                #202.7
        movl      (%rdx), %edx                                  #202.7
..LN1791:
        movslq    %edx, %rdx                                    #202.17
        fldl      -8(%rax,%rdx,8)                               #202.17
..LN1793:
        fmulp     %st, %st(1)                                   #202.16
        movl      -60(%rbp), %eax                               #202.16
        movslq    %eax, %rax                                    #202.16
..LN1795:
        shlq      $4, %rax                                      #202.30
..LN1797:
        addq      96(%rbp), %rax                                #202.7
..LN1799:
        movl      -60(%rbp), %edx                               #202.16
        movslq    %edx, %rdx                                    #202.16
..LN1801:
        shlq      $3, %rdx                                      #202.30
        negq      %rdx                                          #202.30
..LN1803:
        addq      %rdx, %rax                                    #202.7
..LN1805:
        movq      40(%rbp), %rdx                                #202.16
        movl      (%rdx), %edx                                  #202.16
..LN1807:
        movslq    %edx, %rdx                                    #202.30
        fldl      -8(%rax,%rdx,8)                               #202.30
..LN1809:
        fmulp     %st, %st(1)                                   #202.7
        fstpl     -312(%rbp)                                    #202.7
..LN1811:
        movl      -60(%rbp), %eax                               #203.7
        movslq    %eax, %rax                                    #203.7
..LN1813:
        shlq      $4, %rax                                      #203.13
..LN1815:
        addq      96(%rbp), %rax                                #203.7
        movl      -60(%rbp), %edx                               #203.7
        movslq    %edx, %rdx                                    #203.7
..LN1817:
        shlq      $3, %rdx                                      #203.13
        negq      %rdx                                          #203.13
..LN1819:
        addq      %rdx, %rax                                    #203.7
        movq      40(%rbp), %rdx                                #203.7
        movl      (%rdx), %edx                                  #203.7
..LN1821:
        movslq    %edx, %rdx                                    #203.13
        fldl      -8(%rax,%rdx,8)                               #203.13
        movl      -60(%rbp), %eax                               #203.13
        movslq    %eax, %rax                                    #203.13
..LN1823:
        shlq      $5, %rax                                      #203.26
..LN1825:
        addq      96(%rbp), %rax                                #203.7
..LN1827:
        movl      -60(%rbp), %edx                               #203.13
        movslq    %edx, %rdx                                    #203.13
..LN1829:
        shlq      $3, %rdx                                      #203.26
        negq      %rdx                                          #203.26
..LN1831:
        addq      %rdx, %rax                                    #203.7
..LN1833:
        movq      40(%rbp), %rdx                                #203.13
        movl      (%rdx), %edx                                  #203.13
..LN1835:
        movslq    %edx, %rdx                                    #203.26
        fldl      -8(%rax,%rdx,8)                               #203.26
..LN1837:
        fmulp     %st, %st(1)                                   #203.7
        fstpl     -304(%rbp)                                    #203.7
..LN1839:
        movl      -60(%rbp), %eax                               #204.7
        movslq    %eax, %rax                                    #204.7
..LN1841:
        shlq      $3, %rax                                      #204.13
        lea       (%rax,%rax,2), %rax                           #204.13
..LN1843:
        addq      96(%rbp), %rax                                #204.7
        movl      -60(%rbp), %edx                               #204.7
        movslq    %edx, %rdx                                    #204.7
..LN1845:
        shlq      $3, %rdx                                      #204.13
        negq      %rdx                                          #204.13
..LN1847:
        addq      %rdx, %rax                                    #204.7
        movq      40(%rbp), %rdx                                #204.7
        movl      (%rdx), %edx                                  #204.7
..LN1849:
        movslq    %edx, %rdx                                    #204.13
        fldl      -8(%rax,%rdx,8)                               #204.13
        movl      -60(%rbp), %eax                               #204.13
        movslq    %eax, %rax                                    #204.13
..LN1851:
        shlq      $3, %rax                                      #204.26
        lea       (%rax,%rax,4), %rax                           #204.26
..LN1853:
        addq      96(%rbp), %rax                                #204.7
..LN1855:
        movl      -60(%rbp), %edx                               #204.13
        movslq    %edx, %rdx                                    #204.13
..LN1857:
        shlq      $3, %rdx                                      #204.26
        negq      %rdx                                          #204.26
..LN1859:
        addq      %rdx, %rax                                    #204.7
..LN1861:
        movq      40(%rbp), %rdx                                #204.13
        movl      (%rdx), %edx                                  #204.13
..LN1863:
        movslq    %edx, %rdx                                    #204.26
        fldl      -8(%rax,%rdx,8)                               #204.26
..LN1865:
        fmulp     %st, %st(1)                                   #204.7
        fstpl     -296(%rbp)                                    #204.7
..LN1867:
        fldl      -376(%rbp)                                    #205.7
..LN1869:
        fldl      -840(%rbp)                                    #205.22
..LN1871:
        fmulp     %st, %st(1)                                   #205.21
        fldl      -312(%rbp)                                    #205.21
..LN1873:
        faddp     %st, %st(1)                                   #205.28
        fldl      -304(%rbp)                                    #205.28
..LN1875:
        faddp     %st, %st(1)                                   #205.34
        fldl      -296(%rbp)                                    #205.34
..LN1877:
        faddp     %st, %st(1)                                   #205.7
        fstpl     -696(%rbp)                                    #205.7
..LN1879:
        fldl      -376(%rbp)                                    #206.7
..LN1881:
        fldl      -808(%rbp)                                    #206.22
..LN1883:
        fmulp     %st, %st(1)                                   #206.21
        fldl      -304(%rbp)                                    #206.21
..LN1885:
        faddp     %st, %st(1)                                   #206.28
        fldl      -296(%rbp)                                    #206.28
..LN1887:
        fsubrp    %st, %st(1)                                   #206.7
        fstpl     -664(%rbp)                                    #206.7
..LN1889:
        fldl      -392(%rbp)                                    #207.7
        movl      -60(%rbp), %eax                               #207.7
        movslq    %eax, %rax                                    #207.7
        movq      96(%rbp), %rdx                                #207.7
        lea       (%rdx,%rax,8), %rax                           #207.7
        movl      -60(%rbp), %edx                               #207.7
        movslq    %edx, %rdx                                    #207.7
..LN1891:
        shlq      $3, %rdx                                      #207.17
        negq      %rdx                                          #207.17
..LN1893:
        addq      %rdx, %rax                                    #207.7
        movq      40(%rbp), %rdx                                #207.7
        movl      (%rdx), %edx                                  #207.7
..LN1895:
        movslq    %edx, %rdx                                    #207.17
        fldl      -8(%rax,%rdx,8)                               #207.17
..LN1897:
        fmulp     %st, %st(1)                                   #207.16
        movl      -60(%rbp), %eax                               #207.16
        movslq    %eax, %rax                                    #207.16
..LN1899:
        shlq      $3, %rax                                      #207.30
        lea       (%rax,%rax,2), %rax                           #207.30
..LN1901:
        addq      96(%rbp), %rax                                #207.7
..LN1903:
        movl      -60(%rbp), %edx                               #207.16
        movslq    %edx, %rdx                                    #207.16
..LN1905:
        shlq      $3, %rdx                                      #207.30
        negq      %rdx                                          #207.30
..LN1907:
        addq      %rdx, %rax                                    #207.7
..LN1909:
        movq      40(%rbp), %rdx                                #207.16
        movl      (%rdx), %edx                                  #207.16
..LN1911:
        movslq    %edx, %rdx                                    #207.30
        fldl      -8(%rax,%rdx,8)                               #207.30
..LN1913:
        fmulp     %st, %st(1)                                   #207.7
        fstpl     -312(%rbp)                                    #207.7
..LN1915:
        movl      -60(%rbp), %eax                               #208.7
        movslq    %eax, %rax                                    #208.7
..LN1917:
        shlq      $4, %rax                                      #208.13
..LN1919:
        addq      96(%rbp), %rax                                #208.7
        movl      -60(%rbp), %edx                               #208.7
        movslq    %edx, %rdx                                    #208.7
..LN1921:
        shlq      $3, %rdx                                      #208.13
        negq      %rdx                                          #208.13
..LN1923:
        addq      %rdx, %rax                                    #208.7
        movq      40(%rbp), %rdx                                #208.7
        movl      (%rdx), %edx                                  #208.7
..LN1925:
        movslq    %edx, %rdx                                    #208.13
        fldl      -8(%rax,%rdx,8)                               #208.13
        movl      -60(%rbp), %eax                               #208.13
        movslq    %eax, %rax                                    #208.13
..LN1927:
        shlq      $3, %rax                                      #208.26
        lea       (%rax,%rax,4), %rax                           #208.26
..LN1929:
        addq      96(%rbp), %rax                                #208.7
..LN1931:
        movl      -60(%rbp), %edx                               #208.13
        movslq    %edx, %rdx                                    #208.13
..LN1933:
        shlq      $3, %rdx                                      #208.26
        negq      %rdx                                          #208.26
..LN1935:
        addq      %rdx, %rax                                    #208.7
..LN1937:
        movq      40(%rbp), %rdx                                #208.13
        movl      (%rdx), %edx                                  #208.13
..LN1939:
        movslq    %edx, %rdx                                    #208.26
        fldl      -8(%rax,%rdx,8)                               #208.26
..LN1941:
        fmulp     %st, %st(1)                                   #208.7
        fstpl     -304(%rbp)                                    #208.7
..LN1943:
        movl      -60(%rbp), %eax                               #209.7
        movslq    %eax, %rax                                    #209.7
..LN1945:
        shlq      $3, %rax                                      #209.13
        lea       (%rax,%rax,2), %rax                           #209.13
..LN1947:
        addq      96(%rbp), %rax                                #209.7
        movl      -60(%rbp), %edx                               #209.7
        movslq    %edx, %rdx                                    #209.7
..LN1949:
        shlq      $3, %rdx                                      #209.13
        negq      %rdx                                          #209.13
..LN1951:
        addq      %rdx, %rax                                    #209.7
        movq      40(%rbp), %rdx                                #209.7
        movl      (%rdx), %edx                                  #209.7
..LN1953:
        movslq    %edx, %rdx                                    #209.13
        fldl      -8(%rax,%rdx,8)                               #209.13
        movl      -60(%rbp), %eax                               #209.13
        movslq    %eax, %rax                                    #209.13
..LN1955:
        shlq      $5, %rax                                      #209.26
..LN1957:
        addq      96(%rbp), %rax                                #209.7
..LN1959:
        movl      -60(%rbp), %edx                               #209.13
        movslq    %edx, %rdx                                    #209.13
..LN1961:
        shlq      $3, %rdx                                      #209.26
        negq      %rdx                                          #209.26
..LN1963:
        addq      %rdx, %rax                                    #209.7
..LN1965:
        movq      40(%rbp), %rdx                                #209.13
        movl      (%rdx), %edx                                  #209.13
..LN1967:
        movslq    %edx, %rdx                                    #209.26
        fldl      -8(%rax,%rdx,8)                               #209.26
..LN1969:
        fmulp     %st, %st(1)                                   #209.7
        fstpl     -296(%rbp)                                    #209.7
..LN1971:
        fldl      -376(%rbp)                                    #210.7
..LN1973:
        fldl      -832(%rbp)                                    #210.22
..LN1975:
        fmulp     %st, %st(1)                                   #210.21
        fldl      -312(%rbp)                                    #210.21
..LN1977:
        faddp     %st, %st(1)                                   #210.28
        fldl      -304(%rbp)                                    #210.28
..LN1979:
        faddp     %st, %st(1)                                   #210.34
        fldl      -296(%rbp)                                    #210.34
..LN1981:
        fsubrp    %st, %st(1)                                   #210.7
        fstpl     -688(%rbp)                                    #210.7
..LN1983:
        fldl      -376(%rbp)                                    #211.7
..LN1985:
        fldl      -800(%rbp)                                    #211.22
..LN1987:
        fmulp     %st, %st(1)                                   #211.21
        fldl      -304(%rbp)                                    #211.21
..LN1989:
        faddp     %st, %st(1)                                   #211.28
        fldl      -296(%rbp)                                    #211.28
..LN1991:
        faddp     %st, %st(1)                                   #211.7
        fstpl     -656(%rbp)                                    #211.7
..LN1993:
        fldl      -392(%rbp)                                    #212.7
        movl      -60(%rbp), %eax                               #212.7
        movslq    %eax, %rax                                    #212.7
        movq      96(%rbp), %rdx                                #212.7
        lea       (%rdx,%rax,8), %rax                           #212.7
        movl      -60(%rbp), %edx                               #212.7
        movslq    %edx, %rdx                                    #212.7
..LN1995:
        shlq      $3, %rdx                                      #212.17
        negq      %rdx                                          #212.17
..LN1997:
        addq      %rdx, %rax                                    #212.7
        movq      40(%rbp), %rdx                                #212.7
        movl      (%rdx), %edx                                  #212.7
..LN1999:
        movslq    %edx, %rdx                                    #212.17
        fldl      -8(%rax,%rdx,8)                               #212.17
..LN2001:
        fmulp     %st, %st(1)                                   #212.16
        movl      -60(%rbp), %eax                               #212.16
        movslq    %eax, %rax                                    #212.16
..LN2003:
        shlq      $5, %rax                                      #212.30
..LN2005:
        addq      96(%rbp), %rax                                #212.7
..LN2007:
        movl      -60(%rbp), %edx                               #212.16
        movslq    %edx, %rdx                                    #212.16
..LN2009:
        shlq      $3, %rdx                                      #212.30
        negq      %rdx                                          #212.30
..LN2011:
        addq      %rdx, %rax                                    #212.7
..LN2013:
        movq      40(%rbp), %rdx                                #212.16
        movl      (%rdx), %edx                                  #212.16
..LN2015:
        movslq    %edx, %rdx                                    #212.30
        fldl      -8(%rax,%rdx,8)                               #212.30
..LN2017:
        fmulp     %st, %st(1)                                   #212.7
        fstpl     -312(%rbp)                                    #212.7
..LN2019:
        fldl      -376(%rbp)                                    #213.7
..LN2021:
        fldl      -824(%rbp)                                    #213.22
..LN2023:
        fmulp     %st, %st(1)                                   #213.21
        fldl      -312(%rbp)                                    #213.21
..LN2025:
        faddp     %st, %st(1)                                   #213.28
..LN2027:
        fldl      -768(%rbp)                                    #213.35
..LN2029:
        faddp     %st, %st(1)                                   #213.34
..LN2031:
        fldl      -760(%rbp)                                    #213.42
..LN2033:
        fsubrp    %st, %st(1)                                   #213.7
        fstpl     -680(%rbp)                                    #213.7
..LN2035:
        fldl      -392(%rbp)                                    #214.7
        movl      -60(%rbp), %eax                               #214.7
        movslq    %eax, %rax                                    #214.7
        movq      96(%rbp), %rdx                                #214.7
        lea       (%rdx,%rax,8), %rax                           #214.7
        movl      -60(%rbp), %edx                               #214.7
        movslq    %edx, %rdx                                    #214.7
..LN2037:
        shlq      $3, %rdx                                      #214.17
        negq      %rdx                                          #214.17
..LN2039:
        addq      %rdx, %rax                                    #214.7
        movq      40(%rbp), %rdx                                #214.7
        movl      (%rdx), %edx                                  #214.7
..LN2041:
        movslq    %edx, %rdx                                    #214.17
        fldl      -8(%rax,%rdx,8)                               #214.17
..LN2043:
        fmulp     %st, %st(1)                                   #214.16
        movl      -60(%rbp), %eax                               #214.16
        movslq    %eax, %rax                                    #214.16
..LN2045:
        shlq      $3, %rax                                      #214.30
        lea       (%rax,%rax,4), %rax                           #214.30
..LN2047:
        addq      96(%rbp), %rax                                #214.7
..LN2049:
        movl      -60(%rbp), %edx                               #214.16
        movslq    %edx, %rdx                                    #214.16
..LN2051:
        shlq      $3, %rdx                                      #214.30
        negq      %rdx                                          #214.30
..LN2053:
        addq      %rdx, %rax                                    #214.7
..LN2055:
        movq      40(%rbp), %rdx                                #214.16
        movl      (%rdx), %edx                                  #214.16
..LN2057:
        movslq    %edx, %rdx                                    #214.30
        fldl      -8(%rax,%rdx,8)                               #214.30
..LN2059:
        fmulp     %st, %st(1)                                   #214.7
        fstpl     -312(%rbp)                                    #214.7
..LN2061:
        fldl      -376(%rbp)                                    #215.7
..LN2063:
        fldl      -816(%rbp)                                    #215.22
..LN2065:
        fmulp     %st, %st(1)                                   #215.21
        fldl      -312(%rbp)                                    #215.21
..LN2067:
        faddp     %st, %st(1)                                   #215.28
        movl      -60(%rbp), %eax                               #215.28
        movslq    %eax, %rax                                    #215.28
..LN2069:
        shlq      $4, %rax                                      #215.35
..LN2071:
        addq      96(%rbp), %rax                                #215.7
..LN2073:
        movl      -60(%rbp), %edx                               #215.28
        movslq    %edx, %rdx                                    #215.28
..LN2075:
        shlq      $3, %rdx                                      #215.35
        negq      %rdx                                          #215.35
..LN2077:
        addq      %rdx, %rax                                    #215.7
..LN2079:
        movq      40(%rbp), %rdx                                #215.28
        movl      (%rdx), %edx                                  #215.28
..LN2081:
        movslq    %edx, %rdx                                    #215.35
        fldl      -8(%rax,%rdx,8)                               #215.35
        movl      -60(%rbp), %eax                               #215.35
        movslq    %eax, %rax                                    #215.35
..LN2083:
        shlq      $3, %rax                                      #215.48
        lea       (%rax,%rax,2), %rax                           #215.48
..LN2085:
        addq      96(%rbp), %rax                                #215.7
..LN2087:
        movl      -60(%rbp), %edx                               #215.35
        movslq    %edx, %rdx                                    #215.35
..LN2089:
        shlq      $3, %rdx                                      #215.48
        negq      %rdx                                          #215.48
..LN2091:
        addq      %rdx, %rax                                    #215.7
..LN2093:
        movq      40(%rbp), %rdx                                #215.35
        movl      (%rdx), %edx                                  #215.35
..LN2095:
        movslq    %edx, %rdx                                    #215.48
        fldl      -8(%rax,%rdx,8)                               #215.48
..LN2097:
        fmulp     %st, %st(1)                                   #215.47
..LN2099:
        faddp     %st, %st(1)                                   #215.7
        fstpl     -672(%rbp)                                    #215.7
..LN2101:
        fldl      -376(%rbp)                                    #216.7
..LN2103:
        fldl      -792(%rbp)                                    #216.22
..LN2105:
        fmulp     %st, %st(1)                                   #216.21
..LN2107:
        fldl      -752(%rbp)                                    #216.29
..LN2109:
        faddp     %st, %st(1)                                   #216.28
..LN2111:
        fldl      -744(%rbp)                                    #216.36
..LN2113:
        fsubrp    %st, %st(1)                                   #216.7
        fstpl     -648(%rbp)                                    #216.7
..LN2115:
        fldl      -376(%rbp)                                    #217.7
..LN2117:
        fldl      -784(%rbp)                                    #217.22
..LN2119:
        fmulp     %st, %st(1)                                   #217.21
        movl      -60(%rbp), %eax                               #217.21
        movslq    %eax, %rax                                    #217.21
..LN2121:
        shlq      $5, %rax                                      #217.29
..LN2123:
        addq      96(%rbp), %rax                                #217.7
..LN2125:
        movl      -60(%rbp), %edx                               #217.21
        movslq    %edx, %rdx                                    #217.21
..LN2127:
        shlq      $3, %rdx                                      #217.29
        negq      %rdx                                          #217.29
..LN2129:
        addq      %rdx, %rax                                    #217.7
..LN2131:
        movq      40(%rbp), %rdx                                #217.21
        movl      (%rdx), %edx                                  #217.21
..LN2133:
        movslq    %edx, %rdx                                    #217.29
        fldl      -8(%rax,%rdx,8)                               #217.29
        movl      -60(%rbp), %eax                               #217.29
        movslq    %eax, %rax                                    #217.29
..LN2135:
        shlq      $3, %rax                                      #217.42
        lea       (%rax,%rax,4), %rax                           #217.42
..LN2137:
        addq      96(%rbp), %rax                                #217.7
..LN2139:
        movl      -60(%rbp), %edx                               #217.29
        movslq    %edx, %rdx                                    #217.29
..LN2141:
        shlq      $3, %rdx                                      #217.42
        negq      %rdx                                          #217.42
..LN2143:
        addq      %rdx, %rax                                    #217.7
..LN2145:
        movq      40(%rbp), %rdx                                #217.29
        movl      (%rdx), %edx                                  #217.29
..LN2147:
        movslq    %edx, %rdx                                    #217.42
        fldl      -8(%rax,%rdx,8)                               #217.42
..LN2149:
        fmulp     %st, %st(1)                                   #217.41
..LN2151:
        faddp     %st, %st(1)                                   #217.7
        fstpl     -640(%rbp)                                    #217.7
..LN2153:
        fldl      -704(%rbp)                                    #221.11
..LN2155:
        fldl      -696(%rbp)                                    #221.20
..LN2157:
        faddp     %st, %st(1)                                   #221.19
..LN2159:
        fldl      -680(%rbp)                                    #221.29
..LN2161:
        faddp     %st, %st(1)                                   #221.28
..LN2163:
        fldl      -664(%rbp)                                    #221.38
..LN2165:
        faddp     %st, %st(1)                                   #221.37
..LN2167:
        fldl      -648(%rbp)                                    #221.47
..LN2169:
        faddp     %st, %st(1)                                   #221.7
        fstpl     -80(%rbp)                                     #221.7
..LN2171:
        fldl      -80(%rbp)                                     #222.7
        fstpl     -288(%rbp)                                    #222.7
..LN2173:
        fldl      -80(%rbp)                                     #223.7
        fstpl     -72(%rbp)                                     #223.7
..LN2175:
        movl      $0, -152(%rbp)                                #224.7
..LN2177:
        movl      $49, -148(%rbp)                               #225.7
..LN2179:
        fldl      -384(%rbp)                                    #226.7
        movl      -148(%rbp), %eax                              #226.7
..LN2181:
        addl      $1, %eax                                      #226.18
        movl      %eax, -232(%rbp)                              #226.18
        fildl     -232(%rbp)                                    #226.18
..LN2183:
        fstpl     -216(%rbp)                                    #226.7
        fldl      -216(%rbp)                                    #226.7
        fdivrp    %st, %st(1)                                   #226.7
        fstpl     -344(%rbp)                                    #226.7
..LN2185:
        fldl      -408(%rbp)                                    #227.7
        fstpl     -776(%rbp)                                    #227.7
..LN2187:
        movl      -148(%rbp), %eax                              #228.10
        movl      %eax, -144(%rbp)                              #228.10
        movl      $1, -44(%rbp)                                 #228.10
        movl      -144(%rbp), %eax                              #228.10
        testl     %eax, %eax                                    #228.10
        jle       ..B1.95       # Prob 50%                      #228.10
                                # LOE
..B1.86:                        # Preds ..B1.84 ..B1.94
..LN2189:
        movl      -44(%rbp), %eax                               #229.13
        movl      %eax, -232(%rbp)                              #229.13
        fildl     -232(%rbp)                                    #229.13
..LN2191:
        fstpl     -216(%rbp)                                    #229.7
        fldl      -216(%rbp)                                    #229.7
..LN2193:
        fldl      -344(%rbp)                                    #229.13
..LN2195:
        fmulp     %st, %st(1)                                   #229.7
        fstpl     -280(%rbp)                                    #229.7
..LN2197:
        movsd     -280(%rbp), %xmm0                             #230.14
        movl      $1, %eax                                      #230.14
        call      cos                                           #230.14
                                # LOE xmm0
..B1.147:                       # Preds ..B1.86
..LN2199:
        movsd     %xmm0, -768(%rbp)                             #230.7
..LN2201:
        movsd     -280(%rbp), %xmm0                             #231.14
        movl      $1, %eax                                      #231.14
        call      sin                                           #231.14
                                # LOE xmm0
..B1.148:                       # Preds ..B1.147
..LN2203:
        movsd     %xmm0, -760(%rbp)                             #231.7
..LN2205:
        movl      $4, -48(%rbp)                                 #232.10
                                # LOE
..B1.87:                        # Preds ..B1.148 ..B1.87
..LN2207:
        fldl      -768(%rbp)                                    #233.14
        movl      -48(%rbp), %eax                               #233.14
        addl      $-2, %eax                                     #233.14
        movslq    %eax, %rax                                    #233.14
..LN2209:
        fldl      -784(%rbp,%rax,8)                             #233.21
..LN2211:
        fmulp     %st, %st(1)                                   #233.20
..LN2213:
        fldl      -760(%rbp)                                    #233.30
        movl      -48(%rbp), %eax                               #233.30
        addl      $-1, %eax                                     #233.30
        movslq    %eax, %rax                                    #233.30
..LN2215:
        fldl      -784(%rbp,%rax,8)                             #233.37
..LN2217:
        fmulp     %st, %st(1)                                   #233.36
..LN2219:
        fsubrp    %st, %st(1)                                   #233.7
        movl      -48(%rbp), %eax                               #233.7
        movslq    %eax, %rax                                    #233.7
        fstpl     -784(%rbp,%rax,8)                             #233.7
..LN2221:
        fldl      -768(%rbp)                                    #234.16
        movl      -48(%rbp), %eax                               #234.16
        addl      $-1, %eax                                     #234.16
        movslq    %eax, %rax                                    #234.16
..LN2223:
        fldl      -784(%rbp,%rax,8)                             #234.23
..LN2225:
        fmulp     %st, %st(1)                                   #234.22
..LN2227:
        fldl      -760(%rbp)                                    #234.32
        movl      -48(%rbp), %eax                               #234.32
        addl      $-2, %eax                                     #234.32
        movslq    %eax, %rax                                    #234.32
..LN2229:
        fldl      -784(%rbp,%rax,8)                             #234.39
..LN2231:
        fmulp     %st, %st(1)                                   #234.38
..LN2233:
        faddp     %st, %st(1)                                   #234.7
..LN2235:
        movl      -48(%rbp), %eax                               #234.3
        addl      $1, %eax                                      #234.3
        movslq    %eax, %rax                                    #234.3
..LN2237:
        fstpl     -784(%rbp,%rax,8)                             #234.7
..LN2239:
        addl      $2, -48(%rbp)                                 #232.10
        movl      -48(%rbp), %eax                               #232.10
        cmpl      $8, %eax                                      #232.10
        jle       ..B1.87       # Prob 50%                      #232.10
                                # LOE
..B1.88:                        # Preds ..B1.87
..LN2241:
        fldl      -80(%rbp)                                     #235.7
        fstpl     -264(%rbp)                                    #235.7
..LN2243:
        fldl      -88(%rbp)                                     #236.7
        fstpl     -80(%rbp)                                     #236.7
..LN2245:
        movl      $1, -48(%rbp)                                 #237.10
                                # LOE
..B1.89:                        # Preds ..B1.89 ..B1.88
..LN2247:
        movl      -48(%rbp), %eax                               #238.3
        movslq    %eax, %rax                                    #238.3
..LN2249:
        fldl      -712(%rbp,%rax,8)                             #238.15
        movl      -48(%rbp), %eax                               #238.15
        movslq    %eax, %rax                                    #238.15
..LN2251:
        fldl      -784(%rbp,%rax,8)                             #238.24
..LN2253:
        fmulp     %st, %st(1)                                   #238.23
..LN2255:
        fldl      -80(%rbp)                                     #238.3
..LN2257:
        faddp     %st, %st(1)                                   #238.7
        fstpl     -80(%rbp)                                     #238.7
..LN2259:
        addl      $1, -48(%rbp)                                 #237.10
        movl      -48(%rbp), %eax                               #237.10
        cmpl      $9, %eax                                      #237.10
        jle       ..B1.89       # Prob 50%                      #237.10
                                # LOE
..B1.90:                        # Preds ..B1.89
..LN2261:
        fldl      -80(%rbp)                                     #239.7
..LN2263:
        fabs                                                    #239.11
        fldl      -72(%rbp)                                     #239.11
..LN2265:
        fabs                                                    #239.26
..LN2267:
        fcomip    %st(1), %st                                   #239.21
        fstp      %st(0)                                        #239.21
        jae       ..B1.92       # Prob 50%                      #239.21
        jp        ..B1.92       # Prob 0%                       #239.21
                                # LOE
..B1.91:                        # Preds ..B1.90
..LN2269:
        fldl      -80(%rbp)                                     #240.11
        fstpl     -72(%rbp)                                     #240.11
..LN2271:
        movl      -44(%rbp), %eax                               #241.11
        movl      %eax, -152(%rbp)                              #241.11
..LN2273:
        fldl      -264(%rbp)                                    #242.11
        fstpl     -312(%rbp)                                    #242.11
        jmp       ..B1.94       # Prob 100%                     #242.11
                                # LOE
..B1.92:                        # Preds ..B1.90
..LN2275:
        movl      -152(%rbp), %eax                              #243.7
..LN2277:
        addl      $1, %eax                                      #243.28
..LN2279:
        movl      -44(%rbp), %edx                               #243.7
..LN2281:
        cmpl      %eax, %edx                                    #243.18
        jne       ..B1.94       # Prob 50%                      #243.18
                                # LOE
..B1.93:                        # Preds ..B1.92
..LN2283:
        fldl      -80(%rbp)                                     #244.11
        fstpl     -304(%rbp)                                    #244.11
                                # LOE
..B1.94:                        # Preds ..B1.91 ..B1.93 ..B1.92
..LN2285:
        movq      $0, -256(%rbp)                                #246.3
..LN2287:
        addl      $1, -44(%rbp)                                 #228.10
        movl      -44(%rbp), %eax                               #228.10
        movl      -144(%rbp), %edx                              #228.10
        cmpl      %edx, %eax                                    #228.10
        jle       ..B1.86       # Prob 50%                      #228.10
                                # LOE
..B1.95:                        # Preds ..B1.84 ..B1.94
..LN2289:
        movl      -152(%rbp), %eax                              #247.7
..LN2291:
        testl     %eax, %eax                                    #247.17
        jne       ..B1.97       # Prob 50%                      #247.17
                                # LOE
..B1.96:                        # Preds ..B1.95
..LN2293:
        fldl      -80(%rbp)                                     #247.25
        fstpl     -312(%rbp)                                    #247.25
                                # LOE
..B1.97:                        # Preds ..B1.96 ..B1.95
..LN2295:
        movl      -152(%rbp), %eax                              #248.7
        movl      -148(%rbp), %edx                              #248.7
..LN2297:
        cmpl      %edx, %eax                                    #248.17
        jne       ..B1.99       # Prob 50%                      #248.17
                                # LOE
..B1.98:                        # Preds ..B1.97
..LN2299:
        fldl      -288(%rbp)                                    #248.26
        fstpl     -304(%rbp)                                    #248.26
                                # LOE
..B1.99:                        # Preds ..B1.98 ..B1.97
..LN2301:
        fldl      -88(%rbp)                                     #249.7
        fstpl     -272(%rbp)                                    #249.7
..LN2303:
        fldl      -312(%rbp)                                    #250.7
        fldl      -304(%rbp)                                    #250.7
..LN2305:
        fucomip   %st(1), %st                                   #250.17
        fstp      %st(0)                                        #250.17
        jp        ..B1.100      # Prob 0%                       #250.17
        je        ..B1.101      # Prob 50%                      #250.17
                                # LOE
..B1.100:                       # Preds ..B1.99
..LN2307:
        fldl      -312(%rbp)                                    #251.11
        fldl      -72(%rbp)                                     #251.11
        fsubrp    %st, %st(1)                                   #251.11
        fstpl     -312(%rbp)                                    #251.11
..LN2309:
        fldl      -304(%rbp)                                    #252.11
        fldl      -72(%rbp)                                     #252.11
        fsubrp    %st, %st(1)                                   #252.11
        fstpl     -304(%rbp)                                    #252.11
..LN2311:
        fldl      -96(%rbp)                                     #253.11
        fldl      -312(%rbp)                                    #253.11
        fldl      -304(%rbp)                                    #253.11
..LN2313:
        fsubrp    %st, %st(1)                                   #253.27
..LN2315:
        fmulp     %st, %st(1)                                   #253.20
        fldl      -312(%rbp)                                    #253.20
        fldl      -304(%rbp)                                    #253.20
..LN2317:
        faddp     %st, %st(1)                                   #253.41
..LN2319:
        fdivrp    %st, %st(1)                                   #253.11
        fstpl     -272(%rbp)                                    #253.11
                                # LOE
..B1.101:                       # Preds ..B1.100 ..B1.99
..LN2321:
        fldl      -344(%rbp)                                    #255.7
..LN2323:
        movl      -152(%rbp), %eax                              #255.19
        movl      %eax, -232(%rbp)                              #255.19
        fildl     -232(%rbp)                                    #255.19
..LN2325:
        fstpl     -216(%rbp)                                    #255.7
        fldl      -216(%rbp)                                    #255.7
..LN2327:
        fldl      -272(%rbp)                                    #255.19
..LN2329:
        faddp     %st, %st(1)                                   #255.32
..LN2331:
        fmulp     %st, %st(1)                                   #255.7
        fstpl     -280(%rbp)                                    #255.7
..LN2333:
        movsd     -280(%rbp), %xmm0                             #260.14
        movl      $1, %eax                                      #260.14
        call      cos                                           #260.14
                                # LOE xmm0
..B1.149:                       # Preds ..B1.101
..LN2335:
        movsd     %xmm0, -768(%rbp)                             #260.7
..LN2337:
        movsd     -280(%rbp), %xmm0                             #261.14
        movl      $1, %eax                                      #261.14
        call      sin                                           #261.14
                                # LOE xmm0
..B1.150:                       # Preds ..B1.149
..LN2339:
        movsd     %xmm0, -760(%rbp)                             #261.7
..LN2341:
        movl      $4, -48(%rbp)                                 #262.10
                                # LOE
..B1.102:                       # Preds ..B1.150 ..B1.102
..LN2343:
        fldl      -768(%rbp)                                    #263.14
        movl      -48(%rbp), %eax                               #263.14
        addl      $-2, %eax                                     #263.14
        movslq    %eax, %rax                                    #263.14
..LN2345:
        fldl      -784(%rbp,%rax,8)                             #263.21
..LN2347:
        fmulp     %st, %st(1)                                   #263.20
..LN2349:
        fldl      -760(%rbp)                                    #263.30
        movl      -48(%rbp), %eax                               #263.30
        addl      $-1, %eax                                     #263.30
        movslq    %eax, %rax                                    #263.30
..LN2351:
        fldl      -784(%rbp,%rax,8)                             #263.37
..LN2353:
        fmulp     %st, %st(1)                                   #263.36
..LN2355:
        fsubrp    %st, %st(1)                                   #263.7
        movl      -48(%rbp), %eax                               #263.7
        movslq    %eax, %rax                                    #263.7
        fstpl     -784(%rbp,%rax,8)                             #263.7
..LN2357:
        fldl      -768(%rbp)                                    #264.16
        movl      -48(%rbp), %eax                               #264.16
        addl      $-1, %eax                                     #264.16
        movslq    %eax, %rax                                    #264.16
..LN2359:
        fldl      -784(%rbp,%rax,8)                             #264.23
..LN2361:
        fmulp     %st, %st(1)                                   #264.22
..LN2363:
        fldl      -760(%rbp)                                    #264.32
        movl      -48(%rbp), %eax                               #264.32
        addl      $-2, %eax                                     #264.32
        movslq    %eax, %rax                                    #264.32
..LN2365:
        fldl      -784(%rbp,%rax,8)                             #264.39
..LN2367:
        fmulp     %st, %st(1)                                   #264.38
..LN2369:
        faddp     %st, %st(1)                                   #264.7
..LN2371:
        movl      -48(%rbp), %eax                               #264.3
        addl      $1, %eax                                      #264.3
        movslq    %eax, %rax                                    #264.3
..LN2373:
        fstpl     -784(%rbp,%rax,8)                             #264.7
..LN2375:
        addl      $2, -48(%rbp)                                 #262.10
        movl      -48(%rbp), %eax                               #262.10
        cmpl      $8, %eax                                      #262.10
        jle       ..B1.102      # Prob 50%                      #262.10
                                # LOE
..B1.103:                       # Preds ..B1.102
..LN2377:
        movq      72(%rbp), %rax                                #265.7
        fldl      -88(%rbp)                                     #265.7
        fstpl     (%rax)                                        #265.7
..LN2379:
        fldl      -88(%rbp)                                     #266.7
        fstpl     -72(%rbp)                                     #266.7
..LN2381:
        movl      $1, -48(%rbp)                                 #267.10
                                # LOE
..B1.104:                       # Preds ..B1.104 ..B1.103
..LN2383:
        movq      72(%rbp), %rax                                #268.7
        movl      -48(%rbp), %edx                               #268.7
        movslq    %edx, %rdx                                    #268.7
..LN2385:
        fldl      -856(%rbp,%rdx,8)                             #268.17
        movl      -48(%rbp), %edx                               #268.17
        movslq    %edx, %rdx                                    #268.17
..LN2387:
        fldl      -784(%rbp,%rdx,8)                             #268.24
..LN2389:
        fmulp     %st, %st(1)                                   #268.23
..LN2391:
        fldl      (%rax)                                        #268.7
        faddp     %st, %st(1)                                   #268.7
        movq      72(%rbp), %rax                                #268.7
        fstpl     (%rax)                                        #268.7
..LN2393:
        movl      -48(%rbp), %eax                               #269.3
        movslq    %eax, %rax                                    #269.3
..LN2395:
        fldl      -712(%rbp,%rax,8)                             #269.21
        movl      -48(%rbp), %eax                               #269.21
        movslq    %eax, %rax                                    #269.21
..LN2397:
        fldl      -784(%rbp,%rax,8)                             #269.30
..LN2399:
        fmulp     %st, %st(1)                                   #269.29
..LN2401:
        fldl      -72(%rbp)                                     #269.3
..LN2403:
        faddp     %st, %st(1)                                   #269.7
        fstpl     -72(%rbp)                                     #269.7
..LN2405:
        addl      $1, -48(%rbp)                                 #267.10
        movl      -48(%rbp), %eax                               #267.10
        cmpl      $9, %eax                                      #267.10
        jle       ..B1.104      # Prob 50%                      #267.10
                                # LOE
..B1.105:                       # Preds ..B1.104
..LN2407:
        movq      24(%rbp), %rax                                #270.7
..LN2409:
        movl      (%rax), %eax                                  #270.10
        movl      %eax, -140(%rbp)                              #270.10
        movl      $1, -52(%rbp)                                 #270.10
        movl      -140(%rbp), %eax                              #270.10
        testl     %eax, %eax                                    #270.10
        jle       ..B1.110      # Prob 50%                      #270.10
                                # LOE
..B1.107:                       # Preds ..B1.105 ..B1.108
..LN2411:
        movl      -52(%rbp), %eax                               #271.7
        movslq    %eax, %rax                                    #271.7
        movq      64(%rbp), %rdx                                #271.7
        fldl      -88(%rbp)                                     #271.7
        fstpl     -8(%rdx,%rax,8)                               #271.7
..LN2413:
        movl      $1, -48(%rbp)                                 #272.10
        jmp       ..B1.109      # Prob 100%                     #272.10
                                # LOE
..B1.108:                       # Preds ..B1.109
..LN2415:
        addl      $1, -52(%rbp)                                 #270.10
        movl      -52(%rbp), %eax                               #270.10
        movl      -140(%rbp), %edx                              #270.10
        cmpl      %edx, %eax                                    #270.10
        jle       ..B1.107      # Prob 50%                      #270.10
        jmp       ..B1.110      # Prob 100%                     #270.10
                                # LOE
..B1.109:                       # Preds ..B1.109 ..B1.107
..LN2417:
        movl      -52(%rbp), %eax                               #273.7
..LN2419:
        movslq    %eax, %rax                                    #273.15
..LN2421:
        movq      64(%rbp), %rdx                                #273.7
..LN2423:
        movl      -60(%rbp), %ecx                               #273.15
        movslq    %ecx, %rcx                                    #273.15
..LN2425:
        shlq      $3, %rcx                                      #273.23
..LN2427:
        movl      -48(%rbp), %ebx                               #273.15
..LN2429:
        movslq    %ebx, %rbx                                    #273.23
        imulq     %rcx, %rbx                                    #273.23
..LN2431:
        addq      96(%rbp), %rbx                                #273.7
..LN2433:
        movl      -60(%rbp), %ecx                               #273.15
        movslq    %ecx, %rcx                                    #273.15
..LN2435:
        shlq      $3, %rcx                                      #273.23
        negq      %rcx                                          #273.23
..LN2437:
        addq      %rcx, %rbx                                    #273.7
..LN2439:
        movl      -52(%rbp), %ecx                               #273.15
..LN2441:
        movslq    %ecx, %rcx                                    #273.23
        fldl      -8(%rbx,%rcx,8)                               #273.23
        movl      -48(%rbp), %ecx                               #273.23
        movslq    %ecx, %rcx                                    #273.23
..LN2443:
        fldl      -784(%rbp,%rcx,8)                             #273.33
..LN2445:
        fmulp     %st, %st(1)                                   #273.32
..LN2447:
        fldl      -8(%rdx,%rax,8)                               #273.15
..LN2449:
        faddp     %st, %st(1)                                   #273.7
..LN2451:
        movl      -52(%rbp), %eax                               #273.3
..LN2453:
        movslq    %eax, %rax                                    #273.7
..LN2455:
        movq      64(%rbp), %rdx                                #273.3
..LN2457:
        fstpl     -8(%rdx,%rax,8)                               #273.7
..LN2459:
        addl      $1, -48(%rbp)                                 #272.10
        movl      -48(%rbp), %eax                               #272.10
        cmpl      $5, %eax                                      #272.10
        jle       ..B1.109      # Prob 50%                      #272.10
        jmp       ..B1.108      # Prob 100%                     #272.10
                                # LOE
..B1.110:                       # Preds ..B1.105 ..B1.108
..LN2461:
        movq      40(%rbp), %rax                                #274.7
        movl      (%rax), %eax                                  #274.7
..LN2463:
        movslq    %eax, %rax                                    #274.11
..LN2465:
        movq      64(%rbp), %rdx                                #274.7
        fldl      -8(%rdx,%rax,8)                               #274.7
        fstpl     -248(%rbp)                                    #274.7
..LN2467:
        fldl      -88(%rbp)                                     #275.7
        fstpl     -368(%rbp)                                    #275.7
..LN2469:
        fldl      -88(%rbp)                                     #276.7
        fstpl     -312(%rbp)                                    #276.7
..LN2471:
        fldl      -88(%rbp)                                     #277.7
        fstpl     -304(%rbp)                                    #277.7
..LN2473:
        movq      -128(%rbp), %rax                              #278.7
..LN2475:
        movl      (%rax), %eax                                  #278.10
        movl      %eax, -136(%rbp)                              #278.10
        movl      $1, -44(%rbp)                                 #278.10
        movl      -136(%rbp), %eax                              #278.10
        testl     %eax, %eax                                    #278.10
        jle       ..B1.113      # Prob 50%                      #278.10
                                # LOE
..B1.112:                       # Preds ..B1.110 ..B1.112
..LN2477:
        fldl      -768(%rbp)                                    #279.12
        movl      -44(%rbp), %eax                               #279.12
..LN2479:
        movslq    %eax, %rax                                    #279.19
..LN2481:
        movq      48(%rbp), %rdx                                #279.12
..LN2483:
        fldl      -8(%rdx,%rax,8)                               #279.19
..LN2485:
        fmulp     %st, %st(1)                                   #279.18
..LN2487:
        fldl      -760(%rbp)                                    #279.24
        movl      -44(%rbp), %eax                               #279.24
..LN2489:
        movslq    %eax, %rax                                    #279.31
..LN2491:
        movq      80(%rbp), %rdx                                #279.24
..LN2493:
        fldl      -8(%rdx,%rax,8)                               #279.31
..LN2495:
        fmulp     %st, %st(1)                                   #279.30
..LN2497:
        faddp     %st, %st(1)                                   #279.7
        movl      -44(%rbp), %eax                               #279.7
        movslq    %eax, %rax                                    #279.7
        movq      48(%rbp), %rdx                                #279.7
        fstpl     -8(%rdx,%rax,8)                               #279.7
..LN2499:
        movl      -44(%rbp), %eax                               #280.7
..LN2501:
        movslq    %eax, %rax                                    #280.12
..LN2503:
        movq      -424(%rbp), %rdx                              #280.7
..LN2505:
        movl      -44(%rbp), %ecx                               #280.12
..LN2507:
        movslq    %ecx, %rcx                                    #280.20
..LN2509:
        movq      48(%rbp), %rbx                                #280.12
        fldl      -8(%rdx,%rax,8)                               #280.12
..LN2511:
        fldl      -8(%rbx,%rcx,8)                               #280.20
..LN2513:
        faddp     %st, %st(1)                                   #280.7
        movl      -44(%rbp), %eax                               #280.7
        movslq    %eax, %rax                                    #280.7
        movq      56(%rbp), %rdx                                #280.7
        fstpl     -8(%rdx,%rax,8)                               #280.7
..LN2515:
        movl      -44(%rbp), %eax                               #281.7
..LN2517:
        movslq    %eax, %rax                                    #281.13
..LN2519:
        movq      48(%rbp), %rdx                                #281.7
..LN2521:
        fldl      -8(%rdx,%rax,8)                               #281.17
        fmul      %st(0), %st                                   #281.17
..LN2523:
        fldl      -368(%rbp)                                    #281.7
        faddp     %st, %st(1)                                   #281.7
        fstpl     -368(%rbp)                                    #281.7
..LN2525:
        movl      -44(%rbp), %eax                               #282.7
..LN2527:
        movslq    %eax, %rax                                    #282.19
..LN2529:
        movq      48(%rbp), %rdx                                #282.7
..LN2531:
        fldl      -8(%rdx,%rax,8)                               #282.19
        movl      -44(%rbp), %eax                               #282.19
..LN2533:
        movslq    %eax, %rax                                    #282.24
..LN2535:
        movq      56(%rbp), %rdx                                #282.19
..LN2537:
        fldl      -8(%rdx,%rax,8)                               #282.24
..LN2539:
        fmulp     %st, %st(1)                                   #282.23
..LN2541:
        fldl      -312(%rbp)                                    #282.7
        faddp     %st, %st(1)                                   #282.7
        fstpl     -312(%rbp)                                    #282.7
..LN2543:
        movl      -44(%rbp), %eax                               #283.3
..LN2545:
        movslq    %eax, %rax                                    #283.19
..LN2547:
        movq      56(%rbp), %rdx                                #283.3
..LN2549:
        fldl      -8(%rdx,%rax,8)                               #283.19
        movl      -44(%rbp), %eax                               #283.19
..LN2551:
        movslq    %eax, %rax                                    #283.24
..LN2553:
        movq      56(%rbp), %rdx                                #283.19
..LN2555:
        fldl      -8(%rdx,%rax,8)                               #283.24
..LN2557:
        fmulp     %st, %st(1)                                   #283.23
..LN2559:
        fldl      -304(%rbp)                                    #283.3
..LN2561:
        faddp     %st, %st(1)                                   #283.7
        fstpl     -304(%rbp)                                    #283.7
..LN2563:
        addl      $1, -44(%rbp)                                 #278.10
        movl      -44(%rbp), %eax                               #278.10
        movl      -136(%rbp), %edx                              #278.10
        cmpl      %edx, %eax                                    #278.10
        jle       ..B1.112      # Prob 50%                      #278.10
                                # LOE
..B1.113:                       # Preds ..B1.110 ..B1.112
..LN2565:
        movq      -128(%rbp), %rax                              #284.7
        movl      -188(%rbp), %edx                              #284.7
        movl      (%rax), %eax                                  #284.7
..LN2567:
        cmpl      %eax, %edx                                    #284.17
        jl        ..B1.115      # Prob 50%                      #284.17
                                # LOE
..B1.114:                       # Preds ..B1.113
..LN2569:
        movq      $0, -576(%rbp)                                #284.25
        jmp       ..B1.136      # Prob 100%                     #284.25
                                # LOE
..B1.115:                       # Preds ..B1.113
..LN2571:
        movl      -188(%rbp), %eax                              #285.7
..LN2573:
        cmpl      $1, %eax                                      #285.17
        jle       ..B1.117      # Prob 50%                      #285.17
                                # LOE
..B1.116:                       # Preds ..B1.115
..LN2575:
        fldl      -456(%rbp)                                    #285.25
        fldl      -288(%rbp)                                    #285.25
        fxch      %st(1)                                        #285.25
        fcomi     %st(1), %st                                   #285.25
        fcmovbe   %st(1), %st                                   #285.25
        fstp      %st(1)                                        #285.25
        fstpl     -456(%rbp)                                    #285.25
                                # LOE
..B1.117:                       # Preds ..B1.116 ..B1.115
..LN2577:
        fldl      -72(%rbp)                                     #286.7
..LN2579:
        fabs                                                    #286.11
..LN2581:
        fldl      _2il0floatpacket.6(%rip)                      #286.34
..LN2583:
        fldl      -456(%rbp)                                    #286.11
..LN2585:
        fabs                                                    #286.35
..LN2587:
        fmulp     %st, %st(1)                                   #286.34
..LN2589:
        fstpl     -216(%rbp)                                    #286.24
        fldl      -216(%rbp)                                    #286.24
        fcomip    %st(1), %st                                   #286.24
        fstp      %st(0)                                        #286.24
        jb        ..B1.119      # Prob 50%                      #286.24
                                # LOE
..B1.118:                       # Preds ..B1.117
..LN2591:
        movq      $0, -632(%rbp)                                #286.49
        jmp       ..B1.136      # Prob 100%                     #286.49
                                # LOE
..B1.119:                       # Preds ..B1.117
..LN2593:
        fldl      -72(%rbp)                                     #287.7
        fstpl     -456(%rbp)                                    #287.7
..LN2595:
        movq      -128(%rbp), %rax                              #292.7
..LN2597:
        movl      (%rax), %eax                                  #292.10
        movl      %eax, -600(%rbp)                              #292.10
        movl      $1, -44(%rbp)                                 #292.10
        movl      -600(%rbp), %eax                              #292.10
        testl     %eax, %eax                                    #292.10
        jle       ..B1.122      # Prob 50%                      #292.10
                                # LOE
..B1.121:                       # Preds ..B1.119 ..B1.121
..LN2599:
        fldl      -312(%rbp)                                    #293.7
        movl      -44(%rbp), %eax                               #293.7
..LN2601:
        movslq    %eax, %rax                                    #293.18
..LN2603:
        movq      -424(%rbp), %rdx                              #293.7
..LN2605:
        fldl      -8(%rdx,%rax,8)                               #293.18
..LN2607:
        fmulp     %st, %st(1)                                   #293.17
        fldl      -304(%rbp)                                    #293.17
        movl      -44(%rbp), %eax                               #293.17
..LN2609:
        movslq    %eax, %rax                                    #293.32
..LN2611:
        movq      48(%rbp), %rdx                                #293.17
..LN2613:
        fldl      -8(%rdx,%rax,8)                               #293.32
..LN2615:
        fmulp     %st, %st(1)                                   #293.31
..LN2617:
        faddp     %st, %st(1)                                   #293.25
        movq      -120(%rbp), %rax                              #293.25
        movl      -44(%rbp), %edx                               #293.25
        addl      (%rax), %edx                                  #293.25
..LN2619:
        movslq    %edx, %rax                                    #293.37
..LN2621:
        movq      64(%rbp), %rdx                                #293.25
..LN2623:
        fldl      -8(%rdx,%rax,8)                               #293.37
..LN2625:
        fsubrp    %st, %st(1)                                   #293.7
        fstpl     -344(%rbp)                                    #293.7
..LN2627:
        fldl      -248(%rbp)                                    #294.7
        movl      -60(%rbp), %eax                               #294.7
        movslq    %eax, %rax                                    #294.7
..LN2629:
        shlq      $3, %rax                                      #294.16
..LN2631:
        movl      -44(%rbp), %edx                               #294.7
..LN2633:
        movslq    %edx, %rdx                                    #294.16
        imulq     %rax, %rdx                                    #294.16
..LN2635:
        addq      -112(%rbp), %rdx                              #294.7
        movl      -60(%rbp), %eax                               #294.7
        movslq    %eax, %rax                                    #294.7
..LN2637:
        shlq      $3, %rax                                      #294.16
        negq      %rax                                          #294.16
..LN2639:
        addq      %rax, %rdx                                    #294.7
        movq      40(%rbp), %rax                                #294.7
        movl      (%rax), %eax                                  #294.7
..LN2641:
        movslq    %eax, %rax                                    #294.16
        fldl      -8(%rdx,%rax,8)                               #294.16
..LN2643:
        fmulp     %st, %st(1)                                   #294.15
        fldl      -376(%rbp)                                    #294.15
        fldl      -344(%rbp)                                    #294.15
..LN2645:
        fmulp     %st, %st(1)                                   #294.34
..LN2647:
        faddp     %st, %st(1)                                   #294.7
..LN2649:
        movl      -44(%rbp), %eax                               #294.3
..LN2651:
        movslq    %eax, %rax                                    #294.7
..LN2653:
        movq      80(%rbp), %rdx                                #294.3
..LN2655:
        fstpl     -8(%rdx,%rax,8)                               #294.7
..LN2657:
        addl      $1, -44(%rbp)                                 #292.10
        movl      -44(%rbp), %eax                               #292.10
        movl      -600(%rbp), %edx                              #292.10
        cmpl      %edx, %eax                                    #292.10
        jle       ..B1.121      # Prob 50%                      #292.10
                                # LOE
..B1.122:                       # Preds ..B1.119 ..B1.121
..LN2659:
        movq      -120(%rbp), %rax                              #295.7
..LN2661:
        movl      (%rax), %eax                                  #295.10
        movl      %eax, -596(%rbp)                              #295.10
        movl      $1, -52(%rbp)                                 #295.10
        movl      -596(%rbp), %eax                              #295.10
        testl     %eax, %eax                                    #295.10
        jle       ..B1.131      # Prob 50%                      #295.10
                                # LOE
..B1.124:                       # Preds ..B1.122 ..B1.128
..LN2663:
        fldl      -88(%rbp)                                     #296.7
        fstpl     -80(%rbp)                                     #296.7
..LN2665:
        movq      -128(%rbp), %rax                              #297.7
..LN2667:
        movl      (%rax), %eax                                  #297.10
        movl      %eax, -588(%rbp)                              #297.10
        movl      $1, -48(%rbp)                                 #297.10
        movl      -588(%rbp), %eax                              #297.10
        testl     %eax, %eax                                    #297.10
        jle       ..B1.127      # Prob 50%                      #297.10
                                # LOE
..B1.126:                       # Preds ..B1.124 ..B1.126
..LN2669:
        movl      -64(%rbp), %eax                               #298.3
        movslq    %eax, %rax                                    #298.3
..LN2671:
        shlq      $3, %rax                                      #298.15
..LN2673:
        movl      -48(%rbp), %edx                               #298.3
..LN2675:
        movslq    %edx, %rdx                                    #298.15
        imulq     %rax, %rdx                                    #298.15
..LN2677:
        addq      -416(%rbp), %rdx                              #298.7
..LN2679:
        movl      -64(%rbp), %eax                               #298.3
        movslq    %eax, %rax                                    #298.3
..LN2681:
        shlq      $3, %rax                                      #298.15
        negq      %rax                                          #298.15
..LN2683:
        addq      %rax, %rdx                                    #298.7
..LN2685:
        movl      -52(%rbp), %eax                               #298.3
..LN2687:
        movslq    %eax, %rax                                    #298.15
        fldl      -8(%rdx,%rax,8)                               #298.15
        movl      -48(%rbp), %eax                               #298.15
..LN2689:
        movslq    %eax, %rax                                    #298.24
..LN2691:
        movq      56(%rbp), %rdx                                #298.15
..LN2693:
        fldl      -8(%rdx,%rax,8)                               #298.24
..LN2695:
        fmulp     %st, %st(1)                                   #298.23
..LN2697:
        fldl      -80(%rbp)                                     #298.3
..LN2699:
        faddp     %st, %st(1)                                   #298.7
        fstpl     -80(%rbp)                                     #298.7
..LN2701:
        addl      $1, -48(%rbp)                                 #297.10
        movl      -48(%rbp), %eax                               #297.10
        movl      -588(%rbp), %edx                              #297.10
        cmpl      %edx, %eax                                    #297.10
        jle       ..B1.126      # Prob 50%                      #297.10
                                # LOE
..B1.127:                       # Preds ..B1.124 ..B1.126
..LN2703:
        fldl      -248(%rbp)                                    #299.7
        movq      -128(%rbp), %rax                              #299.7
        movl      -52(%rbp), %edx                               #299.7
        addl      (%rax), %edx                                  #299.7
..LN2705:
        movslq    %edx, %rax                                    #299.17
..LN2707:
        movq      56(%rbp), %rdx                                #299.7
..LN2709:
        fldl      -8(%rdx,%rax,8)                               #299.17
..LN2711:
        fmulp     %st, %st(1)                                   #299.16
        fldl      -376(%rbp)                                    #299.16
        movl      -52(%rbp), %eax                               #299.16
..LN2713:
        movslq    %eax, %rax                                    #299.30
..LN2715:
        movq      64(%rbp), %rdx                                #299.16
..LN2717:
        fldl      -8(%rdx,%rax,8)                               #299.30
..LN2719:
        fmulp     %st, %st(1)                                   #299.29
..LN2721:
        fsubrp    %st, %st(1)                                   #299.23
        fldl      -80(%rbp)                                     #299.23
..LN2723:
        fmulp     %st, %st(1)                                   #299.7
        fstpl     -344(%rbp)                                    #299.7
..LN2725:
        movq      -128(%rbp), %rax                              #300.7
..LN2727:
        movl      (%rax), %eax                                  #300.10
        movl      %eax, -584(%rbp)                              #300.10
        movl      $1, -44(%rbp)                                 #300.10
        movl      -584(%rbp), %eax                              #300.10
        testl     %eax, %eax                                    #300.10
        jg        ..B1.130      # Prob 50%                      #300.10
                                # LOE
..B1.128:                       # Preds ..B1.127 ..B1.130
..LN2729:
        addl      $1, -52(%rbp)                                 #295.10
        movl      -52(%rbp), %eax                               #295.10
        movl      -596(%rbp), %edx                              #295.10
        cmpl      %edx, %eax                                    #295.10
        jle       ..B1.124      # Prob 50%                      #295.10
        jmp       ..B1.131      # Prob 100%                     #295.10
                                # LOE
..B1.130:                       # Preds ..B1.127 ..B1.130
..LN2731:
        movl      -44(%rbp), %eax                               #301.7
..LN2733:
        movslq    %eax, %rax                                    #301.12
..LN2735:
        movq      80(%rbp), %rdx                                #301.7
..LN2737:
        fldl      -344(%rbp)                                    #301.12
        movl      -64(%rbp), %ecx                               #301.12
        movslq    %ecx, %rcx                                    #301.12
..LN2739:
        shlq      $3, %rcx                                      #301.22
..LN2741:
        movl      -44(%rbp), %ebx                               #301.12
..LN2743:
        movslq    %ebx, %rbx                                    #301.22
        imulq     %rcx, %rbx                                    #301.22
..LN2745:
        addq      -416(%rbp), %rbx                              #301.7
..LN2747:
        movl      -64(%rbp), %ecx                               #301.12
        movslq    %ecx, %rcx                                    #301.12
..LN2749:
        shlq      $3, %rcx                                      #301.22
        negq      %rcx                                          #301.22
..LN2751:
        addq      %rcx, %rbx                                    #301.7
..LN2753:
        movl      -52(%rbp), %ecx                               #301.12
..LN2755:
        movslq    %ecx, %rcx                                    #301.22
        fldl      -8(%rbx,%rcx,8)                               #301.22
..LN2757:
        fmulp     %st, %st(1)                                   #301.21
..LN2759:
        fldl      -8(%rdx,%rax,8)                               #301.12
..LN2761:
        faddp     %st, %st(1)                                   #301.7
..LN2763:
        movl      -44(%rbp), %eax                               #301.3
..LN2765:
        movslq    %eax, %rax                                    #301.7
..LN2767:
        movq      80(%rbp), %rdx                                #301.3
..LN2769:
        fstpl     -8(%rdx,%rax,8)                               #301.7
..LN2771:
        addl      $1, -44(%rbp)                                 #300.10
        movl      -44(%rbp), %eax                               #300.10
        movl      -584(%rbp), %edx                              #300.10
        cmpl      %edx, %eax                                    #300.10
        jle       ..B1.130      # Prob 50%                      #300.10
        jmp       ..B1.128      # Prob 100%                     #300.10
                                # LOE
..B1.131:                       # Preds ..B1.122 ..B1.128
..LN2773:
        fldl      -88(%rbp)                                     #302.7
        fstpl     -464(%rbp)                                    #302.7
..LN2775:
        fldl      -88(%rbp)                                     #303.7
        fstpl     -360(%rbp)                                    #303.7
..LN2777:
        movq      -128(%rbp), %rax                              #304.7
..LN2779:
        movl      (%rax), %eax                                  #304.10
        movl      %eax, -592(%rbp)                              #304.10
        movl      $1, -44(%rbp)                                 #304.10
        movl      -592(%rbp), %eax                              #304.10
        testl     %eax, %eax                                    #304.10
        jle       ..B1.134      # Prob 50%                      #304.10
                                # LOE
..B1.133:                       # Preds ..B1.131 ..B1.133
..LN2781:
        movl      -44(%rbp), %eax                               #305.7
..LN2783:
        movslq    %eax, %rax                                    #305.13
..LN2785:
        movq      80(%rbp), %rdx                                #305.7
..LN2787:
        fldl      -8(%rdx,%rax,8)                               #305.17
        fmul      %st(0), %st                                   #305.17
..LN2789:
        fldl      -464(%rbp)                                    #305.7
        faddp     %st, %st(1)                                   #305.7
        fstpl     -464(%rbp)                                    #305.7
..LN2791:
        movl      -44(%rbp), %eax                               #306.3
..LN2793:
        movslq    %eax, %rax                                    #306.13
..LN2795:
        movq      48(%rbp), %rdx                                #306.3
..LN2797:
        fldl      -8(%rdx,%rax,8)                               #306.13
        movl      -44(%rbp), %eax                               #306.13
..LN2799:
        movslq    %eax, %rax                                    #306.18
..LN2801:
        movq      80(%rbp), %rdx                                #306.13
..LN2803:
        fldl      -8(%rdx,%rax,8)                               #306.18
..LN2805:
        fmulp     %st, %st(1)                                   #306.17
..LN2807:
        fldl      -360(%rbp)                                    #306.3
..LN2809:
        faddp     %st, %st(1)                                   #306.7
        fstpl     -360(%rbp)                                    #306.7
..LN2811:
        addl      $1, -44(%rbp)                                 #304.10
        movl      -44(%rbp), %eax                               #304.10
        movl      -592(%rbp), %edx                              #304.10
        cmpl      %edx, %eax                                    #304.10
        jle       ..B1.133      # Prob 50%                      #304.10
                                # LOE
..B1.134:                       # Preds ..B1.131 ..B1.133
..LN2813:
        fldl      -368(%rbp)                                    #307.7
        fldl      -464(%rbp)                                    #307.7
..LN2815:
        fmulp     %st, %st(1)                                   #307.15
        fldl      -360(%rbp)                                    #307.15
        fldl      -360(%rbp)                                    #307.15
..LN2817:
        fmulp     %st, %st(1)                                   #307.21
..LN2819:
        fsubrp    %st, %st(1)                                   #307.7
        fstpl     -336(%rbp)                                    #307.7
..LN2821:
        fldl      _2il0floatpacket.7(%rip)                      #308.28
..LN2823:
        fldl      -368(%rbp)                                    #308.7
..LN2825:
        fmulp     %st, %st(1)                                   #308.28
        fldl      -464(%rbp)                                    #308.28
..LN2827:
        fmulp     %st, %st(1)                                   #308.31
..LN2829:
        fldl      -336(%rbp)                                    #308.7
..LN2831:
        fxch      %st(1)                                        #308.17
        fstpl     -216(%rbp)                                    #308.17
        fldl      -216(%rbp)                                    #308.17
        fcomip    %st(1), %st                                   #308.17
        fstp      %st(0)                                        #308.17
        ja        ..B1.136      # Prob 50%                      #308.17
        jp        ..B1.136      # Prob 0%                       #308.17
                                # LOE
..B1.135:                       # Preds ..B1.134
..LN2833:
        movq      $0, -880(%rbp)                                #308.36
        jmp       ..B1.30       # Prob 100%                     #308.36
                                # LOE
..B1.136:                       # Preds ..B1.134 ..B1.118 ..B1.114
..LN2835:
        movq      24(%rbp), %rax                                #312.3
..LN2837:
        movl      (%rax), %eax                                  #312.10
        movl      %eax, -192(%rbp)                              #312.10
        movl      $1, -52(%rbp)                                 #312.10
        movl      -192(%rbp), %eax                              #312.10
        testl     %eax, %eax                                    #312.10
        jle       ..B1.141      # Prob 50%                      #312.10
                                # LOE
..B1.138:                       # Preds ..B1.136 ..B1.139
..LN2839:
        movl      -52(%rbp), %eax                               #313.7
        movslq    %eax, %rax                                    #313.7
        movq      56(%rbp), %rdx                                #313.7
        fldl      -88(%rbp)                                     #313.7
        fstpl     -8(%rdx,%rax,8)                               #313.7
..LN2841:
        movl      $1, -48(%rbp)                                 #314.10
        jmp       ..B1.140      # Prob 100%                     #314.10
                                # LOE
..B1.139:                       # Preds ..B1.140
..LN2843:
        addl      $1, -52(%rbp)                                 #312.10
        movl      -52(%rbp), %eax                               #312.10
        movl      -192(%rbp), %edx                              #312.10
        cmpl      %edx, %eax                                    #312.10
        jle       ..B1.138      # Prob 50%                      #312.10
        jmp       ..B1.141      # Prob 100%                     #312.10
                                # LOE
..B1.140:                       # Preds ..B1.140 ..B1.138
..LN2845:
        movl      -52(%rbp), %eax                               #315.7
..LN2847:
        movslq    %eax, %rax                                    #315.12
..LN2849:
        movq      56(%rbp), %rdx                                #315.7
..LN2851:
        movl      -60(%rbp), %ecx                               #315.12
        movslq    %ecx, %rcx                                    #315.12
..LN2853:
        shlq      $3, %rcx                                      #315.17
..LN2855:
        movl      -48(%rbp), %ebx                               #315.12
..LN2857:
        movslq    %ebx, %rbx                                    #315.17
        imulq     %rcx, %rbx                                    #315.17
..LN2859:
        addq      88(%rbp), %rbx                                #315.7
..LN2861:
        movl      -60(%rbp), %ecx                               #315.12
        movslq    %ecx, %rcx                                    #315.12
..LN2863:
        shlq      $3, %rcx                                      #315.17
        negq      %rcx                                          #315.17
..LN2865:
        addq      %rcx, %rbx                                    #315.7
..LN2867:
        movl      -52(%rbp), %ecx                               #315.12
..LN2869:
        movslq    %ecx, %rcx                                    #315.17
        fldl      -8(%rbx,%rcx,8)                               #315.17
        movl      -48(%rbp), %ecx                               #315.17
        movslq    %ecx, %rcx                                    #315.17
..LN2871:
        fldl      -784(%rbp,%rcx,8)                             #315.27
..LN2873:
        fmulp     %st, %st(1)                                   #315.26
..LN2875:
        fldl      -8(%rdx,%rax,8)                               #315.12
..LN2877:
        faddp     %st, %st(1)                                   #315.7
..LN2879:
        movl      -52(%rbp), %eax                               #315.3
..LN2881:
        movslq    %eax, %rax                                    #315.7
..LN2883:
        movq      56(%rbp), %rdx                                #315.3
..LN2885:
        fstpl     -8(%rdx,%rax,8)                               #315.7
..LN2887:
        addl      $1, -48(%rbp)                                 #314.10
        movl      -48(%rbp), %eax                               #314.10
        cmpl      $5, %eax                                      #314.10
        jle       ..B1.140      # Prob 50%                      #314.10
        jmp       ..B1.139      # Prob 100%                     #314.10
                                # LOE
..B1.141:                       # Preds ..B1.136 ..B1.139
..LN2889:
        movq      32(%rbp), %rax                                #316.7
        movl      (%rax), %eax                                  #316.7
..LN2891:
        movslq    %eax, %rax                                    #316.18
..LN2893:
        movq      64(%rbp), %rdx                                #316.7
..LN2895:
        fldl      -8(%rdx,%rax,8)                               #316.18
        fldl      -408(%rbp)                                    #316.18
..LN2897:
        faddp     %st, %st(1)                                   #316.7
        movq      32(%rbp), %rax                                #316.7
        movl      (%rax), %eax                                  #316.7
        movslq    %eax, %rax                                    #316.7
        movq      64(%rbp), %rdx                                #316.7
        fstpl     -8(%rdx,%rax,8)                               #316.7
..LN2899:
        movq      $0, -448(%rbp)                                #317.7
..LN2901:
        movq      -432(%rbp), %rbx                              #318.7
..___tag_value_bigden_.13:                                      #
        leave                                                   #318.7
..___tag_value_bigden_.15:                                      #
        ret                                                     #318.7
        .align    2,0x90
..___tag_value_bigden_.16:                                      #
                                # LOE
# mark_end;
	.type	bigden_,@function
	.size	bigden_,.-bigden_
.LNbigden_:
	.data
# -- End  bigden_
	.section .rodata, "a"
	.align 8
	.align 8
_2il0floatpacket.1:
	.long	0x00000000,0x3fe00000
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
_2il0floatpacket.2:
	.long	0x00000000,0x3fd00000
	.type	_2il0floatpacket.2,@object
	.size	_2il0floatpacket.2,8
_2il0floatpacket.3:
	.long	0x00000000,0x40000000
	.type	_2il0floatpacket.3,@object
	.size	_2il0floatpacket.3,8
_2il0floatpacket.4:
	.long	0x00000000,0x40200000
	.type	_2il0floatpacket.4,@object
	.size	_2il0floatpacket.4,8
_2il0floatpacket.5:
	.long	0x7ae147ae,0x3fefae14
	.type	_2il0floatpacket.5,@object
	.size	_2il0floatpacket.5,8
_2il0floatpacket.6:
	.long	0x9999999a,0x3ff19999
	.type	_2il0floatpacket.6,@object
	.size	_2il0floatpacket.6,8
_2il0floatpacket.7:
	.long	0xe2308c3a,0x3e45798e
	.type	_2il0floatpacket.7,@object
	.size	_2il0floatpacket.7,8
_2il0floatpacket.8:
	.long	0x00000000,0x3ff00000
	.type	_2il0floatpacket.8,@object
	.size	_2il0floatpacket.8,8
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .debug_info
	.section .debug_info
.debug_info_seg:
	.align 1
	.4byte 0x000005df
	.2byte 0x0002
	.4byte .debug_abbrev_seg
	.byte 0x08
.DWinfo0:
//	DW_TAG_compile_unit:
	.byte 0x01
//	DW_AT_comp_dir:
	.8byte 0x2f63732f73666e2f
	.8byte 0x6c682f736b736964
	.8byte 0x6d2f353130305f70
	.8byte 0x2f656f727473796e
	.8byte 0x6c682d34702f3470
	.8byte 0x2f736c6f6f742f70
	.8byte 0x77656e2f6174656d
	.8byte 0x006372732f616f75
//	DW_AT_language:
	.byte 0x0e
//	DW_AT_name:
	.8byte 0x662e6e6564676962
	.byte 0x00
//	DW_AT_producer:
	.8byte 0x5228206c65746e49
	.8byte 0x617274726f462029
	.8byte 0x6c69706d6f43206e
	.8byte 0x7365786946207265
	.8byte 0x527365676e615220
	.8byte 0x0a65766974616c65
	.byte 0x00
//	DW_AT_stmt_list:
	.4byte .debug_line_seg
.DWinfo2:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x00
//	DW_AT_encoding:
	.byte 0x05
//	DW_AT_name:
	.4byte 0x64696f76
	.byte 0x00
.DWinfo1:
//	DW_TAG_subprogram:
	.byte 0x03
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x12
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_inline:
	.byte 0x00
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x0000008b
//	DW_AT_prototyped:
	.byte 0x00
//	DW_AT_name:
	.4byte 0x64676962
	.2byte 0x6e65
	.byte 0x00
//	DW_AT_low_pc:
	.8byte bigden_
//	DW_AT_high_pc:
	.8byte .LNbigden_
//	DW_AT_external:
	.byte 0x01
//	DW_AT_sibling:
	.4byte 0x000004fb
.DWinfo3:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x1a
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006e
//	DW_AT_location:
	.4byte 0x7f807604
	.byte 0x06
.DWinfo4:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x1c
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x0074706e
//	DW_AT_location:
	.4byte 0x7f887604
	.byte 0x06
.DWinfo5:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x20
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000509
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74706f78
	.byte 0x00
//	DW_AT_location:
	.4byte 0x7cd87604
	.byte 0x06
.DWinfo6:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x25
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000521
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00747078
//	DW_AT_location:
	.4byte 0x7ce07604
	.byte 0x06
.DWinfo7:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x29
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000535
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74616d62
	.byte 0x00
//	DW_AT_location:
	.4byte 0x7f907604
	.byte 0x06
.DWinfo8:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x2e
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000549
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74616d7a
	.byte 0x00
//	DW_AT_location:
	.4byte 0x7f987604
	.byte 0x06
.DWinfo9:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x33
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x007a6469
//	DW_AT_location:
	.4byte 0x06107603
.DWinfo10:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x37
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6d69646e
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06187603
.DWinfo11:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x3c
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74706f6b
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06207603
.DWinfo12:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x09
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x77656e6b
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06287603
.DWinfo13:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x0e
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x0000055d
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0064
//	DW_AT_location:
	.4byte 0x06307603
.DWinfo14:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x10
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x0000056a
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0077
//	DW_AT_location:
	.4byte 0x06387603
.DWinfo15:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x12
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000577
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x67616c76
	.byte 0x00
//	DW_AT_location:
	.4byte 0x00c07604
	.byte 0x06
.DWinfo16:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x17
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x61746562
	.byte 0x00
//	DW_AT_location:
	.4byte 0x00c87604
	.byte 0x06
.DWinfo17:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x1c
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000584
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0073
//	DW_AT_location:
	.4byte 0x00d07604
	.byte 0x06
.DWinfo18:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x1e
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000591
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x63657677
	.byte 0x00
//	DW_AT_location:
	.4byte 0x00d87604
	.byte 0x06
.DWinfo19:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x23
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000005a5
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x646f7270
	.byte 0x00
//	DW_AT_location:
	.4byte 0x00e07604
	.byte 0x06
.DWinfo20:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.2byte 0x0112
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00756174
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7e887603
.DWinfo21:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xf9
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x70657473
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7df07603
.DWinfo22:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xeb
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6f6d7573
	.2byte 0x646c
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7df87603
.DWinfo23:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xe5
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6c676e61
	.2byte 0x0065
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7de87603
.DWinfo24:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xe1
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x7569
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.4byte 0x7eec7603
.DWinfo25:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xe0
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x76617369
	.2byte 0x0065
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.4byte 0x7ee87603
.DWinfo26:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xdf
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6d6e6564
	.2byte 0x7861
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7fb87603
.DWinfo27:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0xde
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6f6e6564
	.2byte 0x646c
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7de07603
.DWinfo28:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x95
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006d7573
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7fb07603
.DWinfo29:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x90
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x776e
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x5c
.DWinfo30:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x8f
//	DW_AT_decl_column:
	.byte 0x0e
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x636a
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x58
.DWinfo31:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x86
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x7069
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.4byte 0x7ed87603
.DWinfo32:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x7b
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706d6574
	.2byte 0x0063
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7dd87603
.DWinfo33:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x6d
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706d6574
	.2byte 0x0062
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7dd07603
.DWinfo34:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x6c
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706d6574
	.2byte 0x0061
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7dc87603
.DWinfo35:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x64
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74706f78
	.2byte 0x0073
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7dc07603
.DWinfo36:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x63
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74706f78
	.2byte 0x0064
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7db87603
.DWinfo37:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x5c
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x736e6564
	.2byte 0x7661
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7cb87603
.DWinfo38:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x5b
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x72657469
	.2byte 0x0063
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.4byte 0x7ec47603
.DWinfo39:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x5a
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x65647373
	.2byte 0x006e
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7db07603
.DWinfo40:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x4c
//	DW_AT_decl_column:
	.byte 0x0f
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x66666964
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x79a87603
.DWinfo41:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x4a
//	DW_AT_decl_column:
	.byte 0x0f
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x65747373
	.2byte 0x706d
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x79a07603
.DWinfo42:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x49
//	DW_AT_decl_column:
	.byte 0x0f
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x65747364
	.2byte 0x706d
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x79987603
.DWinfo43:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x46
//	DW_AT_decl_column:
	.byte 0x0b
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x73657464
	.2byte 0x0074
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7b987603
.DWinfo44:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x45
//	DW_AT_decl_column:
	.byte 0x0b
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x7661736b
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.4byte 0x7bd47603
.DWinfo45:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x3e
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0069
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x54
.DWinfo46:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x3d
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74706f78
	.2byte 0x7173
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7da07603
.DWinfo47:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x3c
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x7373
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7cb07603
.DWinfo48:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x3b
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x7364
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7d987603
.DWinfo49:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x3a
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x6464
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7d907603
.DWinfo50:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x33
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x68706c61
	.2byte 0x0061
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7d887603
.DWinfo51:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x2f
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706d6574
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7da87603
.DWinfo52:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x2e
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006a
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x50
.DWinfo53:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x2c
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006b
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x4c
.DWinfo54:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x27
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6d74706e
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000004fb
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x48
.DWinfo55:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x26
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706f7774
	.2byte 0x0069
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7d807603
.DWinfo56:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x25
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6f72657a
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7fa87603
.DWinfo57:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x24
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006f7774
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7cf87603
.DWinfo58:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x23
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x72617571
	.2byte 0x0074
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7cf07603
.DWinfo59:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x22
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00656e6f
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7ce87603
.DWinfo60:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x21
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x666c6168
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_location:
	.4byte 0x7fa07603
.DWinfo61:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x06
//	DW_AT_decl_column:
	.byte 0x21
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00726170
//	DW_AT_type:
	.4byte 0x000005b9
//	DW_AT_location:
	.4byte 0x79f87603
.DWinfo62:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x06
//	DW_AT_decl_column:
	.byte 0x18
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x656e6564
	.2byte 0x0078
//	DW_AT_type:
	.4byte 0x000005c7
//	DW_AT_location:
	.4byte 0x7ac07603
.DWinfo63:
//	DW_TAG_variable:
	.byte 0x06
//	DW_AT_decl_line:
	.byte 0x06
//	DW_AT_decl_column:
	.byte 0x11
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006e6564
//	DW_AT_type:
	.4byte 0x000005d5
//	DW_AT_location:
	.4byte 0x79b07603
	.byte 0x00
.DWinfo64:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x04
//	DW_AT_encoding:
	.byte 0x05
//	DW_AT_name:
	.8byte 0x2852454745544e49
	.2byte 0x2934
	.byte 0x00
.DWinfo65:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x00000516
.DWinfo66:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo67:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x08
//	DW_AT_encoding:
	.byte 0x04
//	DW_AT_name:
	.8byte 0x002938284c414552
.DWinfo68:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x00000535
.DWinfo69:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7be87604
	.byte 0x06
.DWinfo70:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo71:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x00000549
.DWinfo72:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7bf87604
	.byte 0x06
.DWinfo73:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo74:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x0000055d
.DWinfo75:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7c887604
	.byte 0x06
.DWinfo76:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo77:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x0000056a
.DWinfo78:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo79:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x00000577
.DWinfo80:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo81:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x00000584
.DWinfo82:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo83:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x00000591
.DWinfo84:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo85:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x000005a5
.DWinfo86:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7c987604
	.byte 0x06
.DWinfo87:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo88:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x000005b9
.DWinfo89:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7ca87604
	.byte 0x06
.DWinfo90:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo91:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x000005c7
.DWinfo92:
//	DW_TAG_subrange_type:
	.byte 0x0a
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.byte 0x09
	.byte 0x00
.DWinfo93:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
//	DW_AT_sibling:
	.4byte 0x000005d5
.DWinfo94:
//	DW_TAG_subrange_type:
	.byte 0x0a
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.byte 0x09
	.byte 0x00
.DWinfo95:
//	DW_TAG_array_type:
	.byte 0x0b
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000516
.DWinfo96:
//	DW_TAG_subrange_type:
	.byte 0x0a
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.byte 0x09
	.byte 0x00
	.byte 0x00
	.byte 0x00
	.byte 0x00
	.byte 0x00
// -- Begin DWARF2 SEGMENT .debug_line
	.section .debug_line
.debug_line_seg:
	.align 1
	.4byte 0x00000dab
	.2byte 0x0002
	.4byte 0x00000021
	.byte 0x01
	.byte 0x01
	.byte 0xff
	.byte 0x04
	.byte 0x0a
	.8byte 0x0000000101010100
	.byte 0x01
	.byte 0x00
	.8byte 0x662e6e6564676962
	.byte 0x00
	.8byte 0x4ebc05c0a3cde100
	.byte 0x00
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN3
	.2byte 0x2003
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN5
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN7
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN9
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN11
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN13
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN19
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN25
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN29
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN37
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN39
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN41
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN61
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN67
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN71
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN73
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN113
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN115
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN121
	.2byte 0x0703
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN123
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN125
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN127
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN129
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN133
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN143
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN167
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN185
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN195
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN207
	.2byte 0x7b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN209
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN223
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN225
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN231
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN235
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN239
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN241
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN243
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN247
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN273
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN285
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN293
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN295
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN303
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN305
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN311
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN313
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN315
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN317
	.2byte 0x7103
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN319
	.2byte 0x1003
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN323
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN357
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN359
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN367
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN369
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN371
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN373
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN377
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN379
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN381
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN385
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN407
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN425
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN445
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN447
	.2byte 0x0703
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN453
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN459
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN473
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN479
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN485
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN487
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN489
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN491
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN495
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN497
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN501
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN503
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN505
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN507
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN511
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN539
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN565
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN597
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN599
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN611
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN613
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN615
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN625
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN643
	.2byte 0x7403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN645
	.2byte 0x0d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN649
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN651
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN653
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN659
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN665
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN667
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN679
	.2byte 0x7a03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN681
	.2byte 0x0a03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN683
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN685
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN693
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN697
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN713
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN715
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN717
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN719
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN723
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN767
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN769
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN775
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN779
	.2byte 0x7b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN781
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN839
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN841
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN845
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN849
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN851
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN855
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN899
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN901
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN937
	.2byte 0x7c03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN939
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN943
	.2byte 0x6d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN945
	.2byte 0x1403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN947
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN949
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN993
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN995
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1011
	.2byte 0x7c03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1013
	.2byte 0x0803
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1017
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1019
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1021
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1061
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1067
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1069
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1077
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1131
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1191
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1251
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1261
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1271
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1325
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1385
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1445
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1455
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1465
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1519
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1531
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1585
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1645
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1653
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1663
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1723
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1731
	.2byte 0x6903
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1733
	.2byte 0x1b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1735
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1737
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1765
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1771
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1773
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1785
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1811
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1839
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1867
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1879
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1889
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1915
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1943
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1971
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1983
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1993
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2019
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2035
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2061
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2101
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2115
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2153
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2171
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2173
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2175
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2177
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2179
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2185
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2187
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2189
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2197
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2201
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2205
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2207
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2221
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2239
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2241
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2243
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2245
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2247
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2259
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2261
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2269
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2271
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2273
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2275
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2283
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2285
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2287
	.2byte 0x6e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2289
	.2byte 0x1303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2295
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2301
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2303
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2307
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2309
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2311
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2321
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2333
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2337
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2341
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2343
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2357
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2375
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2377
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2379
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2381
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2383
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2393
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2405
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2407
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2411
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2413
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2415
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2417
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2459
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2461
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2467
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2469
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2471
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2473
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2477
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2499
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2515
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2525
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2543
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2563
	.2byte 0x7b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2565
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2571
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2577
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2593
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2595
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2599
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2627
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2657
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2659
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2663
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2665
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2669
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2701
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2703
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2725
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2729
	.2byte 0x7b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2731
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2771
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2773
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2775
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2777
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2781
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2791
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2811
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2813
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2821
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2835
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2839
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2841
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2843
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2845
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2887
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2889
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2899
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN2901
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte .LNbigden_
	.byte 0x00
	.byte 0x01
	.byte 0x01
// -- Begin DWARF2 SEGMENT .debug_abbrev
	.section .debug_abbrev
.debug_abbrev_seg:
	.align 1
	.byte 0x01
	.byte 0x11
	.byte 0x01
	.byte 0x1b
	.byte 0x08
	.byte 0x13
	.byte 0x0b
	.byte 0x03
	.byte 0x08
	.byte 0x25
	.byte 0x08
	.byte 0x10
	.byte 0x06
	.2byte 0x0000
	.byte 0x02
	.byte 0x24
	.byte 0x00
	.byte 0x0b
	.byte 0x0b
	.byte 0x3e
	.byte 0x0b
	.byte 0x03
	.byte 0x08
	.2byte 0x0000
	.byte 0x03
	.byte 0x2e
	.byte 0x01
	.byte 0x3b
	.byte 0x0b
	.byte 0x39
	.byte 0x0b
	.byte 0x3a
	.byte 0x0b
	.byte 0x20
	.byte 0x0b
	.byte 0x32
	.byte 0x0b
	.byte 0x49
	.byte 0x13
	.byte 0x27
	.byte 0x0c
	.byte 0x03
	.byte 0x08
	.byte 0x11
	.byte 0x01
	.byte 0x12
	.byte 0x01
	.byte 0x3f
	.byte 0x0c
	.byte 0x01
	.byte 0x13
	.2byte 0x0000
	.byte 0x04
	.byte 0x05
	.byte 0x00
	.byte 0x3b
	.byte 0x0b
	.byte 0x39
	.byte 0x0b
	.byte 0x3a
	.byte 0x0b
	.byte 0x49
	.byte 0x13
	.byte 0x4b
	.byte 0x0c
	.byte 0x03
	.byte 0x08
	.byte 0x02
	.byte 0x0a
	.2byte 0x0000
	.byte 0x05
	.byte 0x34
	.byte 0x00
	.byte 0x3b
	.byte 0x05
	.byte 0x39
	.byte 0x0b
	.byte 0x3a
	.byte 0x0b
	.byte 0x32
	.byte 0x0b
	.byte 0x03
	.byte 0x08
	.byte 0x49
	.byte 0x13
	.byte 0x02
	.byte 0x0a
	.2byte 0x0000
	.byte 0x06
	.byte 0x34
	.byte 0x00
	.byte 0x3b
	.byte 0x0b
	.byte 0x39
	.byte 0x0b
	.byte 0x3a
	.byte 0x0b
	.byte 0x32
	.byte 0x0b
	.byte 0x03
	.byte 0x08
	.byte 0x49
	.byte 0x13
	.byte 0x02
	.byte 0x0a
	.2byte 0x0000
	.byte 0x07
	.byte 0x01
	.byte 0x01
	.byte 0x09
	.byte 0x0b
	.byte 0x49
	.byte 0x13
	.byte 0x01
	.byte 0x13
	.2byte 0x0000
	.byte 0x08
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.2byte 0x0000
	.byte 0x09
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.byte 0x2f
	.byte 0x0a
	.2byte 0x0000
	.byte 0x0a
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.byte 0x2f
	.byte 0x0d
	.2byte 0x0000
	.byte 0x0b
	.byte 0x01
	.byte 0x01
	.byte 0x09
	.byte 0x0b
	.byte 0x49
	.byte 0x13
	.2byte 0x0000
	.byte 0x00
// -- Begin DWARF2 SEGMENT .debug_frame
	.section .debug_frame
.debug_frame_seg:
	.align 1
	.4byte 0x00000014
	.4byte 0xffffffff
	.byte 0x01
	.byte 0x00
	.byte 0x01
	.byte 0x78
	.byte 0x10
	.4byte 0x9008070c
	.byte 0x01
	.4byte 0x00000000
	.2byte 0x0000
	.4byte 0x0000002c
	.4byte .debug_frame_seg
	.8byte ..___tag_value_bigden_.2
	.8byte ..___tag_value_bigden_.16-..___tag_value_bigden_.2
	.byte 0x04
	.4byte ..___tag_value_bigden_.9-..___tag_value_bigden_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_bigden_.12-..___tag_value_bigden_.9
	.byte 0x83
	.byte 0x38
	.4byte 0x00000000
	.2byte 0x0000
	.byte 0x00
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
	.4byte 0x00000014
	.4byte 0x00000000
	.byte 0x01
	.byte 0x00
	.byte 0x01
	.byte 0x78
	.byte 0x10
	.4byte 0x9008070c
	.byte 0x01
	.4byte 0x00000000
	.2byte 0x0000
	.4byte 0x0000002c
	.4byte 0x0000001c
	.8byte ..___tag_value_bigden_.2
	.8byte ..___tag_value_bigden_.16-..___tag_value_bigden_.2
	.byte 0x04
	.4byte ..___tag_value_bigden_.9-..___tag_value_bigden_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_bigden_.12-..___tag_value_bigden_.9
	.byte 0x83
	.byte 0x38
	.4byte 0x00000000
	.2byte 0x0000
	.byte 0x00
	.section .text
.LNDBG_TXe:
# End
