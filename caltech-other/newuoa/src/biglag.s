	.section .text
.LNDBG_TX:
# -- Machine type EFI2
# mark_description "Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 10.0    Build 20070809 %s";
# mark_description "-g -S -W1 -recursive -reentrancy threaded";
	.file "biglag.f"
	.data
	.text
..TXTST0:
# -- Begin  biglag_
# mark_begin;
       .align    2,0x90
	.globl biglag_
biglag_:
# parameter 1(n): %rdi
# parameter 2(npt): %rsi
# parameter 3(xopt): %rdx
# parameter 4(xpt): %rcx
# parameter 5(bmat): %r8
# parameter 6(zmat): %r9
# parameter 7(idz): 16 + %rbp
# parameter 8(ndim): 24 + %rbp
# parameter 9(knew): 32 + %rbp
# parameter 10(delta): 40 + %rbp
# parameter 11(d): 48 + %rbp
# parameter 12(alpha): 56 + %rbp
# parameter 13(hcol): 64 + %rbp
# parameter 14(gc): 72 + %rbp
# parameter 15(gd): 80 + %rbp
# parameter 16(s): 88 + %rbp
# parameter 17(w): 96 + %rbp
..B1.1:                         # Preds ..B1.0
..___tag_value_biglag_.2:                                       #
..LN1:
        pushq     %rbp                                          #1.18
        movq      %rsp, %rbp                                    #1.18
..___tag_value_biglag_.9:                                       #
        subq      $528, %rsp                                    #1.18
        movq      %rbx, -184(%rbp)                              #1.18
..___tag_value_biglag_.12:                                      #
        movq      %rdi, -136(%rbp)                              #1.18
        movq      %rsi, -368(%rbp)                              #1.18
        movq      %rdx, -360(%rbp)                              #1.18
        movq      %rcx, -352(%rbp)                              #1.18
        movq      %r8, -344(%rbp)                               #1.18
        movq      %r9, -336(%rbp)                               #1.18
        movq      -368(%rbp), %rax                              #1.18
        movl      (%rax), %eax                                  #1.18
        movl      %eax, -80(%rbp)                               #1.18
        movq      24(%rbp), %rax                                #1.18
        movl      (%rax), %eax                                  #1.18
        movl      %eax, -76(%rbp)                               #1.18
        movl      -80(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -328(%rbp)                              #1.18
        movq      -328(%rbp), %rax                              #1.18
        movq      %rax, -320(%rbp)                              #1.18
        movl      -76(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -312(%rbp)                              #1.18
        movq      -312(%rbp), %rax                              #1.18
        movq      %rax, -304(%rbp)                              #1.18
        movl      -80(%rbp), %eax                               #1.18
        movslq    %eax, %rax                                    #1.18
        movq      %rax, -296(%rbp)                              #1.18
        movq      -296(%rbp), %rax                              #1.18
        movq      %rax, -288(%rbp)                              #1.18
..LN3:
        fldl      _2il0floatpacket.1(%rip)                      #26.7
        fstpl     -280(%rbp)                                    #26.7
..LN5:
        movq      $0x3ff0000000000000, %rax                     #27.7
        movq      %rax, -272(%rbp)                              #27.7
..LN7:
        movq      $0, -128(%rbp)                                #28.7
..LN9:
        movsd     -272(%rbp), %xmm0                             #29.19
        movl      $1, %eax                                      #29.19
        call      atan                                          #29.19
                                # LOE xmm0
..B1.83:                        # Preds ..B1.1
        movsd     %xmm0, -200(%rbp)                             #29.19
                                # LOE
..B1.2:                         # Preds ..B1.83
..LN11:
        fldl      _2il0floatpacket.2(%rip)                      #29.18
..LN13:
        fldl      -200(%rbp)                                    #29.7
        fmulp     %st, %st(1)                                   #29.7
        fstpl     -264(%rbp)                                    #29.7
..LN15:
        movq      40(%rbp), %rax                                #30.7
        fldl      (%rax)                                        #30.7
        movq      40(%rbp), %rax                                #30.7
        fldl      (%rax)                                        #30.7
        fmulp     %st, %st(1)                                   #30.7
        fstpl     -256(%rbp)                                    #30.7
..LN17:
        movq      -368(%rbp), %rax                              #31.7
        movq      -136(%rbp), %rdx                              #31.7
        movl      (%rdx), %edx                                  #31.7
..LN19:
        negl      %edx                                          #31.15
..LN21:
        movl      (%rax), %eax                                  #31.7
        lea       -1(%rax,%rdx), %eax                           #31.7
        movl      %eax, -72(%rbp)                               #31.7
..LN23:
        movl      $0, -24(%rbp)                                 #36.7
..LN25:
        movq      -368(%rbp), %rax                              #37.7
..LN27:
        movl      (%rax), %eax                                  #37.10
        movl      %eax, -68(%rbp)                               #37.10
        movl      $1, -64(%rbp)                                 #37.10
        movl      -68(%rbp), %eax                               #37.10
        testl     %eax, %eax                                    #37.10
        jle       ..B1.5        # Prob 50%                      #37.10
                                # LOE
..B1.4:                         # Preds ..B1.2 ..B1.4
..LN29:
        movl      -64(%rbp), %eax                               #38.4
..LN31:
        movslq    %eax, %rax                                    #38.7
..LN33:
        movq      64(%rbp), %rdx                                #38.4
..LN35:
        fldl      -128(%rbp)                                    #38.7
        fstpl     -8(%rdx,%rax,8)                               #38.7
..LN37:
        addl      $1, -64(%rbp)                                 #37.10
        movl      -64(%rbp), %eax                               #37.10
        movl      -68(%rbp), %edx                               #37.10
        cmpl      %edx, %eax                                    #37.10
        jle       ..B1.4        # Prob 50%                      #37.10
                                # LOE
..B1.5:                         # Preds ..B1.2 ..B1.4
..LN39:
        movl      -72(%rbp), %eax                               #39.10
        movl      %eax, -60(%rbp)                               #39.10
        movl      $1, -56(%rbp)                                 #39.10
        movl      -60(%rbp), %eax                               #39.10
        testl     %eax, %eax                                    #39.10
        jle       ..B1.13       # Prob 50%                      #39.10
                                # LOE
..B1.7:                         # Preds ..B1.5 ..B1.10
..LN41:
        movl      -80(%rbp), %eax                               #40.7
        movslq    %eax, %rax                                    #40.7
..LN43:
        shlq      $3, %rax                                      #40.12
..LN45:
        movl      -56(%rbp), %edx                               #40.7
..LN47:
        movslq    %edx, %rdx                                    #40.12
        imulq     %rax, %rdx                                    #40.12
..LN49:
        addq      -336(%rbp), %rdx                              #40.7
..LN51:
        movl      -80(%rbp), %eax                               #40.7
        movslq    %eax, %rax                                    #40.7
..LN53:
        shlq      $3, %rax                                      #40.12
        negq      %rax                                          #40.12
..LN55:
        addq      %rax, %rdx                                    #40.7
        movq      32(%rbp), %rax                                #40.7
        movl      (%rax), %eax                                  #40.7
..LN57:
        movslq    %eax, %rax                                    #40.12
..LN59:
        fldl      -8(%rdx,%rax,8)                               #40.7
        fstpl     -112(%rbp)                                    #40.7
..LN61:
        movq      16(%rbp), %rax                                #41.7
        movl      -56(%rbp), %edx                               #41.7
        movl      (%rax), %eax                                  #41.7
..LN63:
        cmpl      %eax, %edx                                    #41.13
        jge       ..B1.9        # Prob 50%                      #41.13
                                # LOE
..B1.8:                         # Preds ..B1.7
..LN65:
        fldl      -112(%rbp)                                    #41.23
        fchs                                                    #41.23
        fstpl     -112(%rbp)                                    #41.23
                                # LOE
..B1.9:                         # Preds ..B1.8 ..B1.7
..LN67:
        movq      -368(%rbp), %rax                              #42.7
..LN69:
        movl      (%rax), %eax                                  #42.10
        movl      %eax, -40(%rbp)                               #42.10
        movl      $1, -64(%rbp)                                 #42.10
        movl      -40(%rbp), %eax                               #42.10
        testl     %eax, %eax                                    #42.10
        jg        ..B1.12       # Prob 50%                      #42.10
                                # LOE
..B1.10:                        # Preds ..B1.9 ..B1.12
..LN71:
        addl      $1, -56(%rbp)                                 #39.10
        movl      -56(%rbp), %eax                               #39.10
        movl      -60(%rbp), %edx                               #39.10
        cmpl      %edx, %eax                                    #39.10
        jle       ..B1.7        # Prob 50%                      #39.10
        jmp       ..B1.13       # Prob 100%                     #39.10
                                # LOE
..B1.12:                        # Preds ..B1.9 ..B1.12
..LN73:
        movl      -64(%rbp), %eax                               #43.7
..LN75:
        movslq    %eax, %rax                                    #43.15
..LN77:
        movq      64(%rbp), %rdx                                #43.7
..LN79:
        fldl      -112(%rbp)                                    #43.15
        movl      -80(%rbp), %ecx                               #43.15
        movslq    %ecx, %rcx                                    #43.15
..LN81:
        shlq      $3, %rcx                                      #43.28
..LN83:
        movl      -56(%rbp), %ebx                               #43.15
..LN85:
        movslq    %ebx, %rbx                                    #43.28
        imulq     %rcx, %rbx                                    #43.28
..LN87:
        addq      -336(%rbp), %rbx                              #43.7
..LN89:
        movl      -80(%rbp), %ecx                               #43.15
        movslq    %ecx, %rcx                                    #43.15
..LN91:
        shlq      $3, %rcx                                      #43.28
        negq      %rcx                                          #43.28
..LN93:
        addq      %rcx, %rbx                                    #43.7
..LN95:
        movl      -64(%rbp), %ecx                               #43.15
..LN97:
        movslq    %ecx, %rcx                                    #43.28
        fldl      -8(%rbx,%rcx,8)                               #43.28
..LN99:
        fmulp     %st, %st(1)                                   #43.27
..LN101:
        fldl      -8(%rdx,%rax,8)                               #43.15
..LN103:
        faddp     %st, %st(1)                                   #43.7
..LN105:
        movl      -64(%rbp), %eax                               #43.4
..LN107:
        movslq    %eax, %rax                                    #43.7
..LN109:
        movq      64(%rbp), %rdx                                #43.4
..LN111:
        fstpl     -8(%rdx,%rax,8)                               #43.7
..LN113:
        addl      $1, -64(%rbp)                                 #42.10
        movl      -64(%rbp), %eax                               #42.10
        movl      -40(%rbp), %edx                               #42.10
        cmpl      %edx, %eax                                    #42.10
        jle       ..B1.12       # Prob 50%                      #42.10
        jmp       ..B1.10       # Prob 100%                     #42.10
                                # LOE
..B1.13:                        # Preds ..B1.5 ..B1.10
..LN115:
        movq      32(%rbp), %rax                                #44.7
        movl      (%rax), %eax                                  #44.7
..LN117:
        movslq    %eax, %rax                                    #44.13
..LN119:
        movq      64(%rbp), %rdx                                #44.7
        movq      56(%rbp), %rcx                                #44.7
        fldl      -8(%rdx,%rax,8)                               #44.7
        fstpl     (%rcx)                                        #44.7
..LN121:
        fldl      -128(%rbp)                                    #49.7
        fstpl     -120(%rbp)                                    #49.7
..LN123:
        movq      -136(%rbp), %rax                              #50.7
..LN125:
        movl      (%rax), %eax                                  #50.10
        movl      %eax, -52(%rbp)                               #50.10
        movl      $1, -20(%rbp)                                 #50.10
        movl      -52(%rbp), %eax                               #50.10
        testl     %eax, %eax                                    #50.10
        jle       ..B1.16       # Prob 50%                      #50.10
                                # LOE
..B1.15:                        # Preds ..B1.13 ..B1.15
..LN127:
        movl      -80(%rbp), %eax                               #51.7
        movslq    %eax, %rax                                    #51.7
..LN129:
        shlq      $3, %rax                                      #51.12
..LN131:
        movl      -20(%rbp), %edx                               #51.7
..LN133:
        movslq    %edx, %rdx                                    #51.12
        imulq     %rax, %rdx                                    #51.12
..LN135:
        addq      -352(%rbp), %rdx                              #51.7
..LN137:
        movl      -80(%rbp), %eax                               #51.7
        movslq    %eax, %rax                                    #51.7
..LN139:
        shlq      $3, %rax                                      #51.12
        negq      %rax                                          #51.12
..LN141:
        addq      %rax, %rdx                                    #51.7
        movq      32(%rbp), %rax                                #51.7
        movl      (%rax), %eax                                  #51.7
..LN143:
        movslq    %eax, %rax                                    #51.12
        movl      -20(%rbp), %ecx                               #51.12
..LN145:
        movslq    %ecx, %rcx                                    #51.24
..LN147:
        movq      -360(%rbp), %rbx                              #51.12
        fldl      -8(%rdx,%rax,8)                               #51.12
..LN149:
        fldl      -8(%rbx,%rcx,8)                               #51.24
..LN151:
        fsubrp    %st, %st(1)                                   #51.7
        movl      -20(%rbp), %eax                               #51.7
        movslq    %eax, %rax                                    #51.7
        movq      48(%rbp), %rdx                                #51.7
        fstpl     -8(%rdx,%rax,8)                               #51.7
..LN153:
        movl      -76(%rbp), %eax                               #52.7
        movslq    %eax, %rax                                    #52.7
..LN155:
        shlq      $3, %rax                                      #52.13
..LN157:
        movl      -20(%rbp), %edx                               #52.7
..LN159:
        movslq    %edx, %rdx                                    #52.13
        imulq     %rax, %rdx                                    #52.13
..LN161:
        addq      -344(%rbp), %rdx                              #52.7
        movl      -76(%rbp), %eax                               #52.7
        movslq    %eax, %rax                                    #52.7
..LN163:
        shlq      $3, %rax                                      #52.13
        negq      %rax                                          #52.13
..LN165:
        addq      %rax, %rdx                                    #52.7
        movq      32(%rbp), %rax                                #52.7
        movl      (%rax), %eax                                  #52.7
..LN167:
        movslq    %eax, %rax                                    #52.13
..LN169:
        movl      -20(%rbp), %ecx                               #52.7
        movslq    %ecx, %rcx                                    #52.7
        movq      72(%rbp), %rbx                                #52.7
        fldl      -8(%rdx,%rax,8)                               #52.7
        fstpl     -8(%rbx,%rcx,8)                               #52.7
..LN171:
        movl      -20(%rbp), %eax                               #53.7
        movslq    %eax, %rax                                    #53.7
        movq      80(%rbp), %rdx                                #53.7
        fldl      -128(%rbp)                                    #53.7
        fstpl     -8(%rdx,%rax,8)                               #53.7
..LN173:
        movl      -20(%rbp), %eax                               #54.4
..LN175:
        movslq    %eax, %rax                                    #54.13
..LN177:
        movq      48(%rbp), %rdx                                #54.4
..LN179:
        fldl      -8(%rdx,%rax,8)                               #54.17
        fmul      %st(0), %st                                   #54.17
..LN181:
        fldl      -120(%rbp)                                    #54.4
..LN183:
        faddp     %st, %st(1)                                   #54.7
        fstpl     -120(%rbp)                                    #54.7
..LN185:
        addl      $1, -20(%rbp)                                 #50.10
        movl      -20(%rbp), %eax                               #50.10
        movl      -52(%rbp), %edx                               #50.10
        cmpl      %edx, %eax                                    #50.10
        jle       ..B1.15       # Prob 50%                      #50.10
                                # LOE
..B1.16:                        # Preds ..B1.13 ..B1.15
..LN187:
        movq      -368(%rbp), %rax                              #55.7
..LN189:
        movl      (%rax), %eax                                  #55.10
        movl      %eax, -48(%rbp)                               #55.10
        movl      $1, -64(%rbp)                                 #55.10
        movl      -48(%rbp), %eax                               #55.10
        testl     %eax, %eax                                    #55.10
        jle       ..B1.25       # Prob 50%                      #55.10
                                # LOE
..B1.18:                        # Preds ..B1.16 ..B1.22
..LN191:
        fldl      -128(%rbp)                                    #56.7
        fstpl     -112(%rbp)                                    #56.7
..LN193:
        fldl      -128(%rbp)                                    #57.7
        fstpl     -232(%rbp)                                    #57.7
..LN195:
        movq      -136(%rbp), %rax                              #58.7
..LN197:
        movl      (%rax), %eax                                  #58.10
        movl      %eax, -36(%rbp)                               #58.10
        movl      $1, -56(%rbp)                                 #58.10
        movl      -36(%rbp), %eax                               #58.10
        testl     %eax, %eax                                    #58.10
        jle       ..B1.21       # Prob 50%                      #58.10
                                # LOE
..B1.20:                        # Preds ..B1.18 ..B1.20
..LN199:
        movl      -80(%rbp), %eax                               #59.7
        movslq    %eax, %rax                                    #59.7
..LN201:
        shlq      $3, %rax                                      #59.17
..LN203:
        movl      -56(%rbp), %edx                               #59.7
..LN205:
        movslq    %edx, %rdx                                    #59.17
        imulq     %rax, %rdx                                    #59.17
..LN207:
        addq      -352(%rbp), %rdx                              #59.7
..LN209:
        movl      -80(%rbp), %eax                               #59.7
        movslq    %eax, %rax                                    #59.7
..LN211:
        shlq      $3, %rax                                      #59.17
        negq      %rax                                          #59.17
..LN213:
        addq      %rax, %rdx                                    #59.7
        movl      -64(%rbp), %eax                               #59.7
..LN215:
        movslq    %eax, %rax                                    #59.17
        fldl      -8(%rdx,%rax,8)                               #59.17
        movl      -56(%rbp), %eax                               #59.17
..LN217:
        movslq    %eax, %rax                                    #59.26
..LN219:
        movq      -360(%rbp), %rdx                              #59.17
..LN221:
        fldl      -8(%rdx,%rax,8)                               #59.26
..LN223:
        fmulp     %st, %st(1)                                   #59.25
..LN225:
        fldl      -112(%rbp)                                    #59.7
        faddp     %st, %st(1)                                   #59.7
        fstpl     -112(%rbp)                                    #59.7
..LN227:
        movl      -80(%rbp), %eax                               #60.4
        movslq    %eax, %rax                                    #60.4
..LN229:
        shlq      $3, %rax                                      #60.15
..LN231:
        movl      -56(%rbp), %edx                               #60.4
..LN233:
        movslq    %edx, %rdx                                    #60.15
        imulq     %rax, %rdx                                    #60.15
..LN235:
        addq      -352(%rbp), %rdx                              #60.7
..LN237:
        movl      -80(%rbp), %eax                               #60.4
        movslq    %eax, %rax                                    #60.4
..LN239:
        shlq      $3, %rax                                      #60.15
        negq      %rax                                          #60.15
..LN241:
        addq      %rax, %rdx                                    #60.7
..LN243:
        movl      -64(%rbp), %eax                               #60.4
..LN245:
        movslq    %eax, %rax                                    #60.15
        fldl      -8(%rdx,%rax,8)                               #60.15
        movl      -56(%rbp), %eax                               #60.15
..LN247:
        movslq    %eax, %rax                                    #60.24
..LN249:
        movq      48(%rbp), %rdx                                #60.15
..LN251:
        fldl      -8(%rdx,%rax,8)                               #60.24
..LN253:
        fmulp     %st, %st(1)                                   #60.23
..LN255:
        fldl      -232(%rbp)                                    #60.4
..LN257:
        faddp     %st, %st(1)                                   #60.7
        fstpl     -232(%rbp)                                    #60.7
..LN259:
        addl      $1, -56(%rbp)                                 #58.10
        movl      -56(%rbp), %eax                               #58.10
        movl      -36(%rbp), %edx                               #58.10
        cmpl      %edx, %eax                                    #58.10
        jle       ..B1.20       # Prob 50%                      #58.10
                                # LOE
..B1.21:                        # Preds ..B1.18 ..B1.20
..LN261:
        movl      -64(%rbp), %eax                               #61.7
..LN263:
        movslq    %eax, %rax                                    #61.12
..LN265:
        movq      64(%rbp), %rdx                                #61.7
..LN267:
        fldl      -8(%rdx,%rax,8)                               #61.12
        fldl      -112(%rbp)                                    #61.12
..LN269:
        fmulp     %st, %st(1)                                   #61.7
        fstpl     -112(%rbp)                                    #61.7
..LN271:
        movl      -64(%rbp), %eax                               #62.7
..LN273:
        movslq    %eax, %rax                                    #62.11
..LN275:
        movq      64(%rbp), %rdx                                #62.7
..LN277:
        fldl      -8(%rdx,%rax,8)                               #62.11
        fldl      -232(%rbp)                                    #62.11
..LN279:
        fmulp     %st, %st(1)                                   #62.7
        fstpl     -232(%rbp)                                    #62.7
..LN281:
        movq      -136(%rbp), %rax                              #63.7
..LN283:
        movl      (%rax), %eax                                  #63.10
        movl      %eax, -32(%rbp)                               #63.10
        movl      $1, -20(%rbp)                                 #63.10
        movl      -32(%rbp), %eax                               #63.10
        testl     %eax, %eax                                    #63.10
        jg        ..B1.24       # Prob 50%                      #63.10
                                # LOE
..B1.22:                        # Preds ..B1.21 ..B1.24
..LN285:
        addl      $1, -64(%rbp)                                 #55.10
        movl      -64(%rbp), %eax                               #55.10
        movl      -48(%rbp), %edx                               #55.10
        cmpl      %edx, %eax                                    #55.10
        jle       ..B1.18       # Prob 50%                      #55.10
        jmp       ..B1.25       # Prob 100%                     #55.10
                                # LOE
..B1.24:                        # Preds ..B1.21 ..B1.24
..LN287:
        movl      -20(%rbp), %eax                               #64.7
..LN289:
        movslq    %eax, %rax                                    #64.13
..LN291:
        movq      72(%rbp), %rdx                                #64.7
..LN293:
        fldl      -112(%rbp)                                    #64.13
        movl      -80(%rbp), %ecx                               #64.13
        movslq    %ecx, %rcx                                    #64.13
..LN295:
        shlq      $3, %rcx                                      #64.24
..LN297:
        movl      -20(%rbp), %ebx                               #64.13
..LN299:
        movslq    %ebx, %rbx                                    #64.24
        imulq     %rcx, %rbx                                    #64.24
..LN301:
        addq      -352(%rbp), %rbx                              #64.7
..LN303:
        movl      -80(%rbp), %ecx                               #64.13
        movslq    %ecx, %rcx                                    #64.13
..LN305:
        shlq      $3, %rcx                                      #64.24
        negq      %rcx                                          #64.24
..LN307:
        addq      %rcx, %rbx                                    #64.7
..LN309:
        movl      -64(%rbp), %ecx                               #64.13
..LN311:
        movslq    %ecx, %rcx                                    #64.24
        fldl      -8(%rbx,%rcx,8)                               #64.24
..LN313:
        fmulp     %st, %st(1)                                   #64.23
..LN315:
        fldl      -8(%rdx,%rax,8)                               #64.13
..LN317:
        faddp     %st, %st(1)                                   #64.7
        movl      -20(%rbp), %eax                               #64.7
        movslq    %eax, %rax                                    #64.7
        movq      72(%rbp), %rdx                                #64.7
        fstpl     -8(%rdx,%rax,8)                               #64.7
..LN319:
        movl      -20(%rbp), %eax                               #65.7
..LN321:
        movslq    %eax, %rax                                    #65.13
..LN323:
        movq      80(%rbp), %rdx                                #65.7
..LN325:
        fldl      -232(%rbp)                                    #65.13
        movl      -80(%rbp), %ecx                               #65.13
        movslq    %ecx, %rcx                                    #65.13
..LN327:
        shlq      $3, %rcx                                      #65.23
..LN329:
        movl      -20(%rbp), %ebx                               #65.13
..LN331:
        movslq    %ebx, %rbx                                    #65.23
        imulq     %rcx, %rbx                                    #65.23
..LN333:
        addq      -352(%rbp), %rbx                              #65.7
..LN335:
        movl      -80(%rbp), %ecx                               #65.13
        movslq    %ecx, %rcx                                    #65.13
..LN337:
        shlq      $3, %rcx                                      #65.23
        negq      %rcx                                          #65.23
..LN339:
        addq      %rcx, %rbx                                    #65.7
..LN341:
        movl      -64(%rbp), %ecx                               #65.13
..LN343:
        movslq    %ecx, %rcx                                    #65.23
        fldl      -8(%rbx,%rcx,8)                               #65.23
..LN345:
        fmulp     %st, %st(1)                                   #65.22
..LN347:
        fldl      -8(%rdx,%rax,8)                               #65.13
..LN349:
        faddp     %st, %st(1)                                   #65.7
..LN351:
        movl      -20(%rbp), %eax                               #65.4
..LN353:
        movslq    %eax, %rax                                    #65.7
..LN355:
        movq      80(%rbp), %rdx                                #65.4
..LN357:
        fstpl     -8(%rdx,%rax,8)                               #65.7
..LN359:
        addl      $1, -20(%rbp)                                 #63.10
        movl      -20(%rbp), %eax                               #63.10
        movl      -32(%rbp), %edx                               #63.10
        cmpl      %edx, %eax                                    #63.10
        jle       ..B1.24       # Prob 50%                      #63.10
        jmp       ..B1.22       # Prob 100%                     #63.10
                                # LOE
..B1.25:                        # Preds ..B1.16 ..B1.22
..LN361:
        fldl      -128(%rbp)                                    #70.7
        fstpl     -248(%rbp)                                    #70.7
..LN363:
        fldl      -128(%rbp)                                    #71.7
        fstpl     -104(%rbp)                                    #71.7
..LN365:
        fldl      -128(%rbp)                                    #72.7
        fstpl     -240(%rbp)                                    #72.7
..LN367:
        movq      -136(%rbp), %rax                              #73.7
..LN369:
        movl      (%rax), %eax                                  #73.10
        movl      %eax, -44(%rbp)                               #73.10
        movl      $1, -20(%rbp)                                 #73.10
        movl      -44(%rbp), %eax                               #73.10
        testl     %eax, %eax                                    #73.10
        jle       ..B1.28       # Prob 50%                      #73.10
                                # LOE
..B1.27:                        # Preds ..B1.25 ..B1.27
..LN371:
        movl      -20(%rbp), %eax                               #74.7
..LN373:
        movslq    %eax, %rax                                    #74.13
..LN375:
        movq      72(%rbp), %rdx                                #74.7
..LN377:
        fldl      -8(%rdx,%rax,8)                               #74.18
        fmul      %st(0), %st                                   #74.18
..LN379:
        fldl      -248(%rbp)                                    #74.7
        faddp     %st, %st(1)                                   #74.7
        fstpl     -248(%rbp)                                    #74.7
..LN381:
        movl      -20(%rbp), %eax                               #75.7
..LN383:
        movslq    %eax, %rax                                    #75.13
..LN385:
        movq      48(%rbp), %rdx                                #75.7
..LN387:
        fldl      -8(%rdx,%rax,8)                               #75.13
        movl      -20(%rbp), %eax                               #75.13
..LN389:
        movslq    %eax, %rax                                    #75.18
..LN391:
        movq      72(%rbp), %rdx                                #75.13
..LN393:
        fldl      -8(%rdx,%rax,8)                               #75.18
..LN395:
        fmulp     %st, %st(1)                                   #75.17
..LN397:
        fldl      -104(%rbp)                                    #75.7
        faddp     %st, %st(1)                                   #75.7
        fstpl     -104(%rbp)                                    #75.7
..LN399:
        movl      -20(%rbp), %eax                               #76.4
..LN401:
        movslq    %eax, %rax                                    #76.15
..LN403:
        movq      48(%rbp), %rdx                                #76.4
..LN405:
        fldl      -8(%rdx,%rax,8)                               #76.15
        movl      -20(%rbp), %eax                               #76.15
..LN407:
        movslq    %eax, %rax                                    #76.20
..LN409:
        movq      80(%rbp), %rdx                                #76.15
..LN411:
        fldl      -8(%rdx,%rax,8)                               #76.20
..LN413:
        fmulp     %st, %st(1)                                   #76.19
..LN415:
        fldl      -240(%rbp)                                    #76.4
..LN417:
        faddp     %st, %st(1)                                   #76.7
        fstpl     -240(%rbp)                                    #76.7
..LN419:
        addl      $1, -20(%rbp)                                 #73.10
        movl      -20(%rbp), %eax                               #73.10
        movl      -44(%rbp), %edx                               #73.10
        cmpl      %edx, %eax                                    #73.10
        jle       ..B1.27       # Prob 50%                      #73.10
                                # LOE
..B1.28:                        # Preds ..B1.25 ..B1.27
..LN421:
        movsd     -120(%rbp), %xmm0                             #77.19
        movl      $1, %eax                                      #77.19
        call      sqrt                                          #77.19
                                # LOE xmm0
..B1.84:                        # Preds ..B1.28
        movsd     %xmm0, -192(%rbp)                             #77.19
                                # LOE
..B1.29:                        # Preds ..B1.84
..LN423:
        movq      40(%rbp), %rax                                #77.7
        fldl      (%rax)                                        #77.7
        fldl      -192(%rbp)                                    #77.7
        fdivrp    %st, %st(1)                                   #77.7
        fstpl     -224(%rbp)                                    #77.7
..LN425:
        fldl      -104(%rbp)                                    #78.7
        fldl      -240(%rbp)                                    #78.7
..LN427:
        fmulp     %st, %st(1)                                   #78.13
        fldl      -128(%rbp)                                    #78.13
..LN429:
        fxch      %st(1)                                        #78.18
        fstpl     -88(%rbp)                                     #78.18
        fldl      -88(%rbp)                                     #78.18
        fcomip    %st(1), %st                                   #78.18
        fstp      %st(0)                                        #78.18
        jae       ..B1.31       # Prob 50%                      #78.18
        jp        ..B1.31       # Prob 0%                       #78.18
                                # LOE
..B1.30:                        # Preds ..B1.29
..LN431:
        fldl      -224(%rbp)                                    #78.29
        fchs                                                    #78.29
        fstpl     -224(%rbp)                                    #78.29
                                # LOE
..B1.31:                        # Preds ..B1.30 ..B1.29
..LN433:
        fldl      -128(%rbp)                                    #79.7
        fstpl     -112(%rbp)                                    #79.7
..LN435:
        fldl      -104(%rbp)                                    #80.7
        fldl      -104(%rbp)                                    #80.7
..LN437:
        fmulp     %st, %st(1)                                   #80.13
..LN439:
        fldl      _2il0floatpacket.3(%rip)                      #80.28
..LN441:
        fldl      -120(%rbp)                                    #80.13
..LN443:
        fmulp     %st, %st(1)                                   #80.28
        fldl      -248(%rbp)                                    #80.28
..LN445:
        fmulp     %st, %st(1)                                   #80.31
..LN447:
        fxch      %st(1)                                        #80.17
        fstpl     -88(%rbp)                                     #80.17
        movsd     -88(%rbp), %xmm0                              #80.17
        fstpl     -88(%rbp)                                     #80.17
        movsd     -88(%rbp), %xmm1                              #80.17
        comisd    %xmm1, %xmm0                                  #80.17
        jbe       ..B1.33       # Prob 50%                      #80.17
                                # LOE
..B1.32:                        # Preds ..B1.31
..LN449:
        fldl      -272(%rbp)                                    #80.36
        fstpl     -112(%rbp)                                    #80.36
                                # LOE
..B1.33:                        # Preds ..B1.32 ..B1.31
..LN451:
        fldl      -224(%rbp)                                    #81.7
        fldl      -104(%rbp)                                    #81.7
..LN453:
        fabs                                                    #81.18
        fldl      -280(%rbp)                                    #81.18
        fldl      -224(%rbp)                                    #81.18
..LN455:
        fmulp     %st, %st(1)                                   #81.31
        fldl      -240(%rbp)                                    #81.31
..LN457:
        fabs                                                    #81.38
..LN459:
        fmulp     %st, %st(1)                                   #81.37
..LN461:
        faddp     %st, %st(1)                                   #81.26
..LN463:
        fmulp     %st, %st(1)                                   #81.7
        fstpl     -216(%rbp)                                    #81.7
..LN465:
        fldl      -248(%rbp)                                    #82.7
        fldl      -256(%rbp)                                    #82.7
..LN467:
        fmulp     %st, %st(1)                                   #82.13
..LN469:
        fldl      _2il0floatpacket.4(%rip)                      #82.31
..LN471:
        fldl      -216(%rbp)                                    #82.13
..LN473:
        fmulp     %st, %st(1)                                   #82.31
        fldl      -216(%rbp)                                    #82.31
..LN475:
        fmulp     %st, %st(1)                                   #82.35
..LN477:
        fxch      %st(1)                                        #82.20
        fstpl     -88(%rbp)                                     #82.20
        movsd     -88(%rbp), %xmm0                              #82.20
        fstpl     -88(%rbp)                                     #82.20
        movsd     -88(%rbp), %xmm1                              #82.20
        comisd    %xmm0, %xmm1                                  #82.20
        jbe       ..B1.35       # Prob 50%                      #82.20
                                # LOE
..B1.34:                        # Preds ..B1.33
..LN479:
        fldl      -272(%rbp)                                    #82.41
        fstpl     -112(%rbp)                                    #82.41
                                # LOE
..B1.35:                        # Preds ..B1.34 ..B1.33
..LN481:
        movq      -136(%rbp), %rax                              #83.7
..LN483:
        movl      (%rax), %eax                                  #83.10
        movl      %eax, -28(%rbp)                               #83.10
        movl      $1, -20(%rbp)                                 #83.10
        movl      -28(%rbp), %eax                               #83.10
        testl     %eax, %eax                                    #83.10
        jle       ..B1.39       # Prob 50%                      #83.10
                                # LOE
..B1.37:                        # Preds ..B1.35 ..B1.37
..LN485:
        fldl      -224(%rbp)                                    #84.7
        movl      -20(%rbp), %eax                               #84.7
..LN487:
        movslq    %eax, %rax                                    #84.18
..LN489:
        movq      48(%rbp), %rdx                                #84.7
..LN491:
        fldl      -8(%rdx,%rax,8)                               #84.18
..LN493:
        fmulp     %st, %st(1)                                   #84.7
        movl      -20(%rbp), %eax                               #84.7
        movslq    %eax, %rax                                    #84.7
        movq      48(%rbp), %rdx                                #84.7
        fstpl     -8(%rdx,%rax,8)                               #84.7
..LN495:
        fldl      -224(%rbp)                                    #85.7
        movl      -20(%rbp), %eax                               #85.7
..LN497:
        movslq    %eax, %rax                                    #85.19
..LN499:
        movq      80(%rbp), %rdx                                #85.7
..LN501:
        fldl      -8(%rdx,%rax,8)                               #85.19
..LN503:
        fmulp     %st, %st(1)                                   #85.7
        movl      -20(%rbp), %eax                               #85.7
        movslq    %eax, %rax                                    #85.7
        movq      80(%rbp), %rdx                                #85.7
        fstpl     -8(%rdx,%rax,8)                               #85.7
..LN505:
        movl      -20(%rbp), %eax                               #86.7
..LN507:
        movslq    %eax, %rax                                    #86.12
..LN509:
        movq      72(%rbp), %rdx                                #86.7
..LN511:
        fldl      -112(%rbp)                                    #86.12
        movl      -20(%rbp), %ecx                               #86.12
..LN513:
        movslq    %ecx, %rcx                                    #86.23
..LN515:
        movq      80(%rbp), %rbx                                #86.12
..LN517:
        fldl      -8(%rbx,%rcx,8)                               #86.23
..LN519:
        fmulp     %st, %st(1)                                   #86.22
..LN521:
        fldl      -8(%rdx,%rax,8)                               #86.12
..LN523:
        faddp     %st, %st(1)                                   #86.7
..LN525:
        movl      -20(%rbp), %eax                               #86.4
..LN527:
        movslq    %eax, %rax                                    #86.7
..LN529:
        movq      88(%rbp), %rdx                                #86.4
..LN531:
        fstpl     -8(%rdx,%rax,8)                               #86.7
..LN533:
        addl      $1, -20(%rbp)                                 #83.10
        movl      -20(%rbp), %eax                               #83.10
        movl      -28(%rbp), %edx                               #83.10
        cmpl      %edx, %eax                                    #83.10
        jle       ..B1.37       # Prob 50%                      #83.10
                                # LOE
..B1.39:                        # Preds ..B1.35 ..B1.37 ..B1.79
..LN535:
        addl      $1, -24(%rbp)                                 #92.7
..LN537:
        fldl      -128(%rbp)                                    #93.7
        fstpl     -120(%rbp)                                    #93.7
..LN539:
        fldl      -128(%rbp)                                    #94.7
        fstpl     -104(%rbp)                                    #94.7
..LN541:
        fldl      -128(%rbp)                                    #95.7
        fstpl     -96(%rbp)                                     #95.7
..LN543:
        movq      -136(%rbp), %rax                              #96.7
..LN545:
        movl      (%rax), %eax                                  #96.10
        movl      %eax, -16(%rbp)                               #96.10
        movl      $1, -20(%rbp)                                 #96.10
        movl      -16(%rbp), %eax                               #96.10
        testl     %eax, %eax                                    #96.10
        jle       ..B1.42       # Prob 50%                      #96.10
                                # LOE
..B1.41:                        # Preds ..B1.39 ..B1.41
..LN547:
        movl      -20(%rbp), %eax                               #97.7
..LN549:
        movslq    %eax, %rax                                    #97.13
..LN551:
        movq      48(%rbp), %rdx                                #97.7
..LN553:
        fldl      -8(%rdx,%rax,8)                               #97.17
        fmul      %st(0), %st                                   #97.17
..LN555:
        fldl      -120(%rbp)                                    #97.7
        faddp     %st, %st(1)                                   #97.7
        fstpl     -120(%rbp)                                    #97.7
..LN557:
        movl      -20(%rbp), %eax                               #98.7
..LN559:
        movslq    %eax, %rax                                    #98.13
..LN561:
        movq      48(%rbp), %rdx                                #98.7
..LN563:
        fldl      -8(%rdx,%rax,8)                               #98.13
        movl      -20(%rbp), %eax                               #98.13
..LN565:
        movslq    %eax, %rax                                    #98.18
..LN567:
        movq      88(%rbp), %rdx                                #98.13
..LN569:
        fldl      -8(%rdx,%rax,8)                               #98.18
..LN571:
        fmulp     %st, %st(1)                                   #98.17
..LN573:
        fldl      -104(%rbp)                                    #98.7
        faddp     %st, %st(1)                                   #98.7
        fstpl     -104(%rbp)                                    #98.7
..LN575:
        movl      -20(%rbp), %eax                               #99.4
..LN577:
        movslq    %eax, %rax                                    #99.13
..LN579:
        movq      88(%rbp), %rdx                                #99.4
..LN581:
        fldl      -8(%rdx,%rax,8)                               #99.17
        fmul      %st(0), %st                                   #99.17
..LN583:
        fldl      -96(%rbp)                                     #99.4
..LN585:
        faddp     %st, %st(1)                                   #99.7
        fstpl     -96(%rbp)                                     #99.7
..LN587:
        addl      $1, -20(%rbp)                                 #96.10
        movl      -20(%rbp), %eax                               #96.10
        movl      -16(%rbp), %edx                               #96.10
        cmpl      %edx, %eax                                    #96.10
        jle       ..B1.41       # Prob 50%                      #96.10
                                # LOE
..B1.42:                        # Preds ..B1.39 ..B1.41
..LN589:
        fldl      -120(%rbp)                                    #100.7
        fldl      -96(%rbp)                                     #100.7
..LN591:
        fmulp     %st, %st(1)                                   #100.14
        fldl      -104(%rbp)                                    #100.14
        fldl      -104(%rbp)                                    #100.14
..LN593:
        fmulp     %st, %st(1)                                   #100.20
..LN595:
        fsubrp    %st, %st(1)                                   #100.7
        fstpl     -112(%rbp)                                    #100.7
..LN597:
        fldl      _2il0floatpacket.5(%rip)                      #101.27
..LN599:
        fldl      -120(%rbp)                                    #101.7
..LN601:
        fmulp     %st, %st(1)                                   #101.27
        fldl      -96(%rbp)                                     #101.27
..LN603:
        fmulp     %st, %st(1)                                   #101.30
..LN605:
        fldl      -112(%rbp)                                    #101.7
..LN607:
        fxch      %st(1)                                        #101.16
        fstpl     -88(%rbp)                                     #101.16
        fldl      -88(%rbp)                                     #101.16
        fcomip    %st(1), %st                                   #101.16
        fstp      %st(0)                                        #101.16
        jb        ..B1.44       # Prob 50%                      #101.16
                                # LOE
..B1.43:                        # Preds ..B1.42
..LN609:
        movq      $0, -512(%rbp)                                #101.35
        jmp       ..B1.80       # Prob 100%                     #101.35
                                # LOE
..B1.44:                        # Preds ..B1.42
..LN611:
        movsd     -112(%rbp), %xmm0                             #102.13
        movl      $1, %eax                                      #102.13
        call      sqrt                                          #102.13
                                # LOE xmm0
..B1.85:                        # Preds ..B1.44
        movsd     %xmm0, -384(%rbp)                             #102.13
                                # LOE
..B1.45:                        # Preds ..B1.85
..LN613:
        movq      -384(%rbp), %rax                              #102.7
        movq      %rax, -504(%rbp)                              #102.7
..LN615:
        movq      -136(%rbp), %rax                              #103.7
..LN617:
        movl      (%rax), %eax                                  #103.10
        movl      %eax, -176(%rbp)                              #103.10
        movl      $1, -20(%rbp)                                 #103.10
        movl      -176(%rbp), %eax                              #103.10
        testl     %eax, %eax                                    #103.10
        jle       ..B1.48       # Prob 50%                      #103.10
                                # LOE
..B1.47:                        # Preds ..B1.45 ..B1.47
..LN619:
        fldl      -120(%rbp)                                    #104.7
        movl      -20(%rbp), %eax                               #104.7
..LN621:
        movslq    %eax, %rax                                    #104.16
..LN623:
        movq      88(%rbp), %rdx                                #104.7
..LN625:
        fldl      -8(%rdx,%rax,8)                               #104.16
..LN627:
        fmulp     %st, %st(1)                                   #104.15
        fldl      -104(%rbp)                                    #104.15
        movl      -20(%rbp), %eax                               #104.15
..LN629:
        movslq    %eax, %rax                                    #104.24
..LN631:
        movq      48(%rbp), %rdx                                #104.15
..LN633:
        fldl      -8(%rdx,%rax,8)                               #104.24
..LN635:
        fmulp     %st, %st(1)                                   #104.23
..LN637:
        fsubrp    %st, %st(1)                                   #104.20
        fldl      -504(%rbp)                                    #104.20
..LN639:
        fdivrp    %st, %st(1)                                   #104.7
        movl      -20(%rbp), %eax                               #104.7
        movslq    %eax, %rax                                    #104.7
        movq      88(%rbp), %rdx                                #104.7
        fstpl     -8(%rdx,%rax,8)                               #104.7
..LN641:
        movl      -20(%rbp), %eax                               #105.3
..LN643:
        movslq    %eax, %rax                                    #105.7
..LN645:
        movq      96(%rbp), %rdx                                #105.3
..LN647:
        fldl      -128(%rbp)                                    #105.7
        fstpl     -8(%rdx,%rax,8)                               #105.7
..LN649:
        addl      $1, -20(%rbp)                                 #103.10
        movl      -20(%rbp), %eax                               #103.10
        movl      -176(%rbp), %edx                              #103.10
        cmpl      %edx, %eax                                    #103.10
        jle       ..B1.47       # Prob 50%                      #103.10
                                # LOE
..B1.48:                        # Preds ..B1.45 ..B1.47
..LN651:
        movq      -368(%rbp), %rax                              #110.7
..LN653:
        movl      (%rax), %eax                                  #110.10
        movl      %eax, -172(%rbp)                              #110.10
        movl      $1, -64(%rbp)                                 #110.10
        movl      -172(%rbp), %eax                              #110.10
        testl     %eax, %eax                                    #110.10
        jle       ..B1.57       # Prob 50%                      #110.10
                                # LOE
..B1.50:                        # Preds ..B1.48 ..B1.54
..LN655:
        fldl      -128(%rbp)                                    #111.7
        fstpl     -232(%rbp)                                    #111.7
..LN657:
        movq      -136(%rbp), %rax                              #112.7
..LN659:
        movl      (%rax), %eax                                  #112.10
        movl      %eax, -152(%rbp)                              #112.10
        movl      $1, -56(%rbp)                                 #112.10
        movl      -152(%rbp), %eax                              #112.10
        testl     %eax, %eax                                    #112.10
        jle       ..B1.53       # Prob 50%                      #112.10
                                # LOE
..B1.52:                        # Preds ..B1.50 ..B1.52
..LN661:
        movl      -80(%rbp), %eax                               #113.3
        movslq    %eax, %rax                                    #113.3
..LN663:
        shlq      $3, %rax                                      #113.15
..LN665:
        movl      -56(%rbp), %edx                               #113.3
..LN667:
        movslq    %edx, %rdx                                    #113.15
        imulq     %rax, %rdx                                    #113.15
..LN669:
        addq      -352(%rbp), %rdx                              #113.7
..LN671:
        movl      -80(%rbp), %eax                               #113.3
        movslq    %eax, %rax                                    #113.3
..LN673:
        shlq      $3, %rax                                      #113.15
        negq      %rax                                          #113.15
..LN675:
        addq      %rax, %rdx                                    #113.7
..LN677:
        movl      -64(%rbp), %eax                               #113.3
..LN679:
        movslq    %eax, %rax                                    #113.15
        fldl      -8(%rdx,%rax,8)                               #113.15
        movl      -56(%rbp), %eax                               #113.15
..LN681:
        movslq    %eax, %rax                                    #113.24
..LN683:
        movq      88(%rbp), %rdx                                #113.15
..LN685:
        fldl      -8(%rdx,%rax,8)                               #113.24
..LN687:
        fmulp     %st, %st(1)                                   #113.23
..LN689:
        fldl      -232(%rbp)                                    #113.3
..LN691:
        faddp     %st, %st(1)                                   #113.7
        fstpl     -232(%rbp)                                    #113.7
..LN693:
        addl      $1, -56(%rbp)                                 #112.10
        movl      -56(%rbp), %eax                               #112.10
        movl      -152(%rbp), %edx                              #112.10
        cmpl      %edx, %eax                                    #112.10
        jle       ..B1.52       # Prob 50%                      #112.10
                                # LOE
..B1.53:                        # Preds ..B1.50 ..B1.52
..LN695:
        movl      -64(%rbp), %eax                               #114.7
..LN697:
        movslq    %eax, %rax                                    #114.11
..LN699:
        movq      64(%rbp), %rdx                                #114.7
..LN701:
        fldl      -8(%rdx,%rax,8)                               #114.11
        fldl      -232(%rbp)                                    #114.11
..LN703:
        fmulp     %st, %st(1)                                   #114.7
        fstpl     -232(%rbp)                                    #114.7
..LN705:
        movq      -136(%rbp), %rax                              #115.7
..LN707:
        movl      (%rax), %eax                                  #115.10
        movl      %eax, -148(%rbp)                              #115.10
        movl      $1, -20(%rbp)                                 #115.10
        movl      -148(%rbp), %eax                              #115.10
        testl     %eax, %eax                                    #115.10
        jg        ..B1.56       # Prob 50%                      #115.10
                                # LOE
..B1.54:                        # Preds ..B1.53 ..B1.56
..LN709:
        addl      $1, -64(%rbp)                                 #110.10
        movl      -64(%rbp), %eax                               #110.10
        movl      -172(%rbp), %edx                              #110.10
        cmpl      %edx, %eax                                    #110.10
        jle       ..B1.50       # Prob 50%                      #110.10
        jmp       ..B1.57       # Prob 100%                     #110.10
                                # LOE
..B1.56:                        # Preds ..B1.53 ..B1.56
..LN711:
        movl      -20(%rbp), %eax                               #116.7
..LN713:
        movslq    %eax, %rax                                    #116.12
..LN715:
        movq      96(%rbp), %rdx                                #116.7
..LN717:
        fldl      -232(%rbp)                                    #116.12
        movl      -80(%rbp), %ecx                               #116.12
        movslq    %ecx, %rcx                                    #116.12
..LN719:
        shlq      $3, %rcx                                      #116.21
..LN721:
        movl      -20(%rbp), %ebx                               #116.12
..LN723:
        movslq    %ebx, %rbx                                    #116.21
        imulq     %rcx, %rbx                                    #116.21
..LN725:
        addq      -352(%rbp), %rbx                              #116.7
..LN727:
        movl      -80(%rbp), %ecx                               #116.12
        movslq    %ecx, %rcx                                    #116.12
..LN729:
        shlq      $3, %rcx                                      #116.21
        negq      %rcx                                          #116.21
..LN731:
        addq      %rcx, %rbx                                    #116.7
..LN733:
        movl      -64(%rbp), %ecx                               #116.12
..LN735:
        movslq    %ecx, %rcx                                    #116.21
        fldl      -8(%rbx,%rcx,8)                               #116.21
..LN737:
        fmulp     %st, %st(1)                                   #116.20
..LN739:
        fldl      -8(%rdx,%rax,8)                               #116.12
..LN741:
        faddp     %st, %st(1)                                   #116.7
..LN743:
        movl      -20(%rbp), %eax                               #116.3
..LN745:
        movslq    %eax, %rax                                    #116.7
..LN747:
        movq      96(%rbp), %rdx                                #116.3
..LN749:
        fstpl     -8(%rdx,%rax,8)                               #116.7
..LN751:
        addl      $1, -20(%rbp)                                 #115.10
        movl      -20(%rbp), %eax                               #115.10
        movl      -148(%rbp), %edx                              #115.10
        cmpl      %edx, %eax                                    #115.10
        jle       ..B1.56       # Prob 50%                      #115.10
        jmp       ..B1.54       # Prob 100%                     #115.10
                                # LOE
..B1.57:                        # Preds ..B1.48 ..B1.54
..LN753:
        fldl      -128(%rbp)                                    #117.7
        fstpl     -496(%rbp)                                    #117.7
..LN755:
        fldl      -128(%rbp)                                    #118.7
        fstpl     -488(%rbp)                                    #118.7
..LN757:
        fldl      -128(%rbp)                                    #119.7
        fstpl     -480(%rbp)                                    #119.7
..LN759:
        fldl      -128(%rbp)                                    #120.7
        fstpl     -472(%rbp)                                    #120.7
..LN761:
        fldl      -128(%rbp)                                    #121.7
        fstpl     -464(%rbp)                                    #121.7
..LN763:
        movq      -136(%rbp), %rax                              #122.7
..LN765:
        movl      (%rax), %eax                                  #122.10
        movl      %eax, -168(%rbp)                              #122.10
        movl      $1, -20(%rbp)                                 #122.10
        movl      -168(%rbp), %eax                              #122.10
        testl     %eax, %eax                                    #122.10
        jle       ..B1.60       # Prob 50%                      #122.10
                                # LOE
..B1.59:                        # Preds ..B1.57 ..B1.59
..LN767:
        movl      -20(%rbp), %eax                               #123.7
..LN769:
        movslq    %eax, %rax                                    #123.15
..LN771:
        movq      88(%rbp), %rdx                                #123.7
..LN773:
        fldl      -8(%rdx,%rax,8)                               #123.15
        movl      -20(%rbp), %eax                               #123.15
..LN775:
        movslq    %eax, %rax                                    #123.20
..LN777:
        movq      96(%rbp), %rdx                                #123.15
..LN779:
        fldl      -8(%rdx,%rax,8)                               #123.20
..LN781:
        fmulp     %st, %st(1)                                   #123.19
..LN783:
        fldl      -496(%rbp)                                    #123.7
        faddp     %st, %st(1)                                   #123.7
        fstpl     -496(%rbp)                                    #123.7
..LN785:
        movl      -20(%rbp), %eax                               #124.7
..LN787:
        movslq    %eax, %rax                                    #124.15
..LN789:
        movq      48(%rbp), %rdx                                #124.7
..LN791:
        fldl      -8(%rdx,%rax,8)                               #124.15
        movl      -20(%rbp), %eax                               #124.15
..LN793:
        movslq    %eax, %rax                                    #124.20
..LN795:
        movq      72(%rbp), %rdx                                #124.15
..LN797:
        fldl      -8(%rdx,%rax,8)                               #124.20
..LN799:
        fmulp     %st, %st(1)                                   #124.19
..LN801:
        fldl      -488(%rbp)                                    #124.7
        faddp     %st, %st(1)                                   #124.7
        fstpl     -488(%rbp)                                    #124.7
..LN803:
        movl      -20(%rbp), %eax                               #125.7
..LN805:
        movslq    %eax, %rax                                    #125.15
..LN807:
        movq      88(%rbp), %rdx                                #125.7
..LN809:
        fldl      -8(%rdx,%rax,8)                               #125.15
        movl      -20(%rbp), %eax                               #125.15
..LN811:
        movslq    %eax, %rax                                    #125.20
..LN813:
        movq      72(%rbp), %rdx                                #125.15
..LN815:
        fldl      -8(%rdx,%rax,8)                               #125.20
..LN817:
        fmulp     %st, %st(1)                                   #125.19
..LN819:
        fldl      -480(%rbp)                                    #125.7
        faddp     %st, %st(1)                                   #125.7
        fstpl     -480(%rbp)                                    #125.7
..LN821:
        movl      -20(%rbp), %eax                               #126.7
..LN823:
        movslq    %eax, %rax                                    #126.15
..LN825:
        movq      48(%rbp), %rdx                                #126.7
..LN827:
        fldl      -8(%rdx,%rax,8)                               #126.15
        movl      -20(%rbp), %eax                               #126.15
..LN829:
        movslq    %eax, %rax                                    #126.20
..LN831:
        movq      80(%rbp), %rdx                                #126.15
..LN833:
        fldl      -8(%rdx,%rax,8)                               #126.20
..LN835:
        fmulp     %st, %st(1)                                   #126.19
..LN837:
        fldl      -472(%rbp)                                    #126.7
        faddp     %st, %st(1)                                   #126.7
        fstpl     -472(%rbp)                                    #126.7
..LN839:
        movl      -20(%rbp), %eax                               #127.3
..LN841:
        movslq    %eax, %rax                                    #127.15
..LN843:
        movq      88(%rbp), %rdx                                #127.3
..LN845:
        fldl      -8(%rdx,%rax,8)                               #127.15
        movl      -20(%rbp), %eax                               #127.15
..LN847:
        movslq    %eax, %rax                                    #127.20
..LN849:
        movq      80(%rbp), %rdx                                #127.15
..LN851:
        fldl      -8(%rdx,%rax,8)                               #127.20
..LN853:
        fmulp     %st, %st(1)                                   #127.19
..LN855:
        fldl      -464(%rbp)                                    #127.3
..LN857:
        faddp     %st, %st(1)                                   #127.7
        fstpl     -464(%rbp)                                    #127.7
..LN859:
        addl      $1, -20(%rbp)                                 #122.10
        movl      -20(%rbp), %eax                               #122.10
        movl      -168(%rbp), %edx                              #122.10
        cmpl      %edx, %eax                                    #122.10
        jle       ..B1.59       # Prob 50%                      #122.10
                                # LOE
..B1.60:                        # Preds ..B1.57 ..B1.59
..LN861:
        fldl      -280(%rbp)                                    #128.7
        fldl      -496(%rbp)                                    #128.7
        fmulp     %st, %st(1)                                   #128.7
        fstpl     -496(%rbp)                                    #128.7
..LN863:
        fldl      -280(%rbp)                                    #129.7
        fldl      -472(%rbp)                                    #129.7
..LN865:
        fmulp     %st, %st(1)                                   #129.15
        fldl      -496(%rbp)                                    #129.15
..LN867:
        fsubrp    %st, %st(1)                                   #129.7
        fstpl     -472(%rbp)                                    #129.7
..LN869:
        fldl      -496(%rbp)                                    #133.7
        fldl      -488(%rbp)                                    #133.7
..LN871:
        faddp     %st, %st(1)                                   #133.17
        fldl      -472(%rbp)                                    #133.17
..LN873:
        faddp     %st, %st(1)                                   #133.7
        fstpl     -456(%rbp)                                    #133.7
..LN875:
        fldl      -456(%rbp)                                    #134.7
        fstpl     -448(%rbp)                                    #134.7
..LN877:
        fldl      -456(%rbp)                                    #135.7
        fstpl     -440(%rbp)                                    #135.7
..LN879:
        movl      $0, -164(%rbp)                                #136.7
..LN881:
        movl      $49, -160(%rbp)                               #137.7
..LN883:
        fldl      -264(%rbp)                                    #138.7
        movl      -160(%rbp), %eax                              #138.7
..LN885:
        addl      $1, %eax                                      #138.18
        movl      %eax, -376(%rbp)                              #138.18
        fildl     -376(%rbp)                                    #138.18
..LN887:
        fstpl     -88(%rbp)                                     #138.7
        fldl      -88(%rbp)                                     #138.7
        fdivrp    %st, %st(1)                                   #138.7
        fstpl     -112(%rbp)                                    #138.7
..LN889:
        movl      -160(%rbp), %eax                              #139.10
        movl      %eax, -156(%rbp)                              #139.10
        movl      $1, -20(%rbp)                                 #139.10
        movl      -156(%rbp), %eax                              #139.10
        testl     %eax, %eax                                    #139.10
        jle       ..B1.67       # Prob 50%                      #139.10
                                # LOE
..B1.62:                        # Preds ..B1.60 ..B1.66
..LN891:
        movl      -20(%rbp), %eax                               #140.13
        movl      %eax, -376(%rbp)                              #140.13
        fildl     -376(%rbp)                                    #140.13
..LN893:
        fstpl     -88(%rbp)                                     #140.7
        fldl      -88(%rbp)                                     #140.7
..LN895:
        fldl      -112(%rbp)                                    #140.13
..LN897:
        fmulp     %st, %st(1)                                   #140.7
        fstpl     -424(%rbp)                                    #140.7
..LN899:
        movsd     -424(%rbp), %xmm0                             #141.11
        movl      $1, %eax                                      #141.11
        call      cos                                           #141.11
                                # LOE xmm0
..B1.86:                        # Preds ..B1.62
..LN901:
        movsd     %xmm0, -416(%rbp)                             #141.7
..LN903:
        movsd     -424(%rbp), %xmm0                             #142.11
        movl      $1, %eax                                      #142.11
        call      sin                                           #142.11
                                # LOE xmm0
..B1.87:                        # Preds ..B1.86
..LN905:
        movsd     %xmm0, -408(%rbp)                             #142.7
..LN907:
        fldl      -472(%rbp)                                    #143.7
        fldl      -416(%rbp)                                    #143.7
..LN909:
        fmulp     %st, %st(1)                                   #143.23
..LN911:
        fldl      -488(%rbp)                                    #143.7
..LN913:
        faddp     %st, %st(1)                                   #143.19
        fldl      -416(%rbp)                                    #143.19
..LN915:
        fmulp     %st, %st(1)                                   #143.28
..LN917:
        fldl      -496(%rbp)                                    #143.7
..LN919:
        faddp     %st, %st(1)                                   #143.14
        fldl      -464(%rbp)                                    #143.14
        fldl      -416(%rbp)                                    #143.14
..LN921:
        fmulp     %st, %st(1)                                   #143.41
..LN923:
        fldl      -480(%rbp)                                    #143.14
..LN925:
        faddp     %st, %st(1)                                   #143.37
        fldl      -408(%rbp)                                    #143.37
..LN927:
        fmulp     %st, %st(1)                                   #143.46
..LN929:
        faddp     %st, %st(1)                                   #143.7
        fstpl     -216(%rbp)                                    #143.7
..LN931:
        fldl      -216(%rbp)                                    #144.7
..LN933:
        fabs                                                    #144.11
        fldl      -448(%rbp)                                    #144.11
..LN935:
        fabs                                                    #144.26
..LN937:
        fcomip    %st(1), %st                                   #144.21
        fstp      %st(0)                                        #144.21
        jae       ..B1.64       # Prob 50%                      #144.21
        jp        ..B1.64       # Prob 0%                       #144.21
                                # LOE
..B1.63:                        # Preds ..B1.87
..LN939:
        fldl      -216(%rbp)                                    #145.11
        fstpl     -448(%rbp)                                    #145.11
..LN941:
        movl      -20(%rbp), %eax                               #146.11
        movl      %eax, -164(%rbp)                              #146.11
..LN943:
        fldl      -440(%rbp)                                    #147.11
        fstpl     -432(%rbp)                                    #147.11
        jmp       ..B1.66       # Prob 100%                     #147.11
                                # LOE
..B1.64:                        # Preds ..B1.87
..LN945:
        movl      -164(%rbp), %eax                              #148.7
..LN947:
        addl      $1, %eax                                      #148.28
..LN949:
        movl      -20(%rbp), %edx                               #148.7
..LN951:
        cmpl      %eax, %edx                                    #148.18
        jne       ..B1.66       # Prob 50%                      #148.18
                                # LOE
..B1.65:                        # Preds ..B1.64
..LN953:
        fldl      -216(%rbp)                                    #149.11
        fstpl     -400(%rbp)                                    #149.11
                                # LOE
..B1.66:                        # Preds ..B1.63 ..B1.65 ..B1.64
..LN955:
        fldl      -216(%rbp)                                    #151.7
        fstpl     -440(%rbp)                                    #151.7
..LN957:
        addl      $1, -20(%rbp)                                 #139.10
        movl      -20(%rbp), %eax                               #139.10
        movl      -156(%rbp), %edx                              #139.10
        cmpl      %edx, %eax                                    #139.10
        jle       ..B1.62       # Prob 50%                      #139.10
                                # LOE
..B1.67:                        # Preds ..B1.60 ..B1.66
..LN959:
        movl      -164(%rbp), %eax                              #152.7
..LN961:
        testl     %eax, %eax                                    #152.17
        jne       ..B1.69       # Prob 50%                      #152.17
                                # LOE
..B1.68:                        # Preds ..B1.67
..LN963:
        fldl      -216(%rbp)                                    #152.25
        fstpl     -432(%rbp)                                    #152.25
                                # LOE
..B1.69:                        # Preds ..B1.68 ..B1.67
..LN965:
        movl      -164(%rbp), %eax                              #153.7
        movl      -160(%rbp), %edx                              #153.7
..LN967:
        cmpl      %edx, %eax                                    #153.17
        jne       ..B1.71       # Prob 50%                      #153.17
                                # LOE
..B1.70:                        # Preds ..B1.69
..LN969:
        fldl      -456(%rbp)                                    #153.26
        fstpl     -400(%rbp)                                    #153.26
                                # LOE
..B1.71:                        # Preds ..B1.70 ..B1.69
..LN971:
        fldl      -128(%rbp)                                    #154.7
        fstpl     -392(%rbp)                                    #154.7
..LN973:
        fldl      -432(%rbp)                                    #155.7
        fldl      -400(%rbp)                                    #155.7
..LN975:
        fucomip   %st(1), %st                                   #155.17
        fstp      %st(0)                                        #155.17
        jp        ..B1.72       # Prob 0%                       #155.17
        je        ..B1.73       # Prob 50%                      #155.17
                                # LOE
..B1.72:                        # Preds ..B1.71
..LN977:
        fldl      -432(%rbp)                                    #156.11
        fldl      -448(%rbp)                                    #156.11
        fsubrp    %st, %st(1)                                   #156.11
        fstpl     -432(%rbp)                                    #156.11
..LN979:
        fldl      -400(%rbp)                                    #157.11
        fldl      -448(%rbp)                                    #157.11
        fsubrp    %st, %st(1)                                   #157.11
        fstpl     -400(%rbp)                                    #157.11
..LN981:
        fldl      -280(%rbp)                                    #158.11
        fldl      -432(%rbp)                                    #158.11
        fldl      -400(%rbp)                                    #158.11
..LN983:
        fsubrp    %st, %st(1)                                   #158.27
..LN985:
        fmulp     %st, %st(1)                                   #158.20
        fldl      -432(%rbp)                                    #158.20
        fldl      -400(%rbp)                                    #158.20
..LN987:
        faddp     %st, %st(1)                                   #158.41
..LN989:
        fdivrp    %st, %st(1)                                   #158.11
        fstpl     -392(%rbp)                                    #158.11
                                # LOE
..B1.73:                        # Preds ..B1.72 ..B1.71
..LN991:
        fldl      -112(%rbp)                                    #160.7
..LN993:
        movl      -164(%rbp), %eax                              #160.19
        movl      %eax, -376(%rbp)                              #160.19
        fildl     -376(%rbp)                                    #160.19
..LN995:
        fstpl     -88(%rbp)                                     #160.7
        fldl      -88(%rbp)                                     #160.7
..LN997:
        fldl      -392(%rbp)                                    #160.19
..LN999:
        faddp     %st, %st(1)                                   #160.32
..LN1001:
        fmulp     %st, %st(1)                                   #160.7
        fstpl     -424(%rbp)                                    #160.7
..LN1003:
        movsd     -424(%rbp), %xmm0                             #164.11
        movl      $1, %eax                                      #164.11
        call      cos                                           #164.11
                                # LOE xmm0
..B1.88:                        # Preds ..B1.73
..LN1005:
        movsd     %xmm0, -416(%rbp)                             #164.7
..LN1007:
        movsd     -424(%rbp), %xmm0                             #165.11
        movl      $1, %eax                                      #165.11
        call      sin                                           #165.11
                                # LOE xmm0
..B1.89:                        # Preds ..B1.88
..LN1009:
        movsd     %xmm0, -408(%rbp)                             #165.7
..LN1011:
        fldl      -472(%rbp)                                    #166.7
        fldl      -416(%rbp)                                    #166.7
..LN1013:
        fmulp     %st, %st(1)                                   #166.23
..LN1015:
        fldl      -488(%rbp)                                    #166.7
..LN1017:
        faddp     %st, %st(1)                                   #166.19
        fldl      -416(%rbp)                                    #166.19
..LN1019:
        fmulp     %st, %st(1)                                   #166.28
..LN1021:
        fldl      -496(%rbp)                                    #166.7
..LN1023:
        faddp     %st, %st(1)                                   #166.14
        fldl      -464(%rbp)                                    #166.14
        fldl      -416(%rbp)                                    #166.14
..LN1025:
        fmulp     %st, %st(1)                                   #166.41
..LN1027:
        fldl      -480(%rbp)                                    #166.14
..LN1029:
        faddp     %st, %st(1)                                   #166.37
        fldl      -408(%rbp)                                    #166.37
..LN1031:
        fmulp     %st, %st(1)                                   #166.46
..LN1033:
        faddp     %st, %st(1)                                   #166.7
        fstpl     -216(%rbp)                                    #166.7
..LN1035:
        movq      -136(%rbp), %rax                              #167.7
..LN1037:
        movl      (%rax), %eax                                  #167.10
        movl      %eax, -144(%rbp)                              #167.10
        movl      $1, -20(%rbp)                                 #167.10
        movl      -144(%rbp), %eax                              #167.10
        testl     %eax, %eax                                    #167.10
        jle       ..B1.76       # Prob 50%                      #167.10
                                # LOE
..B1.75:                        # Preds ..B1.89 ..B1.75
..LN1039:
        fldl      -416(%rbp)                                    #168.7
        movl      -20(%rbp), %eax                               #168.7
..LN1041:
        movslq    %eax, %rax                                    #168.16
..LN1043:
        movq      48(%rbp), %rdx                                #168.7
..LN1045:
        fldl      -8(%rdx,%rax,8)                               #168.16
..LN1047:
        fmulp     %st, %st(1)                                   #168.15
        fldl      -408(%rbp)                                    #168.15
        movl      -20(%rbp), %eax                               #168.15
..LN1049:
        movslq    %eax, %rax                                    #168.25
..LN1051:
        movq      88(%rbp), %rdx                                #168.15
..LN1053:
        fldl      -8(%rdx,%rax,8)                               #168.25
..LN1055:
        fmulp     %st, %st(1)                                   #168.24
..LN1057:
        faddp     %st, %st(1)                                   #168.7
        movl      -20(%rbp), %eax                               #168.7
        movslq    %eax, %rax                                    #168.7
        movq      48(%rbp), %rdx                                #168.7
        fstpl     -8(%rdx,%rax,8)                               #168.7
..LN1059:
        fldl      -416(%rbp)                                    #169.7
        movl      -20(%rbp), %eax                               #169.7
..LN1061:
        movslq    %eax, %rax                                    #169.17
..LN1063:
        movq      80(%rbp), %rdx                                #169.7
..LN1065:
        fldl      -8(%rdx,%rax,8)                               #169.17
..LN1067:
        fmulp     %st, %st(1)                                   #169.16
        fldl      -408(%rbp)                                    #169.16
        movl      -20(%rbp), %eax                               #169.16
..LN1069:
        movslq    %eax, %rax                                    #169.27
..LN1071:
        movq      96(%rbp), %rdx                                #169.16
..LN1073:
        fldl      -8(%rdx,%rax,8)                               #169.27
..LN1075:
        fmulp     %st, %st(1)                                   #169.26
..LN1077:
        faddp     %st, %st(1)                                   #169.7
        movl      -20(%rbp), %eax                               #169.7
        movslq    %eax, %rax                                    #169.7
        movq      80(%rbp), %rdx                                #169.7
        fstpl     -8(%rdx,%rax,8)                               #169.7
..LN1079:
        movl      -20(%rbp), %eax                               #170.7
..LN1081:
        movslq    %eax, %rax                                    #170.12
..LN1083:
        movq      72(%rbp), %rdx                                #170.7
..LN1085:
        movl      -20(%rbp), %ecx                               #170.12
..LN1087:
        movslq    %ecx, %rcx                                    #170.18
..LN1089:
        movq      80(%rbp), %rbx                                #170.12
        fldl      -8(%rdx,%rax,8)                               #170.12
..LN1091:
        fldl      -8(%rbx,%rcx,8)                               #170.18
..LN1093:
        faddp     %st, %st(1)                                   #170.7
..LN1095:
        movl      -20(%rbp), %eax                               #170.3
..LN1097:
        movslq    %eax, %rax                                    #170.7
..LN1099:
        movq      88(%rbp), %rdx                                #170.3
..LN1101:
        fstpl     -8(%rdx,%rax,8)                               #170.7
..LN1103:
        addl      $1, -20(%rbp)                                 #167.10
        movl      -20(%rbp), %eax                               #167.10
        movl      -144(%rbp), %edx                              #167.10
        cmpl      %edx, %eax                                    #167.10
        jle       ..B1.75       # Prob 50%                      #167.10
                                # LOE
..B1.76:                        # Preds ..B1.89 ..B1.75
..LN1105:
        fldl      -216(%rbp)                                    #171.7
..LN1107:
        fabs                                                    #171.11
..LN1109:
        fldl      _2il0floatpacket.6(%rip)                      #171.31
..LN1111:
        fldl      -456(%rbp)                                    #171.11
..LN1113:
        fabs                                                    #171.32
..LN1115:
        fmulp     %st, %st(1)                                   #171.31
..LN1117:
        fstpl     -88(%rbp)                                     #171.21
        fldl      -88(%rbp)                                     #171.21
        fcomip    %st(1), %st                                   #171.21
        fstp      %st(0)                                        #171.21
        jb        ..B1.78       # Prob 50%                      #171.21
                                # LOE
..B1.77:                        # Preds ..B1.76
..LN1119:
        movq      $0, -520(%rbp)                                #171.46
        jmp       ..B1.80       # Prob 100%                     #171.46
                                # LOE
..B1.78:                        # Preds ..B1.76
..LN1121:
        movq      -136(%rbp), %rax                              #172.7
        movl      -24(%rbp), %edx                               #172.7
        movl      (%rax), %eax                                  #172.7
..LN1123:
        cmpl      %eax, %edx                                    #172.17
        jge       ..B1.80       # Prob 50%                      #172.17
                                # LOE
..B1.79:                        # Preds ..B1.78
..LN1125:
        movq      $0, -528(%rbp)                                #172.25
        jmp       ..B1.39       # Prob 100%                     #172.25
                                # LOE
..B1.80:                        # Preds ..B1.78 ..B1.77 ..B1.43
..LN1127:
        movq      $0, -208(%rbp)                                #173.3
..LN1129:
        movq      -184(%rbp), %rbx                              #174.7
..___tag_value_biglag_.13:                                      #
        leave                                                   #174.7
..___tag_value_biglag_.15:                                      #
        ret                                                     #174.7
        .align    2,0x90
..___tag_value_biglag_.16:                                      #
                                # LOE
# mark_end;
	.type	biglag_,@function
	.size	biglag_,.-biglag_
.LNbiglag_:
	.data
# -- End  biglag_
	.section .rodata, "a"
	.align 8
	.align 8
_2il0floatpacket.1:
	.long	0x00000000,0x3fe00000
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
_2il0floatpacket.2:
	.long	0x00000000,0x40200000
	.type	_2il0floatpacket.2,@object
	.size	_2il0floatpacket.2,8
_2il0floatpacket.3:
	.long	0x7ae147ae,0x3fefae14
	.type	_2il0floatpacket.3,@object
	.size	_2il0floatpacket.3,8
_2il0floatpacket.4:
	.long	0x47ae147b,0x3f847ae1
	.type	_2il0floatpacket.4,@object
	.size	_2il0floatpacket.4,8
_2il0floatpacket.5:
	.long	0xe2308c3a,0x3e45798e
	.type	_2il0floatpacket.5,@object
	.size	_2il0floatpacket.5,8
_2il0floatpacket.6:
	.long	0x9999999a,0x3ff19999
	.type	_2il0floatpacket.6,@object
	.size	_2il0floatpacket.6,8
_2il0floatpacket.7:
	.long	0x00000000,0x3ff00000
	.type	_2il0floatpacket.7,@object
	.size	_2il0floatpacket.7,8
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .debug_info
	.section .debug_info
.debug_info_seg:
	.align 1
	.4byte 0x00000508
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
	.8byte 0x662e67616c676962
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
	.4byte 0x6c676962
	.2byte 0x6761
	.byte 0x00
//	DW_AT_low_pc:
	.8byte biglag_
//	DW_AT_high_pc:
	.8byte .LNbiglag_
//	DW_AT_external:
	.byte 0x01
//	DW_AT_sibling:
	.4byte 0x0000045c
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
	.4byte 0x0000045c
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006e
//	DW_AT_location:
	.4byte 0x7ef87604
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
	.4byte 0x0000045c
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x0074706e
//	DW_AT_location:
	.4byte 0x7d907604
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
	.4byte 0x0000046a
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74706f78
	.byte 0x00
//	DW_AT_location:
	.4byte 0x7d987604
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
	.4byte 0x00000482
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00747078
//	DW_AT_location:
	.4byte 0x7da07604
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
	.4byte 0x00000496
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74616d62
	.byte 0x00
//	DW_AT_location:
	.4byte 0x7da87604
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
	.4byte 0x000004aa
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x74616d7a
	.byte 0x00
//	DW_AT_location:
	.4byte 0x7db07604
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
	.4byte 0x0000045c
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
	.4byte 0x0000045c
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
	.4byte 0x0000045c
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x77656e6b
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
	.4byte 0x00000477
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x746c6564
	.2byte 0x0061
//	DW_AT_location:
	.4byte 0x06287603
.DWinfo13:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x0f
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004be
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
	.byte 0x11
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x68706c61
	.2byte 0x0061
//	DW_AT_location:
	.4byte 0x06387603
.DWinfo15:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x17
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004cb
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6c6f6368
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
	.byte 0x1c
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004d8
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x6367
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
	.byte 0x1f
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004e5
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x6467
	.byte 0x00
//	DW_AT_location:
	.4byte 0x00d07604
	.byte 0x06
.DWinfo18:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x22
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004f2
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0073
//	DW_AT_location:
	.4byte 0x00d87604
	.byte 0x06
.DWinfo19:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x24
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000004ff
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0077
//	DW_AT_location:
	.4byte 0x00e07604
	.byte 0x06
.DWinfo20:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x9a
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cf87603
.DWinfo21:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x95
//	DW_AT_decl_column:
	.byte 0x0b
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706d6574
	.2byte 0x0062
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cf07603
.DWinfo22:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x93
//	DW_AT_decl_column:
	.byte 0x0b
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x706d6574
	.2byte 0x0061
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cd07603
.DWinfo23:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x8e
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00687473
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7ce87603
.DWinfo24:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x8d
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00687463
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7ce07603
.DWinfo25:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x8c
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cd87603
.DWinfo26:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x89
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
	.4byte 0x0000045c
//	DW_AT_location:
	.4byte 0x7ee07603
.DWinfo27:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x88
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
	.4byte 0x0000045c
//	DW_AT_location:
	.4byte 0x7edc7603
.DWinfo28:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x87
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6f756174
	.2byte 0x646c
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cc87603
.DWinfo29:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x86
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6d756174
	.2byte 0x7861
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cc07603
.DWinfo30:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x85
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x62756174
	.2byte 0x6765
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cb87603
.DWinfo31:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x79
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00356663
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7cb07603
.DWinfo32:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x78
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00346663
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7ca87603
.DWinfo33:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x77
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00336663
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7ca07603
.DWinfo34:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x76
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00326663
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7c987603
.DWinfo35:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x75
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00316663
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7c907603
.DWinfo36:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x66
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6f6e6564
	.2byte 0x006d
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7c887603
.DWinfo37:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x5f
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7fa07603
.DWinfo38:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x51
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00756174
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7ea87603
.DWinfo39:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x4d
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6c616373
	.2byte 0x0065
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7ea07603
.DWinfo40:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x48
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00646864
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7e907603
.DWinfo41:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x47
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x7073
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7f987603
.DWinfo42:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x46
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x6767
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7e887603
.DWinfo43:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x39
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006d7573
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7e987603
.DWinfo44:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x32
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0069
//	DW_AT_type:
	.4byte 0x0000045c
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x6c
.DWinfo45:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x31
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7f887603
.DWinfo46:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x28
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7f907603
.DWinfo47:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x27
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006a
//	DW_AT_type:
	.4byte 0x0000045c
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x48
.DWinfo48:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x25
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006b
//	DW_AT_type:
	.4byte 0x0000045c
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x40
.DWinfo49:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x24
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
	.4byte 0x0000045c
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x68
.DWinfo50:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x1f
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
	.4byte 0x0000045c
//	DW_AT_location:
	.4byte 0x7fb87603
.DWinfo51:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x1e
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x736c6564
	.2byte 0x0071
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7e807603
.DWinfo52:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x1d
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7df87603
.DWinfo53:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x1c
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7f807603
.DWinfo54:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x1b
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00656e6f
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7df07603
.DWinfo55:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x1a
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
	.4byte 0x00000477
//	DW_AT_location:
	.4byte 0x7de87603
	.byte 0x00
.DWinfo56:
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
.DWinfo57:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x00000477
.DWinfo58:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo59:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x08
//	DW_AT_encoding:
	.byte 0x04
//	DW_AT_name:
	.8byte 0x002938284c414552
.DWinfo60:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x00000496
.DWinfo61:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7dc07604
	.byte 0x06
.DWinfo62:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo63:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004aa
.DWinfo64:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7dd07604
	.byte 0x06
.DWinfo65:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo66:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004be
.DWinfo67:
//	DW_TAG_subrange_type:
	.byte 0x08
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.4byte 0x7de07604
	.byte 0x06
.DWinfo68:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo69:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004cb
.DWinfo70:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo71:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004d8
.DWinfo72:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo73:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004e5
.DWinfo74:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo75:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004f2
.DWinfo76:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo77:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
//	DW_AT_sibling:
	.4byte 0x000004ff
.DWinfo78:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo79:
//	DW_TAG_array_type:
	.byte 0x09
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000477
.DWinfo80:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
	.byte 0x00
	.byte 0x00
	.byte 0x00
	.byte 0x00
// -- Begin DWARF2 SEGMENT .debug_line
	.section .debug_line
.debug_line_seg:
	.align 1
	.4byte 0x000006eb
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
	.8byte 0x662e67616c676962
	.byte 0x00
	.8byte 0x289505c0a3cde100
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
	.2byte 0x1903
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
	.8byte ..LN15
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN17
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN23
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN25
	.byte 0x0c
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
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN123
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN127
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN153
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN171
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN173
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN185
	.2byte 0x7c03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN187
	.2byte 0x0503
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN191
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN193
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN195
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN199
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN227
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN259
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN261
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN271
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN281
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN285
	.2byte 0x7803
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN287
	.2byte 0x0903
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN319
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN359
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN361
	.2byte 0x0703
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN363
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN365
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN367
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN371
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN381
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN399
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN419
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN421
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN425
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN433
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN435
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN451
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN465
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN481
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN485
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN495
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN505
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN533
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN535
	.2byte 0x0903
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN537
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN539
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN541
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN543
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN547
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN557
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN575
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN587
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN589
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN597
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN611
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN615
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN619
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN641
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN649
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN651
	.2byte 0x0703
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN655
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN657
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN661
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN693
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN695
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN705
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN709
	.2byte 0x7b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN711
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN751
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN753
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN755
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN757
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN759
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN761
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN763
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN767
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN785
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN803
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN821
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN839
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN859
	.2byte 0x7b03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN861
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN863
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN869
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN875
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN877
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN879
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN881
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN883
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN889
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN891
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN899
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN903
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN907
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN931
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN939
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN941
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN943
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN945
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN953
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN955
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN957
	.2byte 0x7403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN959
	.2byte 0x0d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN965
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN971
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN973
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN977
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN979
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN981
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN991
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1003
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1007
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1011
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1035
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1039
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1059
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1079
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1103
	.2byte 0x7d03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1105
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1121
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1127
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1129
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte .LNbiglag_
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
	.byte 0x06
	.byte 0x01
	.byte 0x01
	.byte 0x09
	.byte 0x0b
	.byte 0x49
	.byte 0x13
	.byte 0x01
	.byte 0x13
	.2byte 0x0000
	.byte 0x07
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.2byte 0x0000
	.byte 0x08
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.byte 0x2f
	.byte 0x0a
	.2byte 0x0000
	.byte 0x09
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
	.8byte ..___tag_value_biglag_.2
	.8byte ..___tag_value_biglag_.16-..___tag_value_biglag_.2
	.byte 0x04
	.4byte ..___tag_value_biglag_.9-..___tag_value_biglag_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_biglag_.12-..___tag_value_biglag_.9
	.byte 0x83
	.byte 0x19
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
	.8byte ..___tag_value_biglag_.2
	.8byte ..___tag_value_biglag_.16-..___tag_value_biglag_.2
	.byte 0x04
	.4byte ..___tag_value_biglag_.9-..___tag_value_biglag_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_biglag_.12-..___tag_value_biglag_.9
	.byte 0x83
	.byte 0x19
	.4byte 0x00000000
	.2byte 0x0000
	.byte 0x00
	.section .text
.LNDBG_TXe:
# End
