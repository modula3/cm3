	.section .text
.LNDBG_TX:
# -- Machine type EFI2
# mark_description "Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 10.0    Build 20070809 %s";
# mark_description "-g -S -W1 -recursive -reentrancy threaded";
	.file "newuoa.f"
	.data
	.text
..TXTST0:
# -- Begin  newuoa_
# mark_begin;
       .align    2,0x90
	.globl newuoa_
newuoa_:
# parameter 1(n): %rdi
# parameter 2(npt): %rsi
# parameter 3(x): %rdx
# parameter 4(rhobeg): %rcx
# parameter 5(rhoend): %r8
# parameter 6(iprint): %r9
# parameter 7(maxfun): 16 + %rbp
# parameter 8(w): 24 + %rbp
# parameter 9(calfun): 32 + %rbp
# parameter 10(f): 40 + %rbp
# parameter 11(info): 48 + %rbp
# parameter 12(itag): 56 + %rbp
..B1.1:                         # Preds ..B1.0
..___tag_value_newuoa_.2:                                       #
..LN1:
        pushq     %rbp                                          #1.18
        movq      %rsp, %rbp                                    #1.18
..___tag_value_newuoa_.9:                                       #
        subq      $160, %rsp                                    #1.18
        movq      %rbx, -16(%rbp)                               #1.18
..___tag_value_newuoa_.12:                                      #
        movq      %rdi, -72(%rbp)                               #1.18
        movq      %rsi, -64(%rbp)                               #1.18
        movq      %rdx, -56(%rbp)                               #1.18
        movq      %rcx, -48(%rbp)                               #1.18
        movq      %r8, -40(%rbp)                                #1.18
        movq      %r9, -32(%rbp)                                #1.18
..LN3:
        movq      -72(%rbp), %rax                               #42.7
        movl      (%rax), %eax                                  #42.7
        addl      $1, %eax                                      #42.7
        movl      %eax, -8(%rbp)                                #42.7
..LN5:
        movq      -64(%rbp), %rax                               #43.7
        movl      -8(%rbp), %edx                                #43.7
        negl      %edx                                          #43.7
        addl      (%rax), %edx                                  #43.7
        movl      %edx, -4(%rbp)                                #43.7
..LN7:
        movq      -64(%rbp), %rax                               #44.7
        movq      -72(%rbp), %rdx                               #44.7
        movl      (%rdx), %edx                                  #44.7
..LN9:
        addl      $2, %edx                                      #44.21
..LN11:
        movl      (%rax), %eax                                  #44.7
..LN13:
        cmpl      %edx, %eax                                    #44.15
        jl        ..B1.3        # Prob 50%                      #44.15
                                # LOE
..B1.2:                         # Preds ..B1.1
        movq      -64(%rbp), %rax                               #44.15
        movq      -72(%rbp), %rdx                               #44.15
        movl      (%rdx), %edx                                  #44.15
..LN15:
        addl      $2, %edx                                      #44.41
..LN17:
        imull     -8(%rbp), %edx                                #44.44
..LN19:
        movq      -72(%rbp), %rcx                               #44.15
        movl      (%rcx), %ecx                                  #44.15
..LN21:
        addl      $2, %ecx                                      #44.41
..LN23:
        imull     -8(%rbp), %ecx                                #44.44
..LN25:
        shrl      $31, %ecx                                     #44.24
..LN27:
        addl      %ecx, %edx                                    #44.48
        sarl      $1, %edx                                      #44.48
..LN29:
        movl      (%rax), %eax                                  #44.15
..LN31:
        cmpl      %edx, %eax                                    #44.33
        jle       ..B1.4        # Prob 50%                      #44.33
                                # LOE
..B1.3:                         # Preds ..B1.1 ..B1.2
..LN33:
        movq      $0, -80(%rbp)                                 #48.17
        jmp       ..B1.5        # Prob 100%                     #48.17
                                # LOE
..B1.4:                         # Preds ..B1.2
..LN35:
        movq      -64(%rbp), %rax                               #50.7
        movq      -72(%rbp), %rdx                               #50.7
        movl      (%rdx), %edx                                  #50.7
        addl      (%rax), %edx                                  #50.7
        movl      %edx, -136(%rbp)                              #50.7
..LN37:
        movl      $1, -132(%rbp)                                #51.7
..LN39:
        movq      -72(%rbp), %rax                               #52.7
        movl      (%rax), %eax                                  #52.7
        addl      -132(%rbp), %eax                              #52.7
        movl      %eax, -128(%rbp)                              #52.7
..LN41:
        movq      -72(%rbp), %rax                               #53.7
        movl      (%rax), %eax                                  #53.7
        addl      -128(%rbp), %eax                              #53.7
        movl      %eax, -124(%rbp)                              #53.7
..LN43:
        movq      -72(%rbp), %rax                               #54.7
        movl      (%rax), %eax                                  #54.7
        addl      -124(%rbp), %eax                              #54.7
        movl      %eax, -120(%rbp)                              #54.7
..LN45:
        movq      -64(%rbp), %rax                               #55.7
        movq      -72(%rbp), %rdx                               #55.7
        movl      (%rax), %eax                                  #55.7
..LN47:
        imull     (%rdx), %eax                                  #55.16
..LN49:
        addl      -120(%rbp), %eax                              #55.7
        movl      %eax, -116(%rbp)                              #55.7
..LN51:
        movq      -64(%rbp), %rax                               #56.7
        movl      (%rax), %eax                                  #56.7
        addl      -116(%rbp), %eax                              #56.7
        movl      %eax, -112(%rbp)                              #56.7
..LN53:
        movq      -72(%rbp), %rax                               #57.7
        movl      (%rax), %eax                                  #57.7
        addl      -112(%rbp), %eax                              #57.7
        movl      %eax, -108(%rbp)                              #57.7
..LN55:
        movq      -72(%rbp), %rax                               #58.7
        movl      -8(%rbp), %edx                                #58.7
..LN57:
        imull     (%rax), %edx                                  #58.17
..LN59:
        movq      -72(%rbp), %rax                               #58.7
        movl      -8(%rbp), %ecx                                #58.7
..LN61:
        imull     (%rax), %ecx                                  #58.17
..LN63:
        shrl      $31, %ecx                                     #58.7
..LN65:
        addl      %ecx, %edx                                    #58.21
        sarl      $1, %edx                                      #58.21
..LN67:
        addl      -108(%rbp), %edx                              #58.7
        movl      %edx, -104(%rbp)                              #58.7
..LN69:
        movq      -64(%rbp), %rax                               #59.7
        movl      (%rax), %eax                                  #59.7
        addl      -104(%rbp), %eax                              #59.7
        movl      %eax, -100(%rbp)                              #59.7
..LN71:
        movq      -72(%rbp), %rax                               #60.7
        movl      (%rax), %eax                                  #60.7
..LN73:
        imull     -136(%rbp), %eax                              #60.23
..LN75:
        addl      -100(%rbp), %eax                              #60.7
        movl      %eax, -96(%rbp)                               #60.7
..LN77:
        movq      -64(%rbp), %rax                               #61.7
        movl      -4(%rbp), %edx                                #61.7
..LN79:
        imull     (%rax), %edx                                  #61.19
..LN81:
        addl      -96(%rbp), %edx                               #61.7
        movl      %edx, -92(%rbp)                               #61.7
..LN83:
        movq      -72(%rbp), %rax                               #62.7
        movl      (%rax), %eax                                  #62.7
        addl      -92(%rbp), %eax                               #62.7
        movl      %eax, -88(%rbp)                               #62.7
..LN85:
        movl      -136(%rbp), %eax                              #63.7
        addl      -88(%rbp), %eax                               #63.7
        movl      %eax, -84(%rbp)                               #63.7
..LN87:
        addq      $-160, %rsp                                   #69.12
        movq      -72(%rbp), %rax                               #69.12
..LN89:
        movq      -64(%rbp), %rdx                               #69.20
..LN91:
        movq      -56(%rbp), %rcx                               #69.22
..LN93:
        movq      -48(%rbp), %rbx                               #69.26
..LN95:
        movq      -40(%rbp), %rsi                               #69.28
..LN97:
        movq      -32(%rbp), %rdi                               #69.35
..LN99:
        movq      16(%rbp), %r8                                 #69.42
        movq      %r8, (%rsp)                                   #69.42
..LN101:
        movl      -132(%rbp), %r8d                              #69.49
..LN103:
        movslq    %r8d, %r8                                     #69.56
..LN105:
        movq      24(%rbp), %r9                                 #69.49
..LN107:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 8(%rsp)                                  #69.12
..LN109:
        movl      -128(%rbp), %r8d                              #69.56
..LN111:
        movslq    %r8d, %r8                                     #70.9
..LN113:
        movq      24(%rbp), %r9                                 #69.56
..LN115:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 16(%rsp)                                 #69.12
..LN117:
        movl      -124(%rbp), %r8d                              #70.9
..LN119:
        movslq    %r8d, %r8                                     #70.16
..LN121:
        movq      24(%rbp), %r9                                 #70.9
..LN123:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 24(%rsp)                                 #69.12
..LN125:
        movl      -120(%rbp), %r8d                              #70.16
..LN127:
        movslq    %r8d, %r8                                     #70.23
..LN129:
        movq      24(%rbp), %r9                                 #70.16
..LN131:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 32(%rsp)                                 #69.12
..LN133:
        movl      -116(%rbp), %r8d                              #70.23
..LN135:
        movslq    %r8d, %r8                                     #70.30
..LN137:
        movq      24(%rbp), %r9                                 #70.23
..LN139:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 40(%rsp)                                 #69.12
..LN141:
        movl      -112(%rbp), %r8d                              #70.30
..LN143:
        movslq    %r8d, %r8                                     #70.37
..LN145:
        movq      24(%rbp), %r9                                 #70.30
..LN147:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 48(%rsp)                                 #69.12
..LN149:
        movl      -108(%rbp), %r8d                              #70.37
..LN151:
        movslq    %r8d, %r8                                     #70.44
..LN153:
        movq      24(%rbp), %r9                                 #70.37
..LN155:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 56(%rsp)                                 #69.12
..LN157:
        movl      -104(%rbp), %r8d                              #70.44
..LN159:
        movslq    %r8d, %r8                                     #70.51
..LN161:
        movq      24(%rbp), %r9                                 #70.44
..LN163:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 64(%rsp)                                 #69.12
..LN165:
        movl      -100(%rbp), %r8d                              #70.51
..LN167:
        movslq    %r8d, %r8                                     #70.58
..LN169:
        movq      24(%rbp), %r9                                 #70.51
..LN171:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 72(%rsp)                                 #69.12
..LN173:
        movl      -96(%rbp), %r8d                               #70.58
..LN175:
        movslq    %r8d, %r8                                     #71.12
..LN177:
        movq      24(%rbp), %r9                                 #70.58
..LN179:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 80(%rsp)                                 #69.12
..LN181:
        lea       -136(%rbp), %r8                               #71.21
        movq      %r8, 88(%rsp)                                 #71.21
        movl      -92(%rbp), %r8d                               #71.21
..LN183:
        movslq    %r8d, %r8                                     #71.26
..LN185:
        movq      24(%rbp), %r9                                 #71.21
..LN187:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 96(%rsp)                                 #69.12
..LN189:
        movl      -88(%rbp), %r8d                               #71.26
..LN191:
        movslq    %r8d, %r8                                     #71.32
..LN193:
        movq      24(%rbp), %r9                                 #71.26
..LN195:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 104(%rsp)                                #69.12
..LN197:
        movl      -84(%rbp), %r8d                               #71.32
..LN199:
        movslq    %r8d, %r8                                     #71.39
..LN201:
        movq      24(%rbp), %r9                                 #71.32
..LN203:
        lea       -8(%r9,%r8,8), %r8                            #69.12
        movq      %r8, 112(%rsp)                                #69.12
..LN205:
        movq      40(%rbp), %r8                                 #71.39
        movq      %r8, 120(%rsp)                                #71.39
..LN207:
        movq      48(%rbp), %r8                                 #71.46
        movq      %r8, 128(%rsp)                                #71.46
..LN209:
        lea       -160(%rbp), %r8                               #71.55
        movq      %r8, 136(%rsp)                                #71.55
        movq      32(%rbp), %r8                                 #71.55
        movq      %r8, 144(%rsp)                                #71.55
..LN211:
        movq      56(%rbp), %r8                                 #71.64
        movq      %r8, 152(%rsp)                                #71.64
..LN213:
        movq      %rdi, -152(%rbp)                              #69.12
        movq      %rax, %rdi                                    #69.12
        movq      %rsi, -144(%rbp)                              #69.12
        movq      %rdx, %rsi                                    #69.12
        movq      %rcx, %rdx                                    #69.12
        movq      %rbx, %rcx                                    #69.12
        movq      -144(%rbp), %rax                              #69.12
        movq      %rax, %r8                                     #69.12
        movq      -152(%rbp), %rax                              #69.12
        movq      %rax, %r9                                     #69.12
        xorl      %eax, %eax                                    #69.12
        call      newuob_                                       #69.12
                                # LOE
..B1.8:                         # Preds ..B1.4
        addq      $160, %rsp                                    #69.12
                                # LOE
..B1.5:                         # Preds ..B1.8 ..B1.3
..LN215:
        movq      $0, -24(%rbp)                                 #73.4
..LN217:
        movq      -16(%rbp), %rbx                               #74.7
..___tag_value_newuoa_.13:                                      #
        leave                                                   #74.7
..___tag_value_newuoa_.15:                                      #
        ret                                                     #74.7
        .align    2,0x90
..___tag_value_newuoa_.16:                                      #
                                # LOE
# mark_end;
	.type	newuoa_,@function
	.size	newuoa_,.-newuoa_
.LNnewuoa_:
	.section .rodata, "a"
	.align 32
	.align 32
newuoa_$BLK$format_pack:
	.byte	54
	.byte	0
	.byte	0
	.byte	0
	.byte	71
	.byte	0
	.byte	1
	.byte	0
	.byte	14
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	28
	.byte	0
	.byte	40
	.byte	0
	.byte	82
	.byte	101
	.byte	116
	.byte	117
	.byte	114
	.byte	110
	.byte	32
	.byte	102
	.byte	114
	.byte	111
	.byte	109
	.byte	32
	.byte	78
	.byte	69
	.byte	87
	.byte	85
	.byte	79
	.byte	65
	.byte	32
	.byte	98
	.byte	101
	.byte	99
	.byte	97
	.byte	117
	.byte	115
	.byte	101
	.byte	32
	.byte	78
	.byte	80
	.byte	84
	.byte	32
	.byte	105
	.byte	115
	.byte	32
	.byte	110
	.byte	111
	.byte	116
	.byte	32
	.byte	105
	.byte	110
	.byte	28
	.byte	0
	.byte	22
	.byte	0
	.byte	32
	.byte	116
	.byte	104
	.byte	101
	.byte	32
	.byte	114
	.byte	101
	.byte	113
	.byte	117
	.byte	105
	.byte	114
	.byte	101
	.byte	100
	.byte	32
	.byte	105
	.byte	110
	.byte	116
	.byte	101
	.byte	114
	.byte	118
	.byte	97
	.byte	108
	.byte	0
	.byte	0
	.byte	55
	.byte	0
	.byte	0
	.byte	0
	.data
# -- End  newuoa_
	.section .rodata, "a"
	.align 32
STRLITPACK_0:
	.byte	32
	.byte	116
	.byte	104
	.byte	101
	.byte	32
	.byte	114
	.byte	101
	.byte	113
	.byte	117
	.byte	105
	.byte	114
	.byte	101
	.byte	100
	.byte	32
	.byte	105
	.byte	110
	.byte	116
	.byte	101
	.byte	114
	.byte	118
	.byte	97
	.byte	108
	.byte	0
	.type	STRLITPACK_0,@object
	.size	STRLITPACK_0,23
	.space 1	# pad
STRLITPACK_1:
	.byte	82
	.byte	101
	.byte	116
	.byte	117
	.byte	114
	.byte	110
	.byte	32
	.byte	102
	.byte	114
	.byte	111
	.byte	109
	.byte	32
	.byte	78
	.byte	69
	.byte	87
	.byte	85
	.byte	79
	.byte	65
	.byte	32
	.byte	98
	.byte	101
	.byte	99
	.byte	97
	.byte	117
	.byte	115
	.byte	101
	.byte	32
	.byte	78
	.byte	80
	.byte	84
	.byte	32
	.byte	105
	.byte	115
	.byte	32
	.byte	110
	.byte	111
	.byte	116
	.byte	32
	.byte	105
	.byte	110
	.byte	0
	.type	STRLITPACK_1,@object
	.size	STRLITPACK_1,41
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .debug_info
	.section .debug_info
.debug_info_seg:
	.align 1
	.4byte 0x000002f3
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
	.8byte 0x662e616f7577656e
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
	.4byte 0x7577656e
	.2byte 0x616f
	.byte 0x00
//	DW_AT_low_pc:
	.8byte newuoa_
//	DW_AT_high_pc:
	.8byte .LNnewuoa_
//	DW_AT_external:
	.byte 0x01
//	DW_AT_sibling:
	.4byte 0x000002b7
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
	.4byte 0x000002b7
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006e
//	DW_AT_location:
	.4byte 0x7fb87604
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
	.4byte 0x000002b7
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x0074706e
//	DW_AT_location:
	.4byte 0x06407603
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
	.4byte 0x000002c5
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0078
//	DW_AT_location:
	.4byte 0x06487603
.DWinfo6:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x22
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002d2
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x626f6872
	.2byte 0x6765
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06507603
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
	.4byte 0x000002d2
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x656f6872
	.2byte 0x646e
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06587603
.DWinfo8:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x30
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x69727069
	.2byte 0x746e
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06607603
.DWinfo9:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x37
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6678616d
	.2byte 0x6e75
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06107603
.DWinfo10:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x3e
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002dd
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0077
//	DW_AT_location:
	.4byte 0x06187603
.DWinfo11:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x40
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002d2
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x666c6163
	.2byte 0x6e75
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06207603
.DWinfo12:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x47
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002d2
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0066
//	DW_AT_location:
	.4byte 0x06287603
.DWinfo13:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x0b
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6f666e69
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06307603
.DWinfo14:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x02
//	DW_AT_decl_column:
	.byte 0x12
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002ea
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x67617469
	.byte 0x00
//	DW_AT_location:
	.4byte 0x06387603
.DWinfo15:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x47
//	DW_AT_decl_column:
	.byte 0x37
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.8byte 0x0074656772617466
//	DW_AT_type:
	.4byte 0x000002d2
//	DW_AT_location:
	.4byte 0x7ee07603
.DWinfo16:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x3f
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x7769
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7fac7603
.DWinfo17:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x3e
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006c7669
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7fa87603
.DWinfo18:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x3d
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x6469
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7fa47603
.DWinfo19:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x3c
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x616d7a69
	.2byte 0x0074
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7fa07603
.DWinfo20:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x3b
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x616d6269
	.2byte 0x0074
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f9c7603
.DWinfo21:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x3a
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00717069
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f987603
.DWinfo22:
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
	.4byte 0x00716869
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f947603
.DWinfo23:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x38
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00716769
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f907603
.DWinfo24:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x37
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00766669
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f8c7603
.DWinfo25:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x36
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00707869
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f887603
.DWinfo26:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x35
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006e7869
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f847603
.DWinfo27:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x34
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006f7869
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7f807603
.DWinfo28:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x33
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x00627869
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7efc7603
.DWinfo29:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x32
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6d69646e
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.4byte 0x7ef87603
.DWinfo30:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x2b
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
	.4byte 0x000002b7
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x7c
.DWinfo31:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x2a
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x706e
	.byte 0x00
//	DW_AT_type:
	.4byte 0x000002b7
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x78
	.byte 0x00
.DWinfo32:
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
.DWinfo33:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002d2
//	DW_AT_sibling:
	.4byte 0x000002d2
.DWinfo34:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo35:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x08
//	DW_AT_encoding:
	.byte 0x04
//	DW_AT_name:
	.8byte 0x002938284c414552
.DWinfo36:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002d2
//	DW_AT_sibling:
	.4byte 0x000002ea
.DWinfo37:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo38:
//	DW_TAG_array_type:
	.byte 0x08
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x000002b7
.DWinfo39:
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
	.4byte 0x0000029d
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
	.8byte 0x662e616f7577656e
	.byte 0x00
	.8byte 0x1ac905c0aee5e300
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
	.2byte 0x2903
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
	.8byte ..LN33
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN35
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN37
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN39
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN41
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN43
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN45
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN51
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN53
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN55
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN69
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN71
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN77
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN83
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN85
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN87
	.2byte 0x0603
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN111
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN113
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN117
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN123
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN125
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN131
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN133
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN139
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN141
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN147
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN149
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN155
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN157
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN163
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN165
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN171
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN173
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN175
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN177
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN179
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN181
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN187
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN189
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN195
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN197
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN203
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN205
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN213
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN215
	.2byte 0x0403
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN217
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte .LNnewuoa_
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
	.8byte ..___tag_value_newuoa_.2
	.8byte ..___tag_value_newuoa_.16-..___tag_value_newuoa_.2
	.byte 0x04
	.4byte ..___tag_value_newuoa_.9-..___tag_value_newuoa_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_newuoa_.12-..___tag_value_newuoa_.9
	.byte 0x83
	.byte 0x04
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
	.8byte ..___tag_value_newuoa_.2
	.8byte ..___tag_value_newuoa_.16-..___tag_value_newuoa_.2
	.byte 0x04
	.4byte ..___tag_value_newuoa_.9-..___tag_value_newuoa_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_newuoa_.12-..___tag_value_newuoa_.9
	.byte 0x83
	.byte 0x04
	.4byte 0x00000000
	.2byte 0x0000
	.byte 0x00
	.section .text
.LNDBG_TXe:
# End
