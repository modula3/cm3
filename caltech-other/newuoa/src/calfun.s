	.section .text
.LNDBG_TX:
# -- Machine type EFI2
# mark_description "Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 10.0    Build 20070809 %s";
# mark_description "-g -S -W1 -recursive -reentrancy threaded";
	.file "calfun.f"
	.data
	.text
..TXTST0:
# -- Begin  calfun_
# mark_begin;
       .align    2,0x90
	.globl calfun_
calfun_:
# parameter 1(n): %rdi
# parameter 2(x): %rsi
..B1.1:                         # Preds ..B1.0
..___tag_value_calfun_.2:                                       #
..LN1:
        pushq     %rbp                                          #1.21
        movq      %rsp, %rbp                                    #1.21
..___tag_value_calfun_.9:                                       #
        subq      $896, %rsp                                    #1.21
        movq      %rdi, -96(%rbp)                               #1.21
        movq      %rsi, -88(%rbp)                               #1.21
..LN3:
        movq      -96(%rbp), %rax                               #4.7
..LN5:
        movl      (%rax), %eax                                  #4.10
        movl      %eax, -48(%rbp)                               #4.10
        movl      $1, -44(%rbp)                                 #4.10
        movl      -48(%rbp), %eax                               #4.10
        testl     %eax, %eax                                    #4.10
        jle       ..B1.4        # Prob 50%                      #4.10
                                # LOE
..B1.3:                         # Preds ..B1.1 ..B1.3
..LN7:
        movq      $0x3ff0000000000000, %rax                     #5.7
        movl      -44(%rbp), %edx                               #5.7
        movslq    %edx, %rdx                                    #5.7
        lea       (%rdx,%rdx,4), %rdx                           #5.7
        shlq      $4, %rdx                                      #5.7
        movq      %rax, -976(%rbp,%rdx)                         #5.7
..LN9:
        fldl      _2il0floatpacket.1(%rip)                      #6.19
..LN11:
        movl      -44(%rbp), %eax                               #6.7
..LN13:
        movslq    %eax, %rax                                    #6.20
..LN15:
        movq      -88(%rbp), %rdx                               #6.7
..LN17:
        fldl      -8(%rdx,%rax,8)                               #6.20
..LN19:
        fmulp     %st, %st(1)                                   #6.19
..LN21:
        fld1                                                    #6.7
        fsubrp    %st, %st(1)                                   #6.7
..LN23:
        movl      -44(%rbp), %eax                               #6.4
        movslq    %eax, %rax                                    #6.4
..LN25:
        lea       (%rax,%rax,4), %rax                           #6.7
        shlq      $4, %rax                                      #6.7
        fstpl     -968(%rbp,%rax)                               #6.7
..LN27:
        addl      $1, -44(%rbp)                                 #4.10
        movl      -44(%rbp), %eax                               #4.10
        movl      -48(%rbp), %edx                               #4.10
        cmpl      %edx, %eax                                    #4.10
        jle       ..B1.3        # Prob 50%                      #4.10
                                # LOE
..B1.4:                         # Preds ..B1.1 ..B1.3
..LN29:
        movq      -96(%rbp), %rax                               #7.7
..LN31:
        movl      (%rax), %eax                                  #7.10
        movl      %eax, -40(%rbp)                               #7.10
        movl      $2, -36(%rbp)                                 #7.10
        movl      -40(%rbp), %eax                               #7.10
        cmpl      $2, %eax                                      #7.10
        jl        ..B1.10       # Prob 50%                      #7.10
                                # LOE
..B1.6:                         # Preds ..B1.4 ..B1.7
..LN33:
        movq      -96(%rbp), %rax                               #8.7
..LN35:
        movl      (%rax), %eax                                  #8.10
        movl      %eax, -16(%rbp)                               #8.10
        movl      $1, -44(%rbp)                                 #8.10
        movl      -16(%rbp), %eax                               #8.10
        testl     %eax, %eax                                    #8.10
        jg        ..B1.9        # Prob 50%                      #8.10
                                # LOE
..B1.7:                         # Preds ..B1.6 ..B1.9
..LN37:
        addl      $1, -36(%rbp)                                 #7.10
        movl      -36(%rbp), %eax                               #7.10
        movl      -40(%rbp), %edx                               #7.10
        cmpl      %edx, %eax                                    #7.10
        jle       ..B1.6        # Prob 50%                      #7.10
        jmp       ..B1.10       # Prob 100%                     #7.10
                                # LOE
..B1.9:                         # Preds ..B1.6 ..B1.9
..LN39:
        fldl      _2il0floatpacket.1(%rip)                      #9.21
..LN41:
        movl      -44(%rbp), %eax                               #9.7
        movslq    %eax, %rax                                    #9.7
..LN43:
        lea       (%rax,%rax,4), %rax                           #9.22
        shlq      $4, %rax                                      #9.22
        fldl      -968(%rbp,%rax)                               #9.22
..LN45:
        fmulp     %st, %st(1)                                   #9.21
..LN47:
        lea       -896(%rbp), %rax                              #9.29
..LN49:
        movl      -44(%rbp), %edx                               #9.21
        movslq    %edx, %rdx                                    #9.21
..LN51:
        lea       (%rdx,%rdx,4), %rdx                           #9.29
        shlq      $4, %rdx                                      #9.29
..LN53:
        movq      %rax, %rcx                                    #9.7
        addq      %rdx, %rcx                                    #9.7
..LN55:
        movl      -36(%rbp), %edx                               #9.21
..LN57:
        movslq    %edx, %rdx                                    #9.29
        fldl      -88(%rcx,%rdx,8)                              #9.29
..LN59:
        fmulp     %st, %st(1)                                   #9.28
        movl      -44(%rbp), %edx                               #9.28
        movslq    %edx, %rdx                                    #9.28
..LN61:
        lea       (%rdx,%rdx,4), %rdx                           #9.36
        shlq      $4, %rdx                                      #9.36
..LN63:
        movq      %rax, %rcx                                    #9.7
        addq      %rdx, %rcx                                    #9.7
..LN65:
        movl      -36(%rbp), %edx                               #9.28
        addl      $-1, %edx                                     #9.28
..LN67:
        movslq    %edx, %rdx                                    #9.36
        fldl      -88(%rcx,%rdx,8)                              #9.36
..LN69:
        fsubrp    %st, %st(1)                                   #9.7
..LN71:
        movl      -44(%rbp), %edx                               #9.4
        movslq    %edx, %rdx                                    #9.4
..LN73:
        lea       (%rdx,%rdx,4), %rdx                           #9.7
        shlq      $4, %rdx                                      #9.7
        addq      %rdx, %rax                                    #9.7
..LN75:
        movl      -36(%rbp), %edx                               #9.4
        addl      $1, %edx                                      #9.4
..LN77:
        movslq    %edx, %rdx                                    #9.7
        fstpl     -88(%rax,%rdx,8)                              #9.7
..LN79:
        addl      $1, -44(%rbp)                                 #8.10
        movl      -44(%rbp), %eax                               #8.10
        movl      -16(%rbp), %edx                               #8.10
        cmpl      %edx, %eax                                    #8.10
        jle       ..B1.9        # Prob 50%                      #8.10
        jmp       ..B1.7        # Prob 100%                     #8.10
                                # LOE
..B1.10:                        # Preds ..B1.4 ..B1.7
..LN81:
        movq      $0, -80(%rbp)                                 #10.7
..LN83:
        movq      -96(%rbp), %rax                               #11.7
        movl      (%rax), %eax                                  #11.7
        addl      $1, %eax                                      #11.7
        movl      %eax, -32(%rbp)                               #11.7
..LN85:
        movl      $1, %eax                                      #12.7
        movl      %eax, -28(%rbp)                               #12.7
..LN87:
        movl      -32(%rbp), %edx                               #13.10
        movl      %edx, -24(%rbp)                               #13.10
        movl      %eax, -36(%rbp)                               #13.10
        movl      -24(%rbp), %eax                               #13.10
        testl     %eax, %eax                                    #13.10
        jle       ..B1.18       # Prob 50%                      #13.10
                                # LOE
..B1.12:                        # Preds ..B1.10 ..B1.17
..LN89:
        movq      $0, -72(%rbp)                                 #14.7
..LN91:
        movq      -96(%rbp), %rax                               #15.7
..LN93:
        movl      (%rax), %eax                                  #15.10
        movl      %eax, -12(%rbp)                               #15.10
        movl      $1, -44(%rbp)                                 #15.10
        movl      -12(%rbp), %eax                               #15.10
        testl     %eax, %eax                                    #15.10
        jle       ..B1.15       # Prob 50%                      #15.10
                                # LOE
..B1.14:                        # Preds ..B1.12 ..B1.14
..LN95:
        lea       -896(%rbp), %rax                              #16.15
..LN97:
        movl      -44(%rbp), %edx                               #16.4
        movslq    %edx, %rdx                                    #16.4
..LN99:
        lea       (%rdx,%rdx,4), %rdx                           #16.15
        shlq      $4, %rdx                                      #16.15
..LN101:
        addq      %rdx, %rax                                    #16.7
..LN103:
        movl      -36(%rbp), %edx                               #16.4
..LN105:
        movslq    %edx, %rdx                                    #16.15
..LN107:
        fldl      -72(%rbp)                                     #16.4
..LN109:
        fldl      -88(%rax,%rdx,8)                              #16.15
..LN111:
        faddp     %st, %st(1)                                   #16.7
        fstpl     -72(%rbp)                                     #16.7
..LN113:
        addl      $1, -44(%rbp)                                 #15.10
        movl      -44(%rbp), %eax                               #15.10
        movl      -12(%rbp), %edx                               #15.10
        cmpl      %edx, %eax                                    #15.10
        jle       ..B1.14       # Prob 50%                      #15.10
                                # LOE
..B1.15:                        # Preds ..B1.12 ..B1.14
..LN115:
        fldl      -72(%rbp)                                     #17.7
        movq      -96(%rbp), %rax                               #17.7
..LN117:
        movl      (%rax), %eax                                  #17.15
        movl      %eax, -64(%rbp)                               #17.15
        fildl     -64(%rbp)                                     #17.15
..LN119:
        fstpl     -56(%rbp)                                     #17.7
        fldl      -56(%rbp)                                     #17.7
        fdivrp    %st, %st(1)                                   #17.7
        fstpl     -72(%rbp)                                     #17.7
..LN121:
        movl      -28(%rbp), %eax                               #18.7
..LN123:
        testl     %eax, %eax                                    #18.14
        jle       ..B1.17       # Prob 50%                      #18.14
                                # LOE
..B1.16:                        # Preds ..B1.15
..LN125:
        fld1                                                    #18.22
        movl      -36(%rbp), %eax                               #18.22
..LN127:
        imull     -36(%rbp), %eax                               #18.44
        movl      -36(%rbp), %edx                               #18.44
..LN129:
        addl      %edx, %edx                                    #18.48
..LN131:
        negl      %edx                                          #18.36
        addl      %edx, %eax                                    #18.36
        movl      %eax, -64(%rbp)                               #18.36
        fildl     -64(%rbp)                                     #18.36
..LN133:
        fstpl     -56(%rbp)                                     #18.22
        fldl      -56(%rbp)                                     #18.22
..LN135:
        fdivrp    %st, %st(1)                                   #18.35
..LN137:
        fldl      -72(%rbp)                                     #18.22
        faddp     %st, %st(1)                                   #18.22
        fstpl     -72(%rbp)                                     #18.22
                                # LOE
..B1.17:                        # Preds ..B1.16 ..B1.15
..LN139:
        movl      -28(%rbp), %eax                               #19.7
        negl      %eax                                          #19.7
        movl      %eax, -28(%rbp)                               #19.7
..LN141:
        fldl      -72(%rbp)                                     #20.4
        fldl      -72(%rbp)                                     #20.4
..LN143:
        fmulp     %st, %st(1)                                   #20.14
..LN145:
        fldl      -80(%rbp)                                     #20.4
..LN147:
        faddp     %st, %st(1)                                   #20.7
        fstpl     -80(%rbp)                                     #20.7
..LN149:
        addl      $1, -36(%rbp)                                 #13.10
        movl      -36(%rbp), %eax                               #13.10
        movl      -24(%rbp), %edx                               #13.10
        cmpl      %edx, %eax                                    #13.10
        jle       ..B1.12       # Prob 50%                      #13.10
                                # LOE
..B1.18:                        # Preds ..B1.10 ..B1.17
..LN151:
        fldl      -80(%rbp)                                     #21.14
..LN153:
        fstps     -56(%rbp)                                     #21.7
        movss     -56(%rbp), %xmm0                              #21.7
        movss     %xmm0, -20(%rbp)                              #21.7
..LN155:
        flds      -20(%rbp)                                     #22.7
        fstps     -56(%rbp)                                     #22.7
        movss     -56(%rbp), %xmm0                              #22.7
        leave                                                   #22.7
..___tag_value_calfun_.13:                                      #
        ret                                                     #22.7
        .align    2,0x90
..___tag_value_calfun_.14:                                      #
                                # LOE
# mark_end;
	.type	calfun_,@function
	.size	calfun_,.-calfun_
.LNcalfun_:
	.data
# -- End  calfun_
	.section .rodata, "a"
	.align 8
	.align 8
_2il0floatpacket.1:
	.long	0x00000000,0x40000000
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
_2il0floatpacket.2:
	.long	0x00000000,0x3ff00000
	.type	_2il0floatpacket.2,@object
	.size	_2il0floatpacket.2,8
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .debug_info
	.section .debug_info
.debug_info_seg:
	.align 1
	.4byte 0x0000018d
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
	.8byte 0x662e6e75666c6163
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
	.byte 0x04
//	DW_AT_encoding:
	.byte 0x04
//	DW_AT_name:
	.8byte 0x002934284c414552
.DWinfo1:
//	DW_TAG_subprogram:
	.byte 0x03
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x15
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
	.4byte 0x666c6163
	.2byte 0x6e75
	.byte 0x00
//	DW_AT_low_pc:
	.8byte calfun_
//	DW_AT_high_pc:
	.8byte .LNcalfun_
//	DW_AT_external:
	.byte 0x01
//	DW_AT_sibling:
	.4byte 0x0000015a
.DWinfo3:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x1d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x0000015a
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006e
//	DW_AT_location:
	.4byte 0x7fa07604
	.byte 0x06
.DWinfo4:
//	DW_TAG_formal_parameter:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x1f
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000168
//	DW_AT_variable_parameter:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0078
//	DW_AT_location:
	.4byte 0x7fa87604
	.byte 0x06
.DWinfo5:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x01
//	DW_AT_decl_column:
	.byte 0x15
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x666c6163
	.2byte 0x6e75
	.byte 0x00
//	DW_AT_type:
	.4byte 0x0000008b
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x6c
.DWinfo6:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x0e
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x006d7573
//	DW_AT_type:
	.4byte 0x00000175
//	DW_AT_location:
	.4byte 0x7fb87603
.DWinfo7:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x0c
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
	.4byte 0x0000015a
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x64
.DWinfo8:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x0b
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
	.4byte 0x0000015a
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x60
.DWinfo9:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x0a
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0066
//	DW_AT_type:
	.4byte 0x00000175
//	DW_AT_location:
	.4byte 0x7fb07603
.DWinfo10:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x07
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0069
//	DW_AT_type:
	.4byte 0x0000015a
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x5c
.DWinfo11:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x04
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006a
//	DW_AT_type:
	.4byte 0x0000015a
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x54
.DWinfo12:
//	DW_TAG_variable:
	.byte 0x05
//	DW_AT_decl_line:
	.byte 0x03
//	DW_AT_decl_column:
	.byte 0x16
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0079
//	DW_AT_type:
	.4byte 0x00000180
//	DW_AT_location:
	.4byte 0x79807603
	.byte 0x00
.DWinfo13:
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
.DWinfo14:
//	DW_TAG_array_type:
	.byte 0x06
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000175
//	DW_AT_sibling:
	.4byte 0x00000175
.DWinfo15:
//	DW_TAG_subrange_type:
	.byte 0x07
//	DW_AT_lower_bound:
	.byte 0x01
	.byte 0x00
.DWinfo16:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x08
//	DW_AT_encoding:
	.byte 0x04
//	DW_AT_name:
	.8byte 0x002938284c414552
.DWinfo17:
//	DW_TAG_array_type:
	.byte 0x08
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000175
.DWinfo18:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.byte 0x0a
.DWinfo19:
//	DW_TAG_subrange_type:
	.byte 0x09
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.byte 0x0a
	.byte 0x00
	.byte 0x00
	.byte 0x00
	.byte 0x00
	.byte 0x00
// -- Begin DWARF2 SEGMENT .debug_line
	.section .debug_line
.debug_line_seg:
	.align 1
	.4byte 0x0000016b
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
	.8byte 0x662e6e75666c6163
	.byte 0x00
	.8byte 0x03fb05c0a3d09a00
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
	.2byte 0x0303
	.byte 0x01
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
	.8byte ..LN27
	.2byte 0x7e03
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN29
	.2byte 0x0303
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN33
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
	.8byte ..LN79
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN81
	.byte 0x0d
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
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN89
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN91
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN95
	.byte 0x0c
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
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN139
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN141
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN149
	.2byte 0x7903
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN151
	.2byte 0x0803
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN155
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte .LNcalfun_
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
	.byte 0x09
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.byte 0x2f
	.byte 0x0d
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
	.4byte 0x00000024
	.4byte .debug_frame_seg
	.8byte ..___tag_value_calfun_.2
	.8byte ..___tag_value_calfun_.14-..___tag_value_calfun_.2
	.byte 0x04
	.4byte ..___tag_value_calfun_.9-..___tag_value_calfun_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.4byte 0x00000000
	.2byte 0x0000
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
	.4byte 0x00000024
	.4byte 0x0000001c
	.8byte ..___tag_value_calfun_.2
	.8byte ..___tag_value_calfun_.14-..___tag_value_calfun_.2
	.byte 0x04
	.4byte ..___tag_value_calfun_.9-..___tag_value_calfun_.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.4byte 0x00000000
	.2byte 0x0000
	.section .text
.LNDBG_TXe:
# End
