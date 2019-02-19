	.section .text
.LNDBG_TX:
# -- Machine type EFI2
# mark_description "Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 10.0    Build 20070809 %s";
# mark_description "-g -S -W1 -recursive -reentrancy threaded";
	.file "main.f"
	.section .rodata, "a"
	.align 8
	.align 8
STRLITPACK_4:
	.byte	0
	.type	STRLITPACK_4,@object
	.size	STRLITPACK_4,1
	.data
	.text
..TXTST0:
# -- Begin  MAIN__
# mark_begin;
       .align    2,0x90
	.globl MAIN__
MAIN__:
..B1.1:                         # Preds ..B1.0
..___tag_value_MAIN__.2:                                        #
..LN1:
        pushq     %rbp                                          #9.7
        movq      %rsp, %rbp                                    #9.7
..___tag_value_MAIN__.9:                                        #
        subq      $80256, %rsp                                  #9.7
        movq      %rbx, -128(%rbp)                              #9.7
..___tag_value_MAIN__.12:                                       #
        movl      $LITPACK_0, %eax                              #9.7
        movq      %rax, %rdi                                    #9.7
        xorl      %eax, %eax                                    #9.7
        call      for_set_reentrancy                            #9.7
                                # LOE
..B1.2:                         # Preds ..B1.1
        movl      $5000, -20(%rbp)                              #9.7
..LN3:
        fldl      _2il0floatpacket.1(%rip)                      #10.7
        fstpl     -112(%rbp)                                    #10.7
..LN5:
        movl      $2, -24(%rbp)                                 #11.10
                                # LOE
..B1.3:                         # Preds ..B1.9 ..B1.2
..LN7:
        movl      -24(%rbp), %eax                               #12.7
        lea       1(%rax,%rax), %eax                            #12.7
        movl      %eax, -28(%rbp)                               #12.7
..LN9:
        movl      -24(%rbp), %eax                               #13.10
        movl      %eax, -16(%rbp)                               #13.10
        movl      $1, -12(%rbp)                                 #13.10
        movl      -16(%rbp), %eax                               #13.10
        testl     %eax, %eax                                    #13.10
        jle       ..B1.6        # Prob 50%                      #13.10
                                # LOE
..B1.5:                         # Preds ..B1.3 ..B1.5
..LN11:
        movl      -12(%rbp), %eax                               #14.12
        movl      %eax, -80(%rbp)                               #14.12
        fildl     -80(%rbp)                                     #14.12
..LN13:
        fstpl     -40(%rbp)                                     #14.7
        fldl      -40(%rbp)                                     #14.7
..LN15:
        movl      -24(%rbp), %eax                               #14.12
..LN17:
        addl      $1, %eax                                      #14.22
        movl      %eax, -80(%rbp)                               #14.22
        fildl     -80(%rbp)                                     #14.22
..LN19:
        fstpl     -40(%rbp)                                     #14.7
        fldl      -40(%rbp)                                     #14.7
        fdivrp    %st, %st(1)                                   #14.7
..LN21:
        movl      -12(%rbp), %eax                               #14.4
        movslq    %eax, %rax                                    #14.4
..LN23:
        fstpl     -264(%rbp,%rax,8)                             #14.7
..LN25:
        addl      $1, -12(%rbp)                                 #13.10
        movl      -12(%rbp), %eax                               #13.10
        movl      -16(%rbp), %edx                               #13.10
        cmpl      %edx, %eax                                    #13.10
        jle       ..B1.5        # Prob 50%                      #13.10
                                # LOE
..B1.6:                         # Preds ..B1.3 ..B1.5
..LN27:
        fldl      _2il0floatpacket.2(%rip)                      #15.19
..LN29:
        fldl      -256(%rbp)                                    #15.20
..LN31:
        fmulp     %st, %st(1)                                   #15.7
        fstpl     -120(%rbp)                                    #15.7
..LN33:
        movq      $0, -176(%rbp)                                #16.7
        movl      -24(%rbp), %eax                               #16.7
        movl      %eax, -104(%rbp)                              #16.7
        lea       -176(%rbp), %rax                              #16.7
        movl      $STRLITPACK_2, %edx                           #16.7
        lea       -104(%rbp), %rcx                              #16.7
        movl      $main$main_$BLK$format_pack, %ebx             #16.7
        movq      %rax, %rdi                                    #16.7
        movl      $-1, %esi                                     #16.7
        movq      %rdx, -72(%rbp)                               #16.7
        movl      $59047680, %edx                               #16.7
        movq      -72(%rbp), %rax                               #16.7
        movq      %rcx, -64(%rbp)                               #16.7
        movq      %rax, %rcx                                    #16.7
        movq      -64(%rbp), %rax                               #16.7
        movq      %rax, %r8                                     #16.7
        movq      %rbx, %r9                                     #16.7
        xorl      %eax, %eax                                    #16.7
        call      for_write_seq_fmt                             #16.7
                                # LOE
..B1.7:                         # Preds ..B1.6
        movl      -28(%rbp), %eax                               #16.7
        movl      %eax, -96(%rbp)                               #16.7
        lea       -176(%rbp), %rax                              #16.7
        movl      $STRLITPACK_3, %edx                           #16.7
        lea       -96(%rbp), %rcx                               #16.7
        movq      %rax, %rdi                                    #16.7
        movq      %rdx, %rsi                                    #16.7
        movq      %rcx, %rdx                                    #16.7
        xorl      %eax, %eax                                    #16.7
        call      for_write_seq_fmt_xmit                        #16.7
                                # LOE
..B1.8:                         # Preds ..B1.7
..LN35:
        addq      $-32, %rsp                                    #18.12
..LN37:
        lea       -24(%rbp), %rax                               #18.20
..LN39:
        lea       -28(%rbp), %rdx                               #18.22
..LN41:
        lea       -256(%rbp), %rcx                              #18.26
..LN43:
        lea       -120(%rbp), %rbx                              #18.28
..LN45:
        lea       -112(%rbp), %rsi                              #18.35
..LN47:
        lea       -32(%rbp), %rdi                               #18.42
..LN49:
        lea       -20(%rbp), %r8                                #18.49
        movq      %r8, (%rsp)                                   #18.49
..LN51:
        lea       -80256(%rbp), %r8                             #18.56
        movq      %r8, 8(%rsp)                                  #18.56
        movl      $calfun_, %r8d                                #18.56
        movq      %r8, 16(%rsp)                                 #18.56
..LN53:
        movq      %rdi, -56(%rbp)                               #18.12
        movq      %rax, %rdi                                    #18.12
        movq      %rsi, -48(%rbp)                               #18.12
        movq      %rdx, %rsi                                    #18.12
        movq      %rcx, %rdx                                    #18.12
        movq      %rbx, %rcx                                    #18.12
        movq      -48(%rbp), %rax                               #18.12
        movq      %rax, %r8                                     #18.12
        movq      -56(%rbp), %rax                               #18.12
        movq      %rax, %r9                                     #18.12
        xorl      %eax, %eax                                    #18.12
        call      newuoa_                                       #18.12
                                # LOE
..B1.14:                        # Preds ..B1.8
        addq      $32, %rsp                                     #18.12
                                # LOE
..B1.9:                         # Preds ..B1.14
..LN55:
        movq      $0, -88(%rbp)                                 #19.4
..LN57:
        addl      $2, -24(%rbp)                                 #11.10
        movl      -24(%rbp), %eax                               #11.10
        cmpl      $8, %eax                                      #11.10
        jle       ..B1.3        # Prob 50%                      #11.10
                                # LOE
..B1.10:                        # Preds ..B1.9
..LN59:
        movl      $STRLITPACK_4, %eax                           #20.7
        movq      %rax, %rdi                                    #20.7
        xorl      %esi, %esi                                    #20.7
        movl      $59047680, %edx                               #20.7
        xorl      %ecx, %ecx                                    #20.7
        xorl      %r8d, %r8d                                    #20.7
        xorl      %eax, %eax                                    #20.7
        call      for_stop_core                                 #20.7
                                # LOE
..B1.11:                        # Preds ..B1.10
..LN61:
        movl      $1, %eax                                      #21.7
        movq      -128(%rbp), %rbx                              #21.7
..___tag_value_MAIN__.13:                                       #
        leave                                                   #21.7
..___tag_value_MAIN__.15:                                       #
        ret                                                     #21.7
        .align    2,0x90
..___tag_value_MAIN__.16:                                       #
                                # LOE
# mark_end;
	.type	MAIN__,@function
	.size	MAIN__,.-MAIN__
.LNMAIN__:
	.data
	.align 32
	.align 32
main$main_$BLK$format_pack:
	.byte	54
	.byte	0
	.byte	0
	.byte	0
	.byte	71
	.byte	0
	.byte	1
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
	.byte	16
	.byte	0
	.byte	82
	.byte	101
	.byte	115
	.byte	117
	.byte	108
	.byte	116
	.byte	115
	.byte	32
	.byte	119
	.byte	105
	.byte	116
	.byte	104
	.byte	32
	.byte	78
	.byte	32
	.byte	61
	.byte	36
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	28
	.byte	0
	.byte	10
	.byte	0
	.byte	32
	.byte	97
	.byte	110
	.byte	100
	.byte	32
	.byte	78
	.byte	80
	.byte	84
	.byte	32
	.byte	61
	.byte	0
	.byte	0
	.byte	36
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	55
	.byte	0
	.byte	0
	.byte	0
	.section .rodata, "a"
	.align 8
LITPACK_0:
	.long	0x00000002,0x00000000
STRLITPACK_2:
	.byte	9
	.byte	1
	.byte	2
	.byte	0
	.byte	0
	.space 3	# pad
STRLITPACK_3:
	.byte	9
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.data
# -- End  MAIN__
	.section .rodata, "a"
	.align 8
_2il0floatpacket.1:
	.long	0xa0b5ed8d,0x3eb0c6f7
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
_2il0floatpacket.2:
	.long	0x9999999a,0x3fc99999
	.type	_2il0floatpacket.2,@object
	.size	_2il0floatpacket.2,8
STRLITPACK_0:
	.byte	32
	.byte	97
	.byte	110
	.byte	100
	.byte	32
	.byte	78
	.byte	80
	.byte	84
	.byte	32
	.byte	61
	.byte	0
	.type	STRLITPACK_0,@object
	.size	STRLITPACK_0,11
	.space 1	# pad
STRLITPACK_1:
	.byte	82
	.byte	101
	.byte	115
	.byte	117
	.byte	108
	.byte	116
	.byte	115
	.byte	32
	.byte	119
	.byte	105
	.byte	116
	.byte	104
	.byte	32
	.byte	78
	.byte	32
	.byte	61
	.byte	0
	.type	STRLITPACK_1,@object
	.size	STRLITPACK_1,17
	.data
# mark_proc_addr_taken calfun_;
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .debug_info
	.section .debug_info
.debug_info_seg:
	.align 1
	.4byte 0x0000018e
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
	.4byte 0x6e69616d
	.2byte 0x662e
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
	.byte 0x09
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_calling_convention:
	.byte 0x02
//	DW_AT_inline:
	.byte 0x00
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000089
//	DW_AT_prototyped:
	.byte 0x00
//	DW_AT_name:
	.8byte 0x69616d246e69616d
	.4byte 0x42245f6e
	.2byte 0x4b4c
	.byte 0x00
//	DW_AT_low_pc:
	.8byte MAIN__
//	DW_AT_high_pc:
	.8byte .LNMAIN__
//	DW_AT_external:
	.byte 0x01
//	DW_AT_sibling:
	.4byte 0x0000015b
.DWinfo3:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x12
//	DW_AT_decl_column:
	.byte 0x2a
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x69727069
	.2byte 0x746e
	.byte 0x00
//	DW_AT_type:
	.4byte 0x0000015b
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x60
.DWinfo4:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x0f
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x626f6872
	.2byte 0x6765
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000169
//	DW_AT_location:
	.4byte 0x7f887603
.DWinfo5:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x0d
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0069
//	DW_AT_type:
	.4byte 0x0000015b
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x74
.DWinfo6:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x0c
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x0074706e
//	DW_AT_type:
	.4byte 0x0000015b
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x64
.DWinfo7:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x0b
//	DW_AT_decl_column:
	.byte 0x0d
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x006e
//	DW_AT_type:
	.4byte 0x0000015b
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x68
.DWinfo8:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x0a
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x656f6872
	.2byte 0x646e
	.byte 0x00
//	DW_AT_type:
	.4byte 0x00000169
//	DW_AT_location:
	.4byte 0x7f907603
.DWinfo9:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x09
//	DW_AT_decl_column:
	.byte 0x07
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.4byte 0x6678616d
	.2byte 0x6e75
	.byte 0x00
//	DW_AT_type:
	.4byte 0x0000015b
//	DW_AT_location:
	.2byte 0x7602
	.byte 0x6c
.DWinfo10:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x07
//	DW_AT_decl_column:
	.byte 0x17
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0077
//	DW_AT_type:
	.4byte 0x00000174
//	DW_AT_location:
	.4byte 0x8d807604
	.byte 0x7b
.DWinfo11:
//	DW_TAG_variable:
	.byte 0x04
//	DW_AT_decl_line:
	.byte 0x07
//	DW_AT_decl_column:
	.byte 0x11
//	DW_AT_decl_file:
	.byte 0x01
//	DW_AT_accessibility:
	.byte 0x01
//	DW_AT_name:
	.2byte 0x0078
//	DW_AT_type:
	.4byte 0x00000184
//	DW_AT_location:
	.4byte 0x7e807603
	.byte 0x00
.DWinfo12:
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
.DWinfo13:
//	DW_TAG_base_type:
	.byte 0x02
//	DW_AT_byte_size:
	.byte 0x08
//	DW_AT_encoding:
	.byte 0x04
//	DW_AT_name:
	.8byte 0x002938284c414552
.DWinfo14:
//	DW_TAG_array_type:
	.byte 0x05
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000169
//	DW_AT_sibling:
	.4byte 0x00000184
.DWinfo15:
//	DW_TAG_subrange_type:
	.byte 0x06
//	DW_AT_lower_bound:
	.byte 0x01
//	DW_AT_upper_bound:
	.2byte 0xce90
	.byte 0x00
	.byte 0x00
.DWinfo16:
//	DW_TAG_array_type:
	.byte 0x07
//	DW_AT_ordering:
	.byte 0x01
//	DW_AT_type:
	.4byte 0x00000169
.DWinfo17:
//	DW_TAG_subrange_type:
	.byte 0x06
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
	.4byte 0x000000e1
	.2byte 0x0002
	.4byte 0x0000001f
	.byte 0x01
	.byte 0x01
	.byte 0xff
	.byte 0x04
	.byte 0x0a
	.8byte 0x0000000101010100
	.byte 0x01
	.byte 0x00
	.4byte 0x6e69616d
	.2byte 0x662e
	.byte 0x00
	.8byte 0x04a605c0a4e0a700
	.byte 0x00
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN1
	.2byte 0x0803
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN3
	.byte 0x0c
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
	.8byte ..LN25
	.byte 0x0a
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN27
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN33
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN35
	.byte 0x0d
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN55
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN57
	.2byte 0x7803
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN59
	.2byte 0x0903
	.byte 0x01
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte ..LN61
	.byte 0x0c
	.byte 0x00
	.byte 0x09
	.byte 0x02
	.8byte .LNMAIN__
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
	.byte 0x36
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
	.byte 0x05
	.byte 0x01
	.byte 0x01
	.byte 0x09
	.byte 0x0b
	.byte 0x49
	.byte 0x13
	.byte 0x01
	.byte 0x13
	.2byte 0x0000
	.byte 0x06
	.byte 0x21
	.byte 0x00
	.byte 0x22
	.byte 0x0d
	.byte 0x2f
	.byte 0x0d
	.2byte 0x0000
	.byte 0x07
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
	.8byte ..___tag_value_MAIN__.2
	.8byte ..___tag_value_MAIN__.16-..___tag_value_MAIN__.2
	.byte 0x04
	.4byte ..___tag_value_MAIN__.9-..___tag_value_MAIN__.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_MAIN__.12-..___tag_value_MAIN__.9
	.byte 0x83
	.byte 0x12
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
	.8byte ..___tag_value_MAIN__.2
	.8byte ..___tag_value_MAIN__.16-..___tag_value_MAIN__.2
	.byte 0x04
	.4byte ..___tag_value_MAIN__.9-..___tag_value_MAIN__.2
	.byte 0x0c
	.2byte 0x1006
	.byte 0x86
	.byte 0x02
	.byte 0x04
	.4byte ..___tag_value_MAIN__.12-..___tag_value_MAIN__.9
	.byte 0x83
	.byte 0x12
	.4byte 0x00000000
	.2byte 0x0000
	.byte 0x00
	.section .text
.LNDBG_TXe:
# End
