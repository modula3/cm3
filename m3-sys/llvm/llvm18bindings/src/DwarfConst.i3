(* ===-- File: DwarfConst.i3 ---Dwarf Constants --------------*-Modula3 -*-===*)
(* Derived from llvm-3.6.1-src/include/llvm/Support/Dwarf.h                   *)
(* Rodney M. Bates, rodney.m.bates@acm.org.  2016-02-28                       *) 
(*   
//===-- llvm/Support/Dwarf.h ---Dwarf Constants------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// \file
// \brief This file contains constants used for implementing Dwarf
// debug support.
//
// For details on the Dwarf specfication see the latest DWARF Debugging
// Information Format standard document on http://www.dwarfstd.org. This
// file often includes support for non-released standard features.
//
//===----------------------------------------------------------------------===//
*) 

INTERFACE DwarfConst 

(* Extra llvm constants: *) 
; CONST 
    DWARF_VERSION = 4             (* Default dwarf version we output. *)
  ; DW_PUBTYPES_VERSION = 2       (* Section version number for .debug_pubtypes. *)
  ; DW_PUBNAMES_VERSION = 2       (* Section version number for .debug_pubnames. *)
  ; DW_ARANGES_VERSION = 2        (* Section version number for .debug_aranges. *)

(* Dwarf Tags: *) 
; CONST 

  (* llvm mock tags: *) 
    DW_TAG_invalid = 0            (* Tag for invalid results. *)
  ; DW_TAG_auto_variable = 16_100 (* Tag for local (auto) variables. *)
  ; DW_TAG_arg_variable = 16_101  (* Tag for argument variables. *)
  ; DW_TAG_expression = 16_102    (* Tag for complex address expressions. *)
  ; DW_TAG_user_base = 16_1000    (*Recommended base for user tags. *)

  (* Dwarf standard tags: *) 
  ; DW_TAG_array_type = 16_01
  ; DW_TAG_class_type = 16_02
  ; DW_TAG_entry_point = 16_03
  ; DW_TAG_enumeration_type = 16_04
  ; DW_TAG_formal_parameter = 16_05
  ; DW_TAG_imported_declaration = 16_08
  ; DW_TAG_label = 16_0a
  ; DW_TAG_lexical_block = 16_0b
  ; DW_TAG_member = 16_0d
  ; DW_TAG_pointer_type = 16_0f
  ; DW_TAG_reference_type = 16_10
  ; DW_TAG_compile_unit = 16_11
  ; DW_TAG_string_type = 16_12
  ; DW_TAG_structure_type = 16_13
  ; DW_TAG_subroutine_type = 16_15
  ; DW_TAG_typedef = 16_16
  ; DW_TAG_union_type = 16_17
  ; DW_TAG_unspecified_parameters = 16_18
  ; DW_TAG_variant = 16_19
  ; DW_TAG_common_block = 16_1a
  ; DW_TAG_common_inclusion = 16_1b
  ; DW_TAG_inheritance = 16_1c
  ; DW_TAG_inlined_subroutine = 16_1d
  ; DW_TAG_module = 16_1e
  ; DW_TAG_ptr_to_member_type = 16_1f
  ; DW_TAG_set_type = 16_20
  ; DW_TAG_subrange_type = 16_21
  ; DW_TAG_with_stmt = 16_22
  ; DW_TAG_access_declaration = 16_23
  ; DW_TAG_base_type = 16_24
  ; DW_TAG_catch_block = 16_25
  ; DW_TAG_const_type = 16_26
  ; DW_TAG_constant = 16_27
  ; DW_TAG_enumerator = 16_28
  ; DW_TAG_file_type = 16_29
  ; DW_TAG_friend = 16_2a
  ; DW_TAG_namelist = 16_2b
  ; DW_TAG_namelist_item = 16_2c
  ; DW_TAG_packed_type = 16_2d
  ; DW_TAG_subprogram = 16_2e
  ; DW_TAG_template_type_parameter = 16_2f
  ; DW_TAG_template_value_parameter = 16_30
  ; DW_TAG_thrown_type = 16_31
  ; DW_TAG_try_block = 16_32
  ; DW_TAG_variant_part = 16_33
  ; DW_TAG_variable = 16_34
  ; DW_TAG_volatile_type = 16_35
  ; DW_TAG_dwarf_procedure = 16_36
  ; DW_TAG_restrict_type = 16_37
  ; DW_TAG_interface_type = 16_38
  ; DW_TAG_namespace = 16_39
  ; DW_TAG_imported_module = 16_3a
  ; DW_TAG_unspecified_type = 16_3b
  ; DW_TAG_partial_unit = 16_3c
  ; DW_TAG_imported_unit = 16_3d
  ; DW_TAG_condition = 16_3f
  ; DW_TAG_shared_type = 16_40
  ; DW_TAG_type_unit = 16_41
  ; DW_TAG_rvalue_reference_type = 16_42
  ; DW_TAG_template_alias = 16_43

  (* New in DWARF 5: *) 
  ; DW_TAG_coarray_type = 16_44
  ; DW_TAG_generic_subrange = 16_45
  ; DW_TAG_dynamic_type = 16_46

  ; DW_TAG_MIPS_loop = 16_4081
  ; DW_TAG_format_label = 16_4101
  ; DW_TAG_function_template = 16_4102
  ; DW_TAG_class_template = 16_4103
  ; DW_TAG_GNU_template_template_param = 16_4106
  ; DW_TAG_GNU_template_parameter_pack = 16_4107
  ; DW_TAG_GNU_formal_parameter_pack = 16_4108
  ; DW_TAG_lo_user = 16_4080
  ; DW_TAG_APPLE_property = 16_4200
  ; DW_TAG_hi_user = 16_ffff

(* Dwarf attributes: *) 
; CONST
    DW_AT_sibling = 16_01
  ; DW_AT_location = 16_02
  ; DW_AT_name = 16_03
  ; DW_AT_ordering = 16_09
  ; DW_AT_byte_size = 16_0b
  ; DW_AT_bit_offset = 16_0c
  ; DW_AT_bit_size = 16_0d
  ; DW_AT_stmt_list = 16_10
  ; DW_AT_low_pc = 16_11
  ; DW_AT_high_pc = 16_12
  ; DW_AT_language = 16_13
  ; DW_AT_discr = 16_15
  ; DW_AT_discr_value = 16_16
  ; DW_AT_visibility = 16_17
  ; DW_AT_import = 16_18
  ; DW_AT_string_length = 16_19
  ; DW_AT_common_reference = 16_1a
  ; DW_AT_comp_dir = 16_1b
  ; DW_AT_const_value = 16_1c
  ; DW_AT_containing_type = 16_1d
  ; DW_AT_default_value = 16_1e
  ; DW_AT_inline = 16_20
  ; DW_AT_is_optional = 16_21
  ; DW_AT_lower_bound = 16_22
  ; DW_AT_producer = 16_25
  ; DW_AT_prototyped = 16_27
  ; DW_AT_return_addr = 16_2a
  ; DW_AT_start_scope = 16_2c
  ; DW_AT_bit_stride = 16_2e
  ; DW_AT_upper_bound = 16_2f
  ; DW_AT_abstract_origin = 16_31
  ; DW_AT_accessibility = 16_32
  ; DW_AT_address_class = 16_33
  ; DW_AT_artificial = 16_34
  ; DW_AT_base_types = 16_35
  ; DW_AT_calling_convention = 16_36
  ; DW_AT_count = 16_37
  ; DW_AT_data_member_location = 16_38
  ; DW_AT_decl_column = 16_39
  ; DW_AT_decl_file = 16_3a
  ; DW_AT_decl_line = 16_3b
  ; DW_AT_declaration = 16_3c
  ; DW_AT_discr_list = 16_3d
  ; DW_AT_encoding = 16_3e
  ; DW_AT_external = 16_3f
  ; DW_AT_frame_base = 16_40
  ; DW_AT_friend = 16_41
  ; DW_AT_identifier_case = 16_42
  ; DW_AT_macro_info = 16_43
  ; DW_AT_namelist_item = 16_44
  ; DW_AT_priority = 16_45
  ; DW_AT_segment = 16_46
  ; DW_AT_specification = 16_47
  ; DW_AT_static_link = 16_48
  ; DW_AT_type = 16_49
  ; DW_AT_use_location = 16_4a
  ; DW_AT_variable_parameter = 16_4b
  ; DW_AT_virtuality = 16_4c
  ; DW_AT_vtable_elem_location = 16_4d
  ; DW_AT_allocated = 16_4e
  ; DW_AT_associated = 16_4f
  ; DW_AT_data_location = 16_50
  ; DW_AT_byte_stride = 16_51
  ; DW_AT_entry_pc = 16_52
  ; DW_AT_use_UTF8 = 16_53
  ; DW_AT_extension = 16_54
  ; DW_AT_ranges = 16_55
  ; DW_AT_trampoline = 16_56
  ; DW_AT_call_column = 16_57
  ; DW_AT_call_file = 16_58
  ; DW_AT_call_line = 16_59
  ; DW_AT_description = 16_5a
  ; DW_AT_binary_scale = 16_5b
  ; DW_AT_decimal_scale = 16_5c
  ; DW_AT_small = 16_5d
  ; DW_AT_decimal_sign = 16_5e
  ; DW_AT_digit_count = 16_5f
  ; DW_AT_picture_string = 16_60
  ; DW_AT_mutable = 16_61
  ; DW_AT_threads_scaled = 16_62
  ; DW_AT_explicit = 16_63
  ; DW_AT_object_pointer = 16_64
  ; DW_AT_endianity = 16_65
  ; DW_AT_elemental = 16_66
  ; DW_AT_pure = 16_67
  ; DW_AT_recursive = 16_68
  ; DW_AT_signature = 16_69
  ; DW_AT_main_subprogram = 16_6a
  ; DW_AT_data_bit_offset = 16_6b
  ; DW_AT_const_expr = 16_6c
  ; DW_AT_enum_class = 16_6d
  ; DW_AT_linkage_name = 16_6e

  (* New in DWARF 5: *)
  ; DW_AT_string_length_bit_size = 16_6f
  ; DW_AT_string_length_byte_size = 16_70
  ; DW_AT_rank = 16_71
  ; DW_AT_str_offsets_base = 16_72
  ; DW_AT_addr_base = 16_73
  ; DW_AT_ranges_base = 16_74
  ; DW_AT_dwo_id = 16_75
  ; DW_AT_dwo_name = 16_76
  ; DW_AT_reference = 16_77
  ; DW_AT_rvalue_reference = 16_78

  ; DW_AT_lo_user = 16_2000
  ; DW_AT_hi_user = 16_3fff

  ; DW_AT_MIPS_loop_begin = 16_2002
  ; DW_AT_MIPS_tail_loop_begin = 16_2003
  ; DW_AT_MIPS_epilog_begin = 16_2004
  ; DW_AT_MIPS_loop_unroll_factor = 16_2005
  ; DW_AT_MIPS_software_pipeline_depth = 16_2006
  ; DW_AT_MIPS_linkage_name = 16_2007
  ; DW_AT_MIPS_stride = 16_2008
  ; DW_AT_MIPS_abstract_name = 16_2009
  ; DW_AT_MIPS_clone_origin = 16_200a
  ; DW_AT_MIPS_has_inlines = 16_200b
  ; DW_AT_MIPS_stride_byte = 16_200c
  ; DW_AT_MIPS_stride_elem = 16_200d
  ; DW_AT_MIPS_ptr_dopetype = 16_200e
  ; DW_AT_MIPS_allocatable_dopetype = 16_200f
  ; DW_AT_MIPS_assumed_shape_dopetype = 16_2010

  (* This one appears to have only been implemented by Open64 for 
     fortran and may conflict with other extensions. *)
  ; DW_AT_MIPS_assumed_size = 16_2011

  (* GNU extensions: *) 
  ; DW_AT_sf_names = 16_2101
  ; DW_AT_src_info = 16_2102
  ; DW_AT_mac_info = 16_2103
  ; DW_AT_src_coords = 16_2104
  ; DW_AT_body_begin = 16_2105
  ; DW_AT_body_end = 16_2106
  ; DW_AT_GNU_vector = 16_2107
  ; DW_AT_GNU_template_name = 16_2110

  ; DW_AT_GNU_odr_signature = 16_210f

  (* Extensions for Fission proposal: *) 
  ; DW_AT_GNU_dwo_name = 16_2130
  ; DW_AT_GNU_dwo_id = 16_2131
  ; DW_AT_GNU_ranges_base = 16_2132
  ; DW_AT_GNU_addr_base = 16_2133
  ; DW_AT_GNU_pubnames = 16_2134
  ; DW_AT_GNU_pubtypes = 16_2135

  (* Apple extensions: *) 
  ; DW_AT_APPLE_optimized = 16_3fe1
  ; DW_AT_APPLE_flags = 16_3fe2
  ; DW_AT_APPLE_isa = 16_3fe3
  ; DW_AT_APPLE_block = 16_3fe4
  ; DW_AT_APPLE_major_runtime_vers = 16_3fe5
  ; DW_AT_APPLE_runtime_class = 16_3fe6
  ; DW_AT_APPLE_omit_frame_ptr = 16_3fe7
  ; DW_AT_APPLE_property_name = 16_3fe8
  ; DW_AT_APPLE_property_getter = 16_3fe9
  ; DW_AT_APPLE_property_setter = 16_3fea
  ; DW_AT_APPLE_property_attribute = 16_3feb
  ; DW_AT_APPLE_objc_complete_type = 16_3fec
  ; DW_AT_APPLE_property = 16_3fed

(* Attribute Forms: *) 
; CONST
    DW_FORM_addr = 16_01
  ; DW_FORM_block2 = 16_03
  ; DW_FORM_block4 = 16_04
  ; DW_FORM_data2 = 16_05
  ; DW_FORM_data4 = 16_06
  ; DW_FORM_data8 = 16_07
  ; DW_FORM_string = 16_08
  ; DW_FORM_block = 16_09
  ; DW_FORM_block1 = 16_0a
  ; DW_FORM_data1 = 16_0b
  ; DW_FORM_flag = 16_0c
  ; DW_FORM_sdata = 16_0d
  ; DW_FORM_strp = 16_0e
  ; DW_FORM_udata = 16_0f
  ; DW_FORM_ref_addr = 16_10
  ; DW_FORM_ref1 = 16_11
  ; DW_FORM_ref2 = 16_12
  ; DW_FORM_ref4 = 16_13
  ; DW_FORM_ref8 = 16_14
  ; DW_FORM_ref_udata = 16_15
  ; DW_FORM_indirect = 16_16
  ; DW_FORM_sec_offset = 16_17
  ; DW_FORM_exprloc = 16_18
  ; DW_FORM_flag_present = 16_19
  ; DW_FORM_ref_sig8 = 16_20

  (* Extensions for Fission proposal: *) 
  ; DW_FORM_GNU_addr_index = 16_1f01
  ; DW_FORM_GNU_str_index = 16_1f02

(* Dwarf Operations: *) 
; CONST 
    DW_OP_addr = 16_03
  ; DW_OP_deref = 16_06
  ; DW_OP_const1u = 16_08
  ; DW_OP_const1s = 16_09
  ; DW_OP_const2u = 16_0a
  ; DW_OP_const2s = 16_0b
  ; DW_OP_const4u = 16_0c
  ; DW_OP_const4s = 16_0d
  ; DW_OP_const8u = 16_0e
  ; DW_OP_const8s = 16_0f
  ; DW_OP_constu = 16_10
  ; DW_OP_consts = 16_11
  ; DW_OP_dup = 16_12
  ; DW_OP_drop = 16_13
  ; DW_OP_over = 16_14
  ; DW_OP_pick = 16_15
  ; DW_OP_swap = 16_16
  ; DW_OP_rot = 16_17
  ; DW_OP_xderef = 16_18
  ; DW_OP_abs = 16_19
  ; DW_OP_and = 16_1a
  ; DW_OP_div = 16_1b
  ; DW_OP_minus = 16_1c
  ; DW_OP_mod = 16_1d
  ; DW_OP_mul = 16_1e
  ; DW_OP_neg = 16_1f
  ; DW_OP_not = 16_20
  ; DW_OP_or = 16_21
  ; DW_OP_plus = 16_22
  ; DW_OP_plus_uconst = 16_23
  ; DW_OP_shl = 16_24
  ; DW_OP_shr = 16_25
  ; DW_OP_shra = 16_26
  ; DW_OP_xor = 16_27
  ; DW_OP_skip = 16_2f
  ; DW_OP_bra = 16_28
  ; DW_OP_eq = 16_29
  ; DW_OP_ge = 16_2a
  ; DW_OP_gt = 16_2b
  ; DW_OP_le = 16_2c
  ; DW_OP_lt = 16_2d
  ; DW_OP_ne = 16_2e
  ; DW_OP_lit0 = 16_30
  ; DW_OP_lit1 = 16_31
  ; DW_OP_lit2 = 16_32
  ; DW_OP_lit3 = 16_33
  ; DW_OP_lit4 = 16_34
  ; DW_OP_lit5 = 16_35
  ; DW_OP_lit6 = 16_36
  ; DW_OP_lit7 = 16_37
  ; DW_OP_lit8 = 16_38
  ; DW_OP_lit9 = 16_39
  ; DW_OP_lit10 = 16_3a
  ; DW_OP_lit11 = 16_3b
  ; DW_OP_lit12 = 16_3c
  ; DW_OP_lit13 = 16_3d
  ; DW_OP_lit14 = 16_3e
  ; DW_OP_lit15 = 16_3f
  ; DW_OP_lit16 = 16_40
  ; DW_OP_lit17 = 16_41
  ; DW_OP_lit18 = 16_42
  ; DW_OP_lit19 = 16_43
  ; DW_OP_lit20 = 16_44
  ; DW_OP_lit21 = 16_45
  ; DW_OP_lit22 = 16_46
  ; DW_OP_lit23 = 16_47
  ; DW_OP_lit24 = 16_48
  ; DW_OP_lit25 = 16_49
  ; DW_OP_lit26 = 16_4a
  ; DW_OP_lit27 = 16_4b
  ; DW_OP_lit28 = 16_4c
  ; DW_OP_lit29 = 16_4d
  ; DW_OP_lit30 = 16_4e
  ; DW_OP_lit31 = 16_4f
  ; DW_OP_reg0 = 16_50
  ; DW_OP_reg1 = 16_51
  ; DW_OP_reg2 = 16_52
  ; DW_OP_reg3 = 16_53
  ; DW_OP_reg4 = 16_54
  ; DW_OP_reg5 = 16_55
  ; DW_OP_reg6 = 16_56
  ; DW_OP_reg7 = 16_57
  ; DW_OP_reg8 = 16_58
  ; DW_OP_reg9 = 16_59
  ; DW_OP_reg10 = 16_5a
  ; DW_OP_reg11 = 16_5b
  ; DW_OP_reg12 = 16_5c
  ; DW_OP_reg13 = 16_5d
  ; DW_OP_reg14 = 16_5e
  ; DW_OP_reg15 = 16_5f
  ; DW_OP_reg16 = 16_60
  ; DW_OP_reg17 = 16_61
  ; DW_OP_reg18 = 16_62
  ; DW_OP_reg19 = 16_63
  ; DW_OP_reg20 = 16_64
  ; DW_OP_reg21 = 16_65
  ; DW_OP_reg22 = 16_66
  ; DW_OP_reg23 = 16_67
  ; DW_OP_reg24 = 16_68
  ; DW_OP_reg25 = 16_69
  ; DW_OP_reg26 = 16_6a
  ; DW_OP_reg27 = 16_6b
  ; DW_OP_reg28 = 16_6c
  ; DW_OP_reg29 = 16_6d
  ; DW_OP_reg30 = 16_6e
  ; DW_OP_reg31 = 16_6f
  ; DW_OP_breg0 = 16_70
  ; DW_OP_breg1 = 16_71
  ; DW_OP_breg2 = 16_72
  ; DW_OP_breg3 = 16_73
  ; DW_OP_breg4 = 16_74
  ; DW_OP_breg5 = 16_75
  ; DW_OP_breg6 = 16_76
  ; DW_OP_breg7 = 16_77
  ; DW_OP_breg8 = 16_78
  ; DW_OP_breg9 = 16_79
  ; DW_OP_breg10 = 16_7a
  ; DW_OP_breg11 = 16_7b
  ; DW_OP_breg12 = 16_7c
  ; DW_OP_breg13 = 16_7d
  ; DW_OP_breg14 = 16_7e
  ; DW_OP_breg15 = 16_7f
  ; DW_OP_breg16 = 16_80
  ; DW_OP_breg17 = 16_81
  ; DW_OP_breg18 = 16_82
  ; DW_OP_breg19 = 16_83
  ; DW_OP_breg20 = 16_84
  ; DW_OP_breg21 = 16_85
  ; DW_OP_breg22 = 16_86
  ; DW_OP_breg23 = 16_87
  ; DW_OP_breg24 = 16_88
  ; DW_OP_breg25 = 16_89
  ; DW_OP_breg26 = 16_8a
  ; DW_OP_breg27 = 16_8b
  ; DW_OP_breg28 = 16_8c
  ; DW_OP_breg29 = 16_8d
  ; DW_OP_breg30 = 16_8e
  ; DW_OP_breg31 = 16_8f
  ; DW_OP_regx = 16_90
  ; DW_OP_fbreg = 16_91
  ; DW_OP_bregx = 16_92
  ; DW_OP_piece = 16_93
  ; DW_OP_deref_size = 16_94
  ; DW_OP_xderef_size = 16_95
  ; DW_OP_nop = 16_96
  ; DW_OP_push_object_address = 16_97
  ; DW_OP_call2 = 16_98
  ; DW_OP_call4 = 16_99
  ; DW_OP_call_ref = 16_9a
  ; DW_OP_form_tls_address = 16_9b
  ; DW_OP_call_frame_cfa = 16_9c
  ; DW_OP_bit_piece = 16_9d
  ; DW_OP_implicit_value = 16_9e
  ; DW_OP_stack_value = 16_9f
  ; DW_OP_lo_user = 16_e0
  ; DW_OP_hi_user = 16_ff

  (* Extensions for GNU-style thread-local storage: *) 
  ; DW_OP_GNU_push_tls_address = 16_e0

  (* Extensions for Fission proposal: *) 
  ; DW_OP_GNU_addr_index = 16_fb
  ; DW_OP_GNU_const_index = 16_fc

(* Encoding attribute values: *) 
; CONST 
    DW_ATE_address = 16_01
  ; DW_ATE_boolean = 16_02
  ; DW_ATE_complex_float = 16_03
  ; DW_ATE_float = 16_04
  ; DW_ATE_signed = 16_05
  ; DW_ATE_signed_char = 16_06
  ; DW_ATE_unsigned = 16_07
  ; DW_ATE_unsigned_char = 16_08
  ; DW_ATE_imaginary_float = 16_09
  ; DW_ATE_packed_decimal = 16_0a
  ; DW_ATE_numeric_string = 16_0b
  ; DW_ATE_edited = 16_0c
  ; DW_ATE_signed_fixed = 16_0d
  ; DW_ATE_unsigned_fixed = 16_0e
  ; DW_ATE_decimal_float = 16_0f
  ; DW_ATE_UTF = 16_10
  ; DW_ATE_lo_user = 16_80
  ; DW_ATE_hi_user = 16_ff

(* Decimal sign attribute values: *) 
; CONST
    DW_DS_unsigned = 16_01
  ; DW_DS_leading_overpunch = 16_02
  ; DW_DS_trailing_overpunch = 16_03
  ; DW_DS_leading_separate = 16_04
  ; DW_DS_trailing_separate = 16_05

(* Endianity attribute values: *) 
; CONST
    DW_END_default = 16_00
  ; DW_END_big = 16_01
  ; DW_END_little = 16_02
  ; DW_END_lo_user = 16_40
  ; DW_END_hi_user = 16_ff

(* Accessibility codes: *) 
; CONST
    DW_ACCESS_public = 16_01
  ; DW_ACCESS_protected = 16_02
  ; DW_ACCESS_private = 16_03

(* Visibility codes: *) 
; CONST
    DW_VIS_local = 16_01
  ; DW_VIS_exported = 16_02
  ; DW_VIS_qualified = 16_03

(* Virtuality codes: *)
; CONST
    DW_VIRTUALITY_none = 16_00
  ; DW_VIRTUALITY_virtual = 16_01
  ; DW_VIRTUALITY_pure_virtual = 16_02

(* Language names: *) 
; CONST
    DW_LANG_C89 = 16_0001
  ; DW_LANG_C = 16_0002
  ; DW_LANG_Ada83 = 16_0003
  ; DW_LANG_C_plus_plus = 16_0004
  ; DW_LANG_Cobol74 = 16_0005
  ; DW_LANG_Cobol85 = 16_0006
  ; DW_LANG_Fortran77 = 16_0007
  ; DW_LANG_Fortran90 = 16_0008
  ; DW_LANG_Pascal83 = 16_0009
  ; DW_LANG_Modula2 = 16_000a
  ; DW_LANG_Java = 16_000b
  ; DW_LANG_C99 = 16_000c
  ; DW_LANG_Ada95 = 16_000d
  ; DW_LANG_Fortran95 = 16_000e
  ; DW_LANG_PLI = 16_000f
  ; DW_LANG_ObjC = 16_0010
  ; DW_LANG_ObjC_plus_plus = 16_0011
  ; DW_LANG_UPC = 16_0012
  ; DW_LANG_D = 16_0013
  (* New in DWARF 5: *) 
  ; DW_LANG_Python = 16_0014
  ; DW_LANG_OpenCL = 16_0015
  ; DW_LANG_Go = 16_0016
  ; DW_LANG_Modula3 = 16_0017
  ; DW_LANG_Haskell = 16_0018
  ; DW_LANG_C_plus_plus_03 = 16_0019
  ; DW_LANG_C_plus_plus_11 = 16_001a
  ; DW_LANG_OCaml = 16_001b

  ; DW_LANG_lo_user = 16_8000
  ; DW_LANG_Mips_Assembler = 16_8001
  ; DW_LANG_hi_user = 16_ffff

(* Identifier case codes: *) 
; CONST
    DW_ID_case_sensitive = 16_00
  ; DW_ID_up_case = 16_01
  ; DW_ID_down_case = 16_02
  ; DW_ID_case_insensitive = 16_03

(* Calling convention codes: *)
; CONST
    DW_CC_normal = 16_01
  ; DW_CC_program = 16_02
  ; DW_CC_nocall = 16_03
  ; DW_CC_lo_user = 16_40
  ; DW_CC_hi_user = 16_ff

(* Inline codes: *)
; CONST
    DW_INL_not_inlined = 16_00
  ; DW_INL_inlined = 16_01
  ; DW_INL_declared_not_inlined = 16_02
  ; DW_INL_declared_inlined = 16_03

(* Array ordering: *)
; CONST
    DW_ORD_row_major = 16_00
  ; DW_ORD_col_major = 16_01

(* Discriminant descriptor values: *)
; CONST
    DW_DSC_label = 16_00
  ; DW_DSC_range = 16_01

(* Line Number Standard Opcode Encodings: *) 
; CONST
    DW_LNS_extended_op = 16_00
  ; DW_LNS_copy = 16_01
  ; DW_LNS_advance_pc = 16_02
  ; DW_LNS_advance_line = 16_03
  ; DW_LNS_set_file = 16_04
  ; DW_LNS_set_column = 16_05
  ; DW_LNS_negate_stmt = 16_06
  ; DW_LNS_set_basic_block = 16_07
  ; DW_LNS_const_add_pc = 16_08
  ; DW_LNS_fixed_advance_pc = 16_09
  ; DW_LNS_set_prologue_end = 16_0a
  ; DW_LNS_set_epilogue_begin = 16_0b
  ; DW_LNS_set_isa = 16_0c

(* Line Number Extended Opcode Encodings: *)
; CONST
   DW_LNE_end_sequence = 16_01
  ; DW_LNE_set_address = 16_02
  ; DW_LNE_define_file = 16_03
  ; DW_LNE_set_discriminator = 16_04
  ; DW_LNE_lo_user = 16_80
  ; DW_LNE_hi_user = 16_ff

(* Macinfo Type Encodings: *) 
; CONST
    DW_MACINFO_define = 16_01
  ; DW_MACINFO_undef = 16_02
  ; DW_MACINFO_start_file = 16_03
  ; DW_MACINFO_end_file = 16_04
  ; DW_MACINFO_vendor_ext = 16_ff

(* Call frame instruction encodings: *)
; CONST
    DW_CFA_extended = 16_00
  ; DW_CFA_nop = 16_00
  ; DW_CFA_advance_loc = 16_40
  ; DW_CFA_offset = 16_80
  ; DW_CFA_restore = 16_c0
  ; DW_CFA_set_loc = 16_01
  ; DW_CFA_advance_loc1 = 16_02
  ; DW_CFA_advance_loc2 = 16_03
  ; DW_CFA_advance_loc4 = 16_04
  ; DW_CFA_offset_extended = 16_05
  ; DW_CFA_restore_extended = 16_06
  ; DW_CFA_undefined = 16_07
  ; DW_CFA_same_value = 16_08
  ; DW_CFA_register = 16_09
  ; DW_CFA_remember_state = 16_0a
  ; DW_CFA_restore_state = 16_0b
  ; DW_CFA_def_cfa = 16_0c
  ; DW_CFA_def_cfa_register = 16_0d
  ; DW_CFA_def_cfa_offset = 16_0e
  ; DW_CFA_def_cfa_expression = 16_0f
  ; DW_CFA_expression = 16_10
  ; DW_CFA_offset_extended_sf = 16_11
  ; DW_CFA_def_cfa_sf = 16_12
  ; DW_CFA_def_cfa_offset_sf = 16_13
  ; DW_CFA_val_offset = 16_14
  ; DW_CFA_val_offset_sf = 16_15
  ; DW_CFA_val_expression = 16_16
  ; DW_CFA_MIPS_advance_loc8 = 16_1d
  ; DW_CFA_GNU_window_save = 16_2d
  ; DW_CFA_GNU_args_size = 16_2e
  ; DW_CFA_lo_user = 16_1c
  ; DW_CFA_hi_user = 16_3f

(* Children flag: *)
; CONST
    DW_CHILDREN_no = 16_00
  ; DW_CHILDREN_yes = 16_01

(* What are these?  They are in the same C enumeration as DW_CHILDREN, 
   with duplicate values? *) 
; CONST
    DW_EH_PE_absptr = 16_00
  ; DW_EH_PE_omit = 16_ff
  ; DW_EH_PE_uleb128 = 16_01
  ; DW_EH_PE_udata2 = 16_02
  ; DW_EH_PE_udata4 = 16_03
  ; DW_EH_PE_udata8 = 16_04
  ; DW_EH_PE_sleb128 = 16_09
  ; DW_EH_PE_sdata2 = 16_0A
  ; DW_EH_PE_sdata4 = 16_0B
  ; DW_EH_PE_sdata8 = 16_0C
  ; DW_EH_PE_signed = 16_08
  ; DW_EH_PE_pcrel = 16_10
  ; DW_EH_PE_textrel = 16_20
  ; DW_EH_PE_datarel = 16_30
  ; DW_EH_PE_funcrel = 16_40
  ; DW_EH_PE_aligned = 16_50
  ; DW_EH_PE_indirect = 16_80

(* Constants for debug_loc.dwo in the DWARF5 Split Debug Info Proposal: *)
; TYPE DW_LLE 
    = { DW_LLE_end_of_list_entry
      , DW_LLE_base_address_selection_entry
      , DW_LLE_start_end_entry
      , DW_LLE_start_length_entry
      , DW_LLE_offset_pair_entry
      }

(* Constants for the DW_APPLE_PROPERTY_attributes attribute: *) 
(* Keep this list in sync with clang's DeclSpec.h ObjCPropertyAttributeKind. *) 
; CONST

  (* Apple Objective-C Property Attributes: *) 
    DW_APPLE_PROPERTY_readonly = 16_01
  ; DW_APPLE_PROPERTY_getter = 16_02
  ; DW_APPLE_PROPERTY_assign = 16_04
  ; DW_APPLE_PROPERTY_readwrite = 16_08
  ; DW_APPLE_PROPERTY_retain = 16_10
  ; DW_APPLE_PROPERTY_copy = 16_20
  ; DW_APPLE_PROPERTY_nonatomic = 16_40
  ; DW_APPLE_PROPERTY_setter = 16_80
  ; DW_APPLE_PROPERTY_atomic = 16_100
  ; DW_APPLE_PROPERTY_weak =   16_200
  ; DW_APPLE_PROPERTY_strong = 16_400
  ; DW_APPLE_PROPERTY_unsafe_unretained = 16_800

(* Constants for the DWARF5 Accelerator Table Proposal: *) 
; CONST

  (* Data layout descriptors: *)
    DW_ATOM_null = 0        (* Marker as the end of a list of atoms. *)
  ; DW_ATOM_die_offset = 1  (* DIE offset in the debug_info section. *)
  ; DW_ATOM_cu_offset = 2   (* Offset of the compile unit header that contains 
                              the item in question. *)
  ; DW_ATOM_die_tag = 3     (* A tag entry. *)
  ; DW_ATOM_type_flags = 4  (* Set of flags for a type. *)

  (* DW_ATOM_type_flags values: *)

  (* Always set for C++; only set for ObjC if this is the @implementation for a
     class. *)
  ; DW_FLAG_type_implementation = 2

  (* Hash functions: *)

  (* Daniel J. Bernstein hash*)
  ; DW_hash_function_djb = 0

(* Constants for the GNU pubnames/pubtypes extensions supporting gdb index: *)
; TYPE GIEK
    = { GIEK_NONE
      , GIEK_TYPE
      , GIEK_VARIABLE
      , GIEK_FUNCTION
      , GIEK_OTHER
      , GIEK_UNUSED5
      , GIEK_UNUSED6
      , GIEK_UNUSED7

        (* GDBIndexEntryLinkage: *) 
      , GIEL_EXTERNAL
      , GIEL_STATIC
      }

; END DwarfConst
.
