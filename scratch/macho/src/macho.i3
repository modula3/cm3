INTERFACE macho;
IMPORT Cstdint;

TYPE int8  = Cstdint.int8_t;  int8_t  = int8;  INT8  = int8;
     int16 = Cstdint.int16_t; int16_t = int16; INT16 = int16;
     int32 = Cstdint.int32_t; int32_t = int32; INT32 = int32;
     int64 = Cstdint.int64_t; int64_t = int64; INT64 = int64;
     uint8  = Cstdint.uint8_t;  uint8_t  = uint8;  UINT8  = uint8;
     uint16 = Cstdint.uint16_t; uint16_t = uint16; UINT16 = uint16;
     uint32 = Cstdint.uint32_t; uint32_t = uint32; UINT32 = uint32;
     uint64 = Cstdint.uint64_t; uint64_t = uint64; UINT64 = uint64;

CONST macho_vm_prot_none          = 16_00;
CONST macho_vm_prot_read          = 16_01;
CONST macho_vm_prot_write         = 16_02;
CONST macho_vm_prot_execute       = 16_04;
CONST macho_vm_prot_default       = macho_vm_prot_read + macho_vm_prot_write;
CONST macho_vm_prot_all           = macho_vm_prot_read + macho_vm_prot_write + macho_vm_prot_execute;
CONST macho_vm_prot_no_change     = 16_08;
CONST macho_vm_prot_copy          = 16_10;
CONST macho_vm_prot_wants_copy    = 16_10;

(* macho_header_t.magic *)

CONST macho_magic32                   = 16_FEEDFACE;
CONST macho_magic64                   = 16_FEEDFACF;
CONST macho_magic32_reversed          = 16_CEFAEDFE;
CONST macho_magic64_reversed          = 16_CFFAEDFE;

(* macho_header_t.cputype *)

CONST macho_cpu_type_64bit            = 16_1000000;
CONST macho_cpu_type_x86              = 16_07;
CONST macho_cpu_type_amd64            = macho_cpu_type_x86 + macho_cpu_type_64bit;
CONST macho_cpu_type_powerpc          = 16_12;
CONST macho_cpu_type_powerpc64        = macho_cpu_type_powerpc + macho_cpu_type_64bit;

(* alternate names *)
CONST macho_cpu_type_x86_64           = macho_cpu_type_amd64;
CONST macho_cpu_type_powerpc32        = macho_cpu_type_powerpc;
CONST macho_cpu_type_ppc              = macho_cpu_type_powerpc;
CONST macho_cpu_type_ppc32            = macho_cpu_type_powerpc;
CONST macho_cpu_type_ppc64            = macho_cpu_type_powerpc64;

(* macho_header_t.cpusubtype *)

CONST macho_cpu_subtype_powerpc_all   = 16_00;
CONST macho_cpu_subtype_x86_all       = 16_03;

(* macho_header_t.filetype *)

CONST macho_type_object                   = 16_01;
CONST macho_type_execute                  = 16_02;
CONST macho_type_fixed_vm_library         = 16_03;
CONST macho_type_core                     = 16_04;
CONST macho_type_preload                  = 16_05;
CONST macho_type_dylib                    = 16_06;
CONST macho_type_dylinker                 = 16_07;
CONST macho_type_bundle                   = 16_08;
CONST macho_type_dylib_stub               = 16_09;
CONST macho_type_dsym                     = 16_0A;

(* macho_header_t.flags *)

CONST macho_flag_noundefs                 = 16_000001;
CONST macho_flag_incremental_link         = 16_000002;
CONST macho_flag_dynamic_link             = 16_000004;
CONST macho_flag_bind_at_load             = 16_000008;
CONST macho_flag_prebound                 = 16_000010;
CONST macho_flag_split_segments           = 16_000020;
CONST macho_flag_lazy_init                = 16_000040;
CONST macho_flag_twolevel                 = 16_000080;
CONST macho_flag_force_flat               = 16_000100;
CONST macho_flag_no_multi_defs            = 16_000200;
CONST macho_flag_no_fix_prebinding        = 16_000400;
CONST macho_flag_prebindable              = 16_000800;
CONST macho_flag_all_mods_bound           = 16_001000;
CONST macho_flag_subsections_via_symbols  = 16_002000;
CONST macho_flag_canonical                = 16_004000;
CONST macho_flag_weak_defines             = 16_008000;
CONST macho_flag_binds_to_weak            = 16_010000;
CONST macho_flag_allow_stack_executation  = 16_020000;
CONST macho_flag_root_safe                = 16_040000;
CONST macho_flag_setuid_safe              = 16_080000;
CONST macho_flag_no_reexported_dylibs     = 16_100000;
CONST macho_flag_position_independent_executable  = 16_200000;


(* macho_load_command_t.cmd *)

(* This bit indicates that the command must be understood
 * by the dynamic linker, else it should fail to load the file.
 * Commands without this bit that are not understood are ignored.
 *)
CONST macho_loadcommand_require_dyld      = 16_80000000;

CONST macho_loadcommand_segment32         = 16_01;
CONST macho_loadcommand_symtab            = 16_02;
CONST macho_loadcommand_symseg            = 16_03; (* gdb symbol table info (obsolete) *)
CONST macho_loadcommand_thread            = 16_04;
CONST macho_loadcommand_unixthread        = 16_05;
CONST macho_loadcommand_fixed_vm_lib      = 16_06;
CONST macho_loadcommand_id_fixed_vm_lib   = 16_07;
CONST macho_loadcommand_ident             = 16_08; (* obsolete *)
CONST macho_loadcommand_fixed_vm_file     = 16_09; (* internal *)
CONST macho_loadcommand_prepage           = 16_0A; (* internal *)
CONST macho_loadcommand_dysymtab          = 16_0B;
CONST macho_loadcommand_load_dylib        = 16_0C;
CONST macho_loadcommand_id_dylib          = 16_0D;
CONST macho_loadcommand_load_dylinker     = 16_0E;
CONST macho_loadcommand_id_dylinker       = 16_0F;
CONST macho_loadcommand_prebound_dylib    = 16_10;
CONST macho_loadcommand_routines32        = 16_11;
CONST macho_loadcommand_sub_framework     = 16_12;
CONST macho_loadcommand_sub_umbrella      = 16_13;
CONST macho_loadcommand_sub_client        = 16_14;
CONST macho_loadcommand_sub_library       = 16_15;
CONST macho_loadcommand_twolevel_hints    = 16_16;
CONST macho_loadcommand_prebind_checksum  = 16_17;
CONST macho_loadcommand_load_weak_dylib   = 16_18 + macho_loadcommand_require_dyld;
CONST macho_loadcommand_segment64         = 16_19;
CONST macho_loadcommand_routines64        = 16_1A;
CONST macho_loadcommand_uuid              = 16_1B;
CONST macho_loadcommand_rpath             = 16_1C + macho_loadcommand_require_dyld;
CONST macho_loadcommand_code_signature    = 16_1D;
CONST macho_loadcommand_segment_split_info = 16_1E;
CONST macho_loadcommand_reexport_dylib    = 16_1F + macho_loadcommand_require_dyld;
CONST macho_loadcommand_lazy_load_dylib   = 16_20;
CONST macho_loadcommand_encryption_info   = 16_21;

TYPE macho_header32_t = RECORD
    magic:      uint32_t;
    cputype:    uint32_t;
    cpusubtype: uint32_t;
    filetype:   uint32_t;
    ncmds:      uint32_t;
    sizeofcmds: uint32_t;
    flags:      uint32_t;
END;

TYPE macho_header64_t = RECORD
    magic:      uint32_t;
    cputype:    uint32_t;
    cpusubtype: uint32_t;
    filetype:   uint32_t;
    ncmds:      uint32_t;
    sizeofcmds: uint32_t;
    flags:      uint32_t;
    reserved:   uint32_t;
END;

TYPE macho_loadcommand_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
END;

(* macho_segment_t.flags *)

CONST macho_segment_flag_highvm               = 16_01;
CONST macho_segment_flag_fixed_vm_lib         = 16_02;
CONST macho_segment_flag_no_reloc             = 16_04;
CONST macho_segment_flag_protected_version_1  = 16_08;

TYPE macho_segment32_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    segname:    ARRAY [0..15] OF CHAR;
    vmaddr:     uint32_t;
    vmsize:     uint32_t;
    fileoff:    uint32_t;
    filesize:   uint32_t;
    maxprot:    uint32_t;
    initproc:   uint32_t;
    nsects:     uint32_t;
    flags:      uint32_t;
END;

TYPE macho_segment64_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    segname:    ARRAY [0..15] OF CHAR;
    vmaddr:     uint64_t;
    vmsize:     uint64_t;
    fileoff:    uint64_t;
    filesize:   uint64_t;
    maxprot:    uint32_t;
    initproc:   uint32_t;
    nsects:     uint32_t;
    flags:      uint32_t;
END;

TYPE macho_section32_t = RECORD
    sectname:   ARRAY [0..15] OF CHAR;
    segname:    ARRAY [0..15] OF CHAR;
    addr:       uint32_t;
    size:       uint32_t;
    offset:     uint32_t;
    align:      uint32_t;
    reloff:     uint32_t;
    nreloc:     uint32_t;
    floags:     uint32_t;
    reserved1:  uint32_t;
    reserved2:  uint32_t;
END;

TYPE macho_section64_t = RECORD
    sectname:   ARRAY [0..15] OF CHAR;
    segname:    ARRAY [0..15] OF CHAR;
    addr:       uint64_t;
    size:       uint64_t;
    offset:     uint32_t;
    align:      uint32_t;
    reloff:     uint32_t;
    nreloc:     uint32_t;
    flags:      uint32_t;
    reserved1:  uint32_t;
    reserved2:  uint32_t;
    reserved3:  uint32_t;
END;

CONST macho_section_type                      = 16_000000FF;
CONST macho_section_attributes                = 16_FFFFFF00;

(* section types *)
CONST macho_stype_regular                       = 16_00;
CONST macho_stype_zerofill                      = 16_01;
CONST macho_stype_string_literals               = 16_02;
CONST macho_stype_4byte_literals                = 16_03;
CONST macho_stype_8byte_literals                = 16_04;
CONST macho_stype_literal_pointers              = 16_05;
CONST macho_stype_non_lazy_symbol_pointers      = 16_06;
CONST macho_stype_lazy_symbol_pointers          = 16_07;
CONST macho_stype_symbol_stubs                  = 16_08;
CONST macho_stype_mod_init_func_pointers        = 16_09;
CONST macho_stype_mod_term_func_pointers        = 16_0A;
CONST macho_stype_coalesced                     = 16_0B;
CONST macho_stype_gb_zerofill                   = 16_0C;
CONST macho_stype_interposing                   = 16_0D;
CONST macho_stype_16byte_literals               = 16_0E;
CONST macho_stype_dtrace                        = 16_0F;
CONST macho_stype_lazy_dylib_symbol_pointers    = 16_10;

(* section attributes *)

CONST macho_sattrs_usr                  = 16_FF000000; (* user settable attributes *)
CONST macho_sattr_pure_instructions     = 16_80000000;
CONST macho_sattr_no_toc                = 16_40000000;
CONST macho_sattr_strip_static_syms     = 16_20000000;
CONST macho_sattr_no_dead_strip         = 16_10000000;
CONST macho_sattr_live_support          = 16_08000000;
CONST macho_sattr_self_modifying_code   = 16_04000000;
CONST macho_sattr_debug                 = 16_02000000;
CONST macho_sattr_sys                   = 16_00FFFF00; (* system settable attributes *)
CONST macho_sattr_some_instructions     = 16_00000400;
CONST macho_sattr_external_relocs       = 16_00000200;
CONST macho_sattr_local_relocs          = 16_00000100;

(* segment/section names *)

CONST macho_segname_pagezero              = "__PAGEZERO";

CONST macho_segname_text                  = "__TEXT";
CONST macho_sectname_text                 = "__text";
CONST macho_sectname_fixed_vm_lib_init0   = "__fvmlib_init0";
CONST macho_sectname_fixed_vm_lib_init1   = "__fvmlib_init1";

CONST macho_segname_data                  = "__DATA";
CONST macho_sectname_data                 = "__data";
CONST macho_sectname_bss                  = "__bss";
CONST macho_sectname_common               = "__common";

CONST macho_segname_objc                  = "__OBJC";
CONST macho_sectname_objc_symbols         = "__symbol_table";
CONST macho_sectname_objc_modules         = "__module_info";
CONST macho_sectname_objc_refs            = "__selector_refs";
CONST macho_sectname_objc_strings         = "__selector_strs";

CONST macho_segname_icon                  = "__ICON";
CONST macho_sectname_icon_header          = "__header";
CONST macho_sectname_icon_tiff            = "__tiff";

CONST macho_segname_import                = "__IMPORT";
CONST macho_segname_linkedit              = "__LINKEDIT";
CONST macho_segname_unixstack             = "__UNIXSTACK";

TYPE macho_loadcommand_string_t = RECORD
    offset:     uint32_t;
END;

(* fixed_vm_lib is obsolete *)

TYPE macho_fixed_vm_lib_t = RECORD
    name:               macho_loadcommand_string_t;
    minor_version:      uint32_t;
    header_addr:        uint32_t;
END;

TYPE macho_fixed_vm_lib_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    fixed_vm_lib:       macho_fixed_vm_lib_t;
END;

TYPE macho_dylib_t = RECORD
    name:                       macho_loadcommand_string_t;
    timestamp:                  uint32_t;
    current_version:            uint32_t;
    compability_version:        uint32_t;
END;

TYPE macho_dylib_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    dylib:      macho_dylib_t;
END;

TYPE macho_sub_client_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    client:     macho_loadcommand_string_t;
END;

TYPE macho_sub_framework_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    umbrella:   macho_loadcommand_string_t;
END;

TYPE macho_sub_umbrella_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    sub_umbrella:       macho_loadcommand_string_t;
END;

TYPE macho_sub_library_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    sub_library:        macho_loadcommand_string_t;
END;

(* TBD: don't use bitfields
TYPE macho_twolevel_hint_t = RECORD
    uint32_t isub_image : 8;
    uint32_t itoc : 24;
END;
*)

TYPE macho_twolevel_hints_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    offset:     uint32_t;
    nhints:     uint32_t;
END;

TYPE macho_uuid_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    uuid:       ARRAY [0..15] OF uint8_t;
END;

TYPE macho_dylinker_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    name:       macho_loadcommand_string_t;
END;

TYPE macho_prebound_dylib_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    name:               macho_loadcommand_string_t;
    nmodules:           uint32_t;
    linked_modules:     macho_loadcommand_string_t;
END;

TYPE macho_thread_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    flavor:     uint32_t;
    count:      uint32_t;
    (* state *)
END;

TYPE macho_routines32_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    init_address:       uint32_t;
    init_module:        uint32_t;
    reserved1:          uint32_t;
    reserved2:          uint32_t;
    reserved3:          uint32_t;
    reserved4:          uint32_t;
    reserved5:          uint32_t;
    reserved6:          uint32_t;
END;

TYPE macho_routines64_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    init_address:       uint64_t;
    init_module:        uint64_t;
    reserved1:          uint64_t;
    reserved2:          uint64_t;
    reserved3:          uint64_t;
    reserved4:          uint64_t;
    reserved5:          uint64_t;
    reserved6:          uint64_t;
END;

TYPE macho_symtab_command_t = RECORD
    cmd:        uint32_t;
    cmdsize:    uint32_t;
    symoff:     uint32_t;
    nsyms:      uint32_t;
    stroff:     uint32_t;
    strsize:    uint32_t;
END;

(* any of these bits set means the whole thing is
 * a stab value *)
CONST macho_n_stab = 16_E0;

CONST macho_n_pext = 16_10; (* private extern *)
CONST macho_n_type = 16_0E;
CONST macho_n_ext  = 16_01; (* extern *)

(* type bits *)
CONST macho_n_undf = 16_00; (* undefined *)
CONST macho_n_abs  = 16_02; (* absolute; n_sect = NO_SECT *)
CONST macho_n_indr = 16_0A;
CONST macho_n_pbud = 16_0C; (* undefined/prebound; n_sect = NO_SECT *)
CONST macho_n_sect = 16_0E; (* defined in section n_sect *)

(* n_desc *)
CONST macho_reference_mask                        = 16_0F;
CONST macho_reference_undefined_non_lazy          = 16_00;
CONST macho_reference_undefined_lazy              = 16_01;
CONST macho_reference_defined                     = 16_02;
CONST macho_reference_private_defined             = 16_03;
CONST macho_reference_private_undefined_non_lazy  = 16_04;
CONST macho_reference_private_undefined_lazy      = 16_05;
CONST macho_referenced_dynamically                = 16_10;
CONST macho_n_desc_discarded                      = 16_20;
CONST macho_n_no_dead_strip                       = 16_20;
CONST macho_n_weak_ref                            = 16_40;
CONST macho_n_weak_def                            = 16_80;

TYPE macho_nlist32_t = RECORD
    n_strx:     int32_t;
    n_type:     uint8_t;
    n_sect:     uint8_t;
    n_desc:     uint16_t;
    n_value:    uint32_t;
END;

TYPE macho_nlist64_t = RECORD
    n_strx:     int32_t;
    n_type:     uint8_t;
    n_sect:     uint8_t;
    n_desc:     uint16_t;
    n_value:    uint64_t;
END;

TYPE macho_dysymtab_command_t = RECORD
    cmd:                uint32_t;
    cmdsize:            uint32_t;
    ilocalsym:            uint32_t;
    nlocalsym:          uint32_t;
    iextdefsym:         uint32_t;
    nextdefsym:         uint32_t;
    iundefsym:          uint32_t;
    nundefsym:          uint32_t;
    tocoff:             uint32_t;
    ntoc:               uint32_t;
    modtaboff:          uint32_t;
    nmodtab:            uint32_t;
    extrefsymoff:       uint32_t;
    nextrefsyms:        uint32_t;
    indirectsymoff:     uint32_t;
    nindirectsyms:      uint32_t;
    extreloff:          uint32_t;
    locreloff:          uint32_t;
    nlocrel:            uint32_t;
END;

TYPE macho_dylib_table_of_contents = RECORD
    symbol_index: uint32_t;
    module_index: uint32_t;
END;

TYPE macho_dylib_module32_t = RECORD
    module_name:                uint32_t;
    iextdefsym:                 uint32_t;
    nextdefsym:                 uint32_t;
    irefsym:                    uint32_t;
    nrefsym:                    uint32_t;
    ilocalsym:                  uint32_t;
    nlocalsym:                  uint32_t;
    iextrel:                    uint32_t;
    nextrel:                    uint32_t;
    iinit_iterm:                uint32_t;
    ninit_nterm:                uint32_t;
    objc_module_info_addr:      uint32_t;
    objc_module_info_size:      uint32_t;
END;

TYPE macho_dylib_module64_t = RECORD
    module_name:                uint32_t;
    iextdefsym:                 uint32_t;
    nextdefsym:                 uint32_t;
    irefsym:                    uint32_t;
    nrefsym:                    uint32_t;
    ilocalsym:                  uint32_t;
    nlocalsym:                  uint32_t;
    iextrel:                    uint32_t;
    nextrel:                    uint32_t;
    iinit_iterm:                uint32_t;
    ninit_nterm:                uint32_t;
    objc_module_info_size:      uint32_t;
    objc_module_info_addr:      uint64_t;
END;

END macho.
