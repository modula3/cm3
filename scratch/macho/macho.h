typedef int BOOL;
typedef signed char int8, INT8;
typedef unsigned char BOOL8, UINT8, uint8_t, uchar, UCHAR;
typedef unsigned short UINT16, uint16_t, uint16;
typedef short int16, INT16;
typedef int int32, INT, INT32;
typedef unsigned UINT32, uint32, uint32_t, uint, UINT;
#if defined(_MSC_VER) || defined(__DECC)
typedef __int64 int64, INT64, int64_t, int64;
typedef unsigned __int64 UINT64, uint64_t, int64;
#else
typedef long long INT64, int64_t, int64;
typedef unsigned long long UINT64, uint64_t, uint64;
#endif
typedef unsigned long ulong, ULONG, ulong_t;

#define macho_vm_prot_none          0x00
#define macho_vm_prot_read          0x01
#define macho_vm_prot_write         0x02
#define macho_vm_prot_execute       0x04
#define macho_vm_prot_default       (macho_vm_prot_read | macho_vm_prot_write)
#define macho_vm_prot_all           (macho_vm_prot_read | macho_vm_prot_write | macho_vm_prot_execute)
#define macho_vm_prot_no_change     0x08
#define macho_vm_prot_copy          0x10
#define macho_vm_prot_wants_copy    0x10

/* macho_header_t.magic */

#define macho_magic32                   0xFEEDFACE
#define macho_magic64                   0xFEEDFACF
#define macho_magic32_reversed          0xCEFAEDFE
#define macho_magic64_reversed          0xCFFAEDFE

/* macho_header_t.cputype */

#define macho_cpu_type_64bit            0x1000000
#define macho_cpu_type_x86              0x07
#define macho_cpu_type_x86_64           (macho_cpu_type_x86 | macho_cpu_type_64bit)
#define macho_cpu_type_powerpc          0x12
#define macho_cpu_type_powerpc64        (macho_cpu_type_powerpc | macho_cpu_type_64bit)

/* macho_header_t.cpusubtype */

#define macho_cpu_subtype_powerpc_all   0x00
#define macho_cpu_subtype_x86_all       0x03

/* macho_header_t.filetype */

#define macho_type_object                   0x01
#define macho_type_execute                  0x02
#define macho_type_fixed_vm_library         0x03
#define macho_type_core                     0x04
#define macho_type_preload                  0x05
#define macho_type_dylib                    0x06
#define macho_type_dylinker                 0x07
#define macho_type_bundle                   0x08
#define macho_type_dylib_stub               0x09
#define macho_type_dsym                     0x0A

/* macho_header_t.flags */

#define macho_flag_noundefs                 0x000001
#define macho_flag_incremental_link         0x000002
#define macho_flag_dynamic_link             0x000004
#define macho_flag_bind_at_load             0x000008
#define macho_flag_prebound                 0x000010
#define macho_flag_split_segments           0x000020
#define macho_flag_lazy_init                0x000040
#define macho_flag_twolevel                 0x000080
#define macho_flag_force_flat               0x000100
#define macho_flag_no_multi_defs            0x000200
#define macho_flag_no_fix_prebinding        0x000400
#define macho_flag_prebindable              0x000800
#define macho_flag_all_mods_bound           0x001000
#define macho_flag_subsections_via_symbols  0x002000
#define macho_flag_canonical                0x004000
#define macho_flag_weak_defines             0x008000
#define macho_flag_binds_to_weak            0x010000
#define macho_flag_allow_stack_executation  0x020000
#define macho_flag_root_safe                0x040000
#define macho_flag_setuid_safe              0x080000
#define macho_flag_no_reexported_dylibs     0x100000
#define macho_flag_position_independent_executable  0x200000


/* macho_load_command_t.cmd */

/* This bit indicates that the command must be understood
 * by the dynamic linker, else it should fail to load the file.
 * Commands without this bit that are not understood are ignored.
 */
#define macho_loadcommand_require_dyld      0x80000000

#define macho_loadcommand_segment32         0x01
#define macho_loadcommand_symtab            0x02
#define macho_loadcommand_symseg            0x03 /* gdb symbol table info (obsolete) */
#define macho_loadcommand_thread            0x04
#define macho_loadcommand_unixthread        0x05
#define macho_loadcommand_fixed_vm_lib      0x06
#define macho_loadcommand_id_fixed_vm_lib   0x07
#define macho_loadcommand_ident             0x08 /* obsolete */
#define macho_loadcommand_fixed_vm_file     0x09 /* internal */
#define macho_loadcommand_prepage           0x0A /* internal */
#define macho_loadcommand_dysymtab          0x0B
#define macho_loadcommand_load_dylib        0x0C
#define macho_loadcommand_id_dylib          0x0D
#define macho_loadcommand_load_dylinker     0x0E
#define macho_loadcommand_id_dylinker       0x0F
#define macho_loadcommand_prebound_dylib    0x10
#define macho_loadcommand_routines32        0x11
#define macho_loadcommand_sub_framework     0x12
#define macho_loadcommand_sub_umbrella      0x13
#define macho_loadcommand_sub_client        0x14
#define macho_loadcommand_sub_library       0x15
#define macho_loadcommand_twolevel_hints    0x16
#define macho_loadcommand_prebind_checksum  0x17
#define macho_loadcommand_load_weak_dylib   (0x18 | macho_loadcommand_require_dyld)
#define macho_loadcommand_segment64         0x19
#define macho_loadcommand_routines64        0x1A
#define macho_loadcommand_uuid              0x1B
#define macho_loadcommand_rpath             (0x1C | macho_loadcommand_require_dyld)
#define macho_loadcommand_code_signature    0x1D
#define macho_loadcommand_segment_split_info 0x1E
#define macho_loadcommand_reexport_dylib    (0x1F | macho_loadcommand_require_dyld)
#define macho_loadcommand_lazy_load_dylib   0x20
#define macho_loadcommand_encryption_info   0x21

typedef struct _macho_header32_t {
    uint32_t magic;
    uint32_t cputype;
    uint32_t cpusubtype;
    uint32_t filetype;
    uint32_t ncmds;
    uint32_t sizeofcmds;
    uint32_t flags;
} macho_header32_t;

typedef struct _macho_header64_t {
    uint32_t magic;
    uint32_t cputype;
    uint32_t cpusubtype;
    uint32_t filetype;
    uint32_t ncmds;
    uint32_t sizeofcmds;
    uint32_t flags;
    uint32_t reserved;
} macho_header64_t;

typedef struct _macho_loadcommand_t {
    uint32_t cmd;
    uint32_t cmdsize;
} macho_loadcommand_t;

/* macho_segment_t.flags */

#define macho_segment_flag_highvm               0x01
#define macho_segment_flag_fixed_vm_lib         0x02
#define macho_segment_flag_no_reloc             0x04
#define macho_segment_flag_protected_version_1  0x08

typedef struct _macho_segment32_t {
    uint32_t cmd;
    uint32_t cmdsize;
    char segname[16];
    uint32_t vmaddr;
    uint32_t vmsize;
    uint32_t fileoff;
    uint32_t filesize;
    uint32_t maxprot;
    uint32_t initprot;
    uint32_t nsects;
    uint32_t flags;
} macho_segment32_t;

typedef struct _macho_segment64_t {
    uint32_t cmd;
    uint32_t cmdsize;
    char segname[16];
    uint64_t vmaddr;
    uint64_t vmsize;
    uint64_t fileoff;
    uint64_t filesize;
    uint32_t maxprot;
    uint32_t initprot;
    uint32_t nsects;
    uint32_t flags;
} macho_segment64_t;

typedef struct _macho_section32_t {
    char        sectname[16];
    char        segname[16];
    uint32_t    addr;
    uint32_t    size;
    uint32_t    offset;
    uint32_t    align;
    uint32_t    reloff;
    uint32_t    nreloc;
    uint32_t    flags;
    uint32_t    reserved1;
    uint32_t    reserved2;
} macho_section32_t;

typedef struct _macho_section64_t {
    char        sectname[16];
    char        segname[16];
    uint64_t    addr;
    uint64_t    size;
    uint32_t    offset;
    uint32_t    align;
    uint32_t    reloff;
    uint32_t    nreloc;
    uint32_t    flags;
    uint32_t    reserved1;
    uint32_t    reserved2;
    uint32_t    reserved3;
} macho_section64_t;

#define macho_section_type                      0x000000FF
#define macho_section_attributes                0xFFFFFF00

/* section types */
#define macho_stype_regular                       0x00
#define macho_stype_zerofill                      0x01
#define macho_stype_string_literals               0x02
#define macho_stype_4byte_literals                0x03
#define macho_stype_8byte_literals                0x04
#define macho_stype_literal_pointers              0x05
#define macho_stype_non_lazy_symbol_pointers      0x06
#define macho_stype_lazy_symbol_pointers          0x07
#define macho_stype_symbol_stubs                  0x08
#define macho_stype_mod_init_func_pointers        0x09
#define macho_stype_mod_term_func_pointers        0x0A
#define macho_stype_coalesced                     0x0B
#define macho_stype_gb_zerofill                   0x0C
#define macho_stype_interposing                   0x0D
#define macho_stype_16byte_literals               0x0E
#define macho_stype_dtrace                        0x0F
#define macho_stype_lazy_dylib_symbol_pointers    0x10

/* section attributes */

#define macho_sattrs_usr                        0xFF000000 /* user settable attributes */
#define macho_sattr_pure_instructions           0x80000000
#define macho_sattr_no_toc                      0x40000000
#define macho_sattr_strip_static_syms           0x20000000
#define macho_sattr_no_dead_strip               0x10000000
#define macho_sattr_live_support                0x08000000
#define macho_sattr_self_modifying_code         0x04000000
#define macho_sattr_debug                       0x02000000
#define macho_sattr_sys                         0x00FFFF00 /* system settable attributes */
#define macho_sattr_some_instructions           0x00000400
#define macho_sattr_external_relocs             0x00000200
#define macho_sattr_local_relocs                0x00000100

/* segment/section names */

#define macho_segname_pagezero              "__PAGEZERO"

#define macho_segname_text                  "__TEXT"
#define macho_sectname_text                 "__text"
#define macho_sectname_fixed_vm_lib_init0   "__fvmlib_init0"
#define macho_sectname_fixed_vm_lib_init1   "__fvmlib_init1"

#define macho_segname_data                  "__DATA"
#define macho_sectname_data                 "__data"
#define macho_sectname_bss                  "__bss"
#define macho_sectname_common               "__common"

#define macho_segname_objc                  "__OBJC"
#define macho_sectname_objc_symbols         "__symbol_table"
#define macho_sectname_objc_modules         "__module_info"
#define macho_sectname_objc_refs            "__selector_refs"
#define macho_sectname_objc_strings         "__selector_strs"

#define macho_segname_icon                  "__ICON"
#define macho_sectname_icon_header          "__header"
#define macho_sectname_icon_tiff            "__tiff"

#define macho_segname_import                "__IMPORT"
#define macho_segname_linkedit              "__LINKEDIT"
#define macho_segname_unixstack             "__UNIXSTACK"

typedef union _macho_loadcommand_string_t {
    uint32_t offset;
} macho_loadcommand_string_t;

/* fixed_vm_lib is obsolete */

typedef struct _macho_fixed_vm_lib_t {
    macho_loadcommand_string_t  name;
    uint32_t                    minor_version;
    uint32_t                    header_addr;
} macho_fixed_vm_lib_t;

typedef struct _macho_fixed_vm_lib_command_t {
    uint32_t                cmd;
    uint32_t                cmdsize;
    macho_fixed_vm_lib_t    fixed_vm_lib;
} macho_fixed_vm_lib_command_t;

typedef struct _macho_dylib_t {
    macho_loadcommand_string_t  name;
    uint32_t                    timestamp;
    uint32_t                    current_version;
    uint32_t                    compability_version;
} macho_dylib_t;

typedef struct _macho_dylib_command_t {
    uint32_t        cmd;
    uint32_t        cmdsize;
    macho_dylib_t   dylib;
} macho_dylib_command_t;

typedef struct _macho_sub_client_command_t {
    uint32_t    cmd;
    uint32_t    cmdsize;
    macho_loadcommand_string_t client;
} macho_sub_client_command_t;

typedef struct _macho_sub_framework_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    macho_loadcommand_string_t umbrella;
} macho_sub_framework_command_t;

typedef struct _macho_sub_umbrella_command_t {
    uint32_t    cmd;
    uint32_t    cmdsize;
    macho_loadcommand_string_t sub_umbrella;
} macho_sub_umbrella_command_t;

typedef struct _macho_sub_library_command_t {
    uint32_t    cmd;
    uint32_t    cmdsize;
    macho_loadcommand_string_t sub_library;
} macho_sub_library_command_t;

/* TBD: don't use bitfields */
typedef struct _macho_twolevel_hint_t {
    uint32_t isub_image : 8;
    uint32_t itoc : 24;
} macho_twolevel_hint_t;

typedef struct _macho_twolevel_hints_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t offset;
    uint32_t nhints;
} macho_twolevel_hints_command_t;

typedef struct _macho_uuid_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint8_t uuid[16];
} macho_uuid_command_t;

typedef struct _macho_dylinker_command_t {
    uint32_t    cmd;
    uint32_t    cmdsize;
    macho_loadcommand_string_t name;
} macho_dylinker_command_t;

typedef struct _macho_prebound_dylib_command_t {
    uint32_t    cmd;
    uint32_t    cmdsize;
    macho_loadcommand_string_t name;
    uint32_t    nmodules;
    macho_loadcommand_string_t linked_modules;
} macho_prebound_dylib_command_t;

typedef struct _macho_thread_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t flavor;
    uint32_t count;
    /* state */
} macho_thread_command_t;

typedef struct _macho_routines32_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t init_address;
    uint32_t init_module;
    uint32_t reserved1;
    uint32_t reserved2;
    uint32_t reserved3;
    uint32_t reserved4;
    uint32_t reserved5;
    uint32_t reserved6;
} macho_routines32_command_t;

typedef struct _macho_routines64_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint64_t init_address;
    uint64_t init_module;
    uint64_t reserved1;
    uint64_t reserved2;
    uint64_t reserved3;
    uint64_t reserved4;
    uint64_t reserved5;
    uint64_t reserved6;
} macho_routines64_command_t;

typedef struct _macho_symtab_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t symoff;
    uint32_t nsyms;
    uint32_t stroff;
    uint32_t strsize;
} macho_symtab_command_t;

/* any of these bits set means the whole thing is
 * a stab value */
#define macho_n_stab 0xE0

#define macho_n_pext 0x10 /* private extern */
#define macho_n_type 0x0E
#define macho_n_ext  0x01 /* extern */

/* type bits */
#define macho_n_undf 0x00 /* undefined */
#define macho_n_abs  0x02 /* absolute; n_sect = NO_SECT */
#define macho_n_indr 0x0A
#define macho_n_pbud 0x0C /* undefined/prebound; n_sect = NO_SECT
#define macho_n_sect 0x0E /* defined in section n_sect */

/* n_desc */
#define macho_reference_mask                        0x0F
#define macho_reference_undefined_non_lazy          0x00
#define macho_reference_undefined_lazy              0x01
#define macho_reference_defined                     0x02
#define macho_reference_private_defined             0x03
#define macho_reference_private_undefined_non_lazy  0x04
#define macho_reference_private_undefined_lazy      0x05
#define macho_referenced_dynamically                0x10
#define macho_n_desc_discarded                      0x20
#define macho_n_no_dead_strip                       0x20
#define macho_n_weak_ref                            0x40
#define macho_n_weak_def                            0x80

typedef struct _macho_nlist32_t {
    int32_t n_strx;
    uint8_t n_type;
    uint8_t n_sect;
    uint16_t n_desc;
    uint32_t n_value;
} macho_nlist32_t;

typedef struct _macho_nlist64_t {
    int32_t n_strx;
    uint8_t n_type;
    uint8_t n_sect;
    uint16_t n_desc;
    uint64_t n_value;
} macho_nlist64_t;

typedef struct _macho_dysymtab_command_t {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t ilocalsym;
    uint32_t nlocalsym;
    uint32_t iextdefsym;
    uint32_t nextdefsym;
    uint32_t iundefsym;
    uint32_t nundefsym;
    uint32_t tocoff;
    uint32_t ntoc;
    uint32_t modtaboff;
    uint32_t nmodtab;
    uint32_t extrefsymoff;
    uint32_t nextrefsyms;
    uint32_t indirectsymoff;
    uint32_t nindirectsyms;
    uint32_t extreloff;
    uint32_t locreloff;
    uint32_t nlocrel;
} macho_dysymtab_command_t;

typedef struct _macho_dylib_table_of_contents {
    uint32_t symbol_index;
    uint32_t module_index;
} macho_dylib_table_of_contents;

typedef struct _macho_dylib_module32_t {
    uint32_t module_name;
    uint32_t iextdefsym;
    uint32_t nextdefsym;
    uint32_t irefsym;
    uint32_t nrefsym;
    uint32_t ilocalsym;
    uint32_t nlocalsym;
    uint32_t iextrel;
    uint32_t nextrel;
    uint32_t iinit_iterm;
    uint32_t ninit_nterm;
    uint32_t objc_module_info_addr;
    uint32_t objc_module_info_size;
} macho_dylib_module32_t;

typedef struct _macho_dylib_module64_t {
    uint32_t module_name;
    uint32_t iextdefsym;
    uint32_t nextdefsym;
    uint32_t irefsym;
    uint32_t nrefsym;
    uint32_t ilocalsym;
    uint32_t nlocalsym;
    uint32_t iextrel;
    uint32_t nextrel;
    uint32_t iinit_iterm;
    uint32_t ninit_nterm;
    uint32_t objc_module_info_size;
    uint64_t objc_module_info_addr;
} macho_dylib_module64_t;
