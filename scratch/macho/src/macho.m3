UNSAFE MODULE macho EXPORTS macho, Main;

TYPE enum_t = RECORD
  name: TEXT;
  value: INTEGER;
END;

TYPE field_t = RECORD
  name:                 TEXT;
  offset:               uchar := 0; (* small integer *)
  size:                 uchar := 4; (* small integer *)
  element_size:         uchar := 0; (* small integer *)
  str:                  uchar := 0; (* BOOLEAN *)
  macho_string:         uchar := 0; (* BOOLEAN *)
  enum_table_count:     uchar := 0; (* small integer *)
  enum_table: UNTRACED REF enum_t := NIL;
END;

TYPE struct_t = RECORD
    name:               TEXT;
    size:               uint;
    nfields:            uint;
    fields:             UNTRACED REF field_t;
    widest_field:       int;
END;

(* define STRUCT(a) {STRINGIZE(a), sizeof(a), NUMBER_OF(PASTE(a,_fields)), PASTE(a,_fields)} *)

VAR macho_magic_names := ARRAY [0..1] OF enum_t{
    enum_t{ "magic32", macho_magic32 },
    enum_t{ "magic64", macho_magic64 } };

VAR macho_cputype_names := ARRAY [0..3] OF enum_t{
    enum_t{ "x86", macho_cpu_type_x86 },
    enum_t{ "amd64", macho_cpu_type_amd64 },
    enum_t{ "powerpc", macho_cpu_type_powerpc },
    enum_t{ "powerpc64", macho_cpu_type_powerpc64 } };

VAR macho_cpusubtype_names := ARRAY [0..1] OF enum_t{
    enum_t{ "powerpc_all", macho_cpu_subtype_powerpc_all },
    enum_t{ "x86_all", macho_cpu_subtype_x86_all } };

VAR macho_filetype_names := ARRAY [0..9] OF enum_t{
    enum_t{ "object", macho_type_object },
    enum_t{ "execute", macho_type_execute },
    enum_t{ "fixed_vm_library", macho_type_fixed_vm_library },
    enum_t{ "core", macho_type_core },
    enum_t{ "preload", macho_type_preload },
    enum_t{ "dylib", macho_type_dylib },
    enum_t{ "dylinker", macho_type_dylinker },
    enum_t{ "bundle", macho_type_bundle },
    enum_t{ "dylib_stub", macho_type_dylib_stub },
    enum_t{ "dsym", macho_type_dsym } };

VAR macho_header32_t_fields := ARRAY [0..6] OF field_t{
  field_t{ "magic", 0, 4, enum_table := ADR(macho_magic_names[0]), enum_table_count := NUMBER(macho_magic_names)},
  field_t{ "cputype", 4, 4, enum_table := ADR(macho_cputype_names[0]), enum_table_count := NUMBER(macho_cputype_names)},
  field_t{ "cpusubtype", 8, 4, enum_table := ADR(macho_cpusubtype_names[0]), enum_table_count := NUMBER(macho_cpusubtype_names)},
  field_t{ "filetype", 12, 4, enum_table := ADR(macho_filetype_names[0]), enum_table_count := NUMBER(macho_filetype_names)},
  field_t{ "ncmds", 16, 4},
  field_t{ "sizeofcmds", 20, 4},
  field_t{ "flags", 24, 4}};

(*
extern_const field_t
macho_header32_t_fields[] = {
    FIELD_ENUM(macho_header32_t, magic, macho_magic_names),
    FIELD_ENUM(macho_header32_t, cputype, macho_cputype_names),
    FIELD_ENUM(macho_header32_t, cpusubtype, macho_cpusubtype_names),
    FIELD_ENUM(macho_header32_t, filetype, macho_filetype_names),
    FIELD(macho_header32_t, ncmds),
    FIELD(macho_header32_t, sizeofcmds),
    FIELD(macho_header32_t, flags)
};

struct_t
struct_macho_header32 = STRUCT(macho_header32_t);
*)

(*
#include <stdlib.h>
#include <string.h>
#include "macho.h"
#include <stdio.h>
#include <stddef.h>
#include <limits.h>
#ifdef _WIN32
#define PRINT64 "I64"
#else
#define PRINT64 "ll"
#endif
#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))
#define STRINGIZEx(a) #a
#define STRINGIZE(a) STRINGIZEx(a)
#define PASTEx(a, b) a##b
#define PASTE(a, b) PASTEx(a, b)

/* const means static const in C++, and extern const in C,
 * "extern const" portable means the same thing in either, but gcc warns.
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define extern_const extern const
#else
#define extern_const const
#endif

#define FIELD(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t* )0)->f))}
#define FIELD_ARRAY(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t* )0)->f)), sizeof((((t* )0)->f)[0]) }
#define FIELD_STRING(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t* )0)->f)), sizeof((((t* )0)->f)[0]), 1 }
#define FIELD_ENUM(t, f, e) {STRINGIZE(f), offsetof(t, f), sizeof((((t* )0)->f)), 0, 0, 0, sizeof(e)/sizeof((e)[0]), e }
#define FIELD_MACHO_STRING(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t* )0)->f)), 0, 0, 1 }

typedef union _macho_load_command_u {
    macho_loadcommand_t base;
    macho_segment32_t segment32;
    macho_segment64_t segment64;
} macho_load_command_u;

typedef struct _macho_file_t {
    const char* path;
    uchar* contents;
    size_t size;
    /* macho_header64 just adds uint32_t reserved at end */
    macho_header32_t* macho_header;
    uchar macho_header_size;
    uint32 ( *swap32)(uint32 a);
    uint64 ( *swap64)(uint64 a);
} macho_file_t;

uint32
no_swap32(uint32 a) { return a; }

uint64
no_swap64(uint64 a) { return a; }

uint32
swap32(uint32 a)
{
    return (a >> 24) | ((a >> 8) & 0xFF00) | ((a << 8) & 0xFF0000) | (a << 24);
}

uint64
swap64(uint64 a)
{
    const uint64 FF = 0xFF;
    return (a >> 56)
        | ((a >> 40) & (FF << 8))
        | ((a >> 24) & (FF << 16))
        | ((a >>  8) & (FF << 24))
        | ((a <<  8) & (FF << 32))
        | ((a << 24) & (FF << 40))
        | ((a << 40) & (FF << 48))
        | (a << 56);
}

/*
uint32
macho_swap32(macho_file_t* m, uint32 a)
{
    return m->swapped ? swap32(a) : a;
}

void
swap32p(uint32* a)
{
    *a = swap32( *a);
}

int
char_is_printable(char ch)
{
    return (ch >= 0x20 && ch <= 0x7E);
}

char
char_to_printable(char ch)
{
    return char_is_printable(ch) ? ch : '.';
}

void adjust_hex_case(char* a)
{
    a = strchr(a, 'X');
    if (a) *a = 'x';
    else return;
    a = strchr(a, 'X');
    if (a) *a = 'x';
#endif
}
*/

void
field_print(struct_t* s, const field_t* f, const void* p)
{
    char buffer[1024];
    char* cursor = buffer;
    uchar* q = (f->offset + (uchar* )p);
    uint32* q32 = (uint32* )q;
    uint64* q64 = (uint64* )q;
    uint i = {0};
    uchar size = f->size;
    uchar str = f->str;
    uchar macho_string = f->macho_string;
    const enum_t* enum_table = f->enum_table;
    uint32_t enum_table_count = f->enum_table_count;
    uint64 value = { 0 };
    uint64 value_swapped = { 0 };
    cursor += sprintf(cursor, "%s.%-*s ", s->name, s->widest_field, f->name);
    if (str)
    {
        uint length = 0;
        for (i = 0; i < size && q[i]; ++i)
            length += 1;
        while (length < 16)
        {
            length += 1;
            *cursor++ = ' ';
        }
        for (i = 0; i < size && q[i]; ++i)
            *cursor++ = q[i];
        *cursor++ = 0;
    }
    else
    {
        if (size == 4)
        {
            value = *q32;
            value_swapped = swap32(value);
        }
        else if (size == 8)
        {
            value = *q64;
            value_swapped = swap64(value);
        }
        if (sizeof(value) == sizeof(long))
            i = sprintf(cursor, "0x%lX 0x%lX", value, value_swapped);
        else
            i = sprintf(cursor, "0x%" PRINT64 "X 0x%" PRINT64 "X", value, value_swapped);
        /* adjust_hex_case(cursor); */
        if (enum_table && enum_table_count)
        {
          uint32_t e;
          for (e = 0; e < enum_table_count; ++e)
          {
            if (enum_table[e].value == value)
            {
              i += sprintf(cursor + i, " (%s)", enum_table[e].name);
              break;
            }
            else if (enum_table[e].value == value_swapped)
            {
              i += sprintf(cursor + i, " (%s, swapped)", enum_table[e].name);
              break;
            }
          }
        }
        else if (macho_string && size == 4)
        {
            if (value_swapped < value)
                value = value_swapped;
            i += sprintf(cursor + i, " (%s)", value + (char* )p);
        }
        cursor += i;
    }
    printf("%s\n", buffer);
}

void
struct_print(struct_t* t, const void* p)
{
    uint i;
    uint nfields = t->nfields;
    int widest_field = t->widest_field;
    if (widest_field == 0)
    {
        for (i = 0; i < nfields; ++i)
        {
            size_t j = strlen(t->fields[i].name);
            if (j > widest_field && j <= INT_MAX)
                widest_field = (int)j;
        }
        t->widest_field = widest_field;
     }
    for (i = 0; i < nfields; ++i)
        field_print(t, &t->fields[i], p);
}

const char*
macho_loadcommand_name(uint32_t cmd)
{
    static char a[99];
    switch (cmd & ~macho_loadcommand_require_dyld)
    {
#define X(a) case macho_loadcommand_##a & ~macho_loadcommand_require_dyld: return #a;
    X(segment32)
    X(symtab)
    X(symseg)
    X(thread)
    X(unixthread)
    X(fixed_vm_lib)
    X(id_fixed_vm_lib)
    X(ident)
    X(fixed_vm_file)
    X(prepage)
    X(dysymtab)
    X(load_dylib)
    X(id_dylib)
    X(load_dylinker)
    X(id_dylinker)
    X(prebound_dylib)
    X(routines32)
    X(sub_framework)
    X(sub_umbrella)
    X(sub_client)
    X(sub_library)
    X(twolevel_hints)
    X(prebind_checksum)
    X(load_weak_dylib)
    X(segment64)
    X(routines64)
    X(uuid)
    X(rpath)
    X(code_signature)
    X(segment_split_info)
    X(reexport_dylib)
    X(lazy_load_dylib)
    X(encryption_info)
#undef X
    }
    sprintf(a, "unknown (%u)", cmd);
    return a;
}

macho_loadcommand_t*
macho_first_load_command(macho_file_t* m)
{
    return (macho_loadcommand_t* )(m->macho_header_size + (uchar* )m->macho_header);
}

macho_loadcommand_t*
macho_next_load_command(macho_file_t* m, macho_loadcommand_t* L)
{
    return (macho_loadcommand_t* )(m->swap32(L->cmdsize) + (uchar* )L);
}

uint
macho_ncmds(macho_file_t* m)
{
    return m->swap32(m->macho_header->ncmds);
}

extern_const field_t
macho_segment32_t_fields[] = {
    FIELD(macho_segment32_t, cmd),
    FIELD(macho_segment32_t, cmdsize),
    FIELD_STRING(macho_segment32_t, segname),
    FIELD(macho_segment32_t, vmaddr),
    FIELD(macho_segment32_t, vmsize),
    FIELD(macho_segment32_t, fileoff),
    FIELD(macho_segment32_t, filesize),
    FIELD(macho_segment32_t, maxprot),
    FIELD(macho_segment32_t, initprot),
    FIELD(macho_segment32_t, nsects),
    FIELD(macho_segment32_t, flags)
};

struct_t
struct_macho_segment32 = STRUCT(macho_segment32_t);

extern_const field_t
macho_segment64_t_fields[] = {
    FIELD(macho_segment64_t, cmd),
    FIELD(macho_segment64_t, cmdsize),
    FIELD_STRING(macho_segment64_t, segname),
    FIELD(macho_segment64_t, vmaddr),
    FIELD(macho_segment64_t, vmsize),
    FIELD(macho_segment64_t, fileoff),
    FIELD(macho_segment64_t, filesize),
    FIELD(macho_segment64_t, maxprot),
    FIELD(macho_segment64_t, initprot),
    FIELD(macho_segment64_t, nsects),
    FIELD(macho_segment64_t, flags)
};

struct_t
struct_macho_segment64 = STRUCT(macho_segment64_t);

extern_const field_t
macho_section32_t_fields[] = {
    FIELD_STRING(macho_section32_t, sectname),
    FIELD_STRING(macho_section32_t, segname),
    FIELD(macho_section32_t, addr),
    FIELD(macho_section32_t, size),
    FIELD(macho_section32_t, offset),
    FIELD(macho_section32_t, align),
    FIELD(macho_section32_t, reloff),
    FIELD(macho_section32_t, nreloc),
    FIELD(macho_section32_t, flags),
    FIELD(macho_section32_t, reserved1),
    FIELD(macho_section32_t, reserved2)
};

struct_t
struct_macho_section32 = STRUCT(macho_section32_t);

extern_const field_t
macho_section64_t_fields[] = {
    FIELD_STRING(macho_section64_t, sectname),
    FIELD_STRING(macho_section64_t, segname),
    FIELD(macho_section64_t, addr),
    FIELD(macho_section64_t, size),
    FIELD(macho_section64_t, offset),
    FIELD(macho_section64_t, align),
    FIELD(macho_section64_t, reloff),
    FIELD(macho_section64_t, nreloc),
    FIELD(macho_section64_t, flags),
    FIELD(macho_section64_t, reserved1),
    FIELD(macho_section64_t, reserved2),
    FIELD(macho_section64_t, reserved3)
};

struct_t
struct_macho_section64 = STRUCT(macho_section64_t);

void
macho_dump_load_command_segmentX(macho_file_t* m,
                                 const void* L,
                                 uint32 load_command_size,
                                 struct_t* struct_macho_segmentX,
                                 struct_t* struct_macho_sectionX,
                                 uint32 section_count,
                                 uint32 section_size)
{
    uint32 i;
    uchar* sections = load_command_size + (uchar* )L;
    struct_print(struct_macho_segmentX, L);
    for (i = 0; i < section_count; ++i)
    {
      struct_print(struct_macho_sectionX, sections);
      sections += section_size;
    }
}

void
macho_dump_load_command_segment32(macho_file_t* m, macho_segment32_t* L)
{
    macho_dump_load_command_segmentX(m,
                                     L,
                                     sizeof( *L),
                                     &struct_macho_segment32,
                                     &struct_macho_section32,
                                     m->swap32(L->nsects),
                                     sizeof(macho_section32_t));
}

void
macho_dump_load_command_segment64(macho_file_t* m, macho_segment64_t* L)
{
    macho_dump_load_command_segmentX(m,
                                     L,
                                     sizeof( *L),
                                     &struct_macho_segment64,
                                     &struct_macho_section64,
                                     m->swap32(L->nsects),
                                     sizeof(macho_section64_t));
}

extern_const field_t
macho_symtab_command_t_fields[] = {
    FIELD(macho_symtab_command_t, symoff),
    FIELD(macho_symtab_command_t, nsyms),
    FIELD(macho_symtab_command_t, stroff),
    FIELD(macho_symtab_command_t, strsize)
};

struct_t
struct_macho_symtab_command = STRUCT(macho_symtab_command_t);

void
macho_dump_load_command_symtab(macho_file_t* m, macho_loadcommand_t* L)
{
    struct_print(&struct_macho_symtab_command, L);
}

void
macho_dump_load_command_symseg(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_thread(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_unixthread(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_fixed_vm_lib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_id_fixed_vm_lib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_ident(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_fixed_vm_file(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_prepage(macho_file_t* m, macho_loadcommand_t* L)
{
}

extern_const field_t
macho_dysymtab_command_t_fields[] = {
    FIELD(macho_dysymtab_command_t, ilocalsym),
    FIELD(macho_dysymtab_command_t, nlocalsym),
    FIELD(macho_dysymtab_command_t, iextdefsym),
    FIELD(macho_dysymtab_command_t, nextdefsym),
    FIELD(macho_dysymtab_command_t, iundefsym),
    FIELD(macho_dysymtab_command_t, nundefsym),
    FIELD(macho_dysymtab_command_t, tocoff),
    FIELD(macho_dysymtab_command_t, ntoc),
    FIELD(macho_dysymtab_command_t, modtaboff),
    FIELD(macho_dysymtab_command_t, nmodtab),
    FIELD(macho_dysymtab_command_t, extrefsymoff),
    FIELD(macho_dysymtab_command_t, nextrefsyms),
    FIELD(macho_dysymtab_command_t, indirectsymoff),
    FIELD(macho_dysymtab_command_t, nindirectsyms),
    FIELD(macho_dysymtab_command_t, extreloff),
    FIELD(macho_dysymtab_command_t, locreloff),
    FIELD(macho_dysymtab_command_t, nlocrel)
};

struct_t
struct_macho_dysymtab_command = STRUCT(macho_dysymtab_command_t);

void
macho_dump_load_command_dysymtab(macho_file_t* m, macho_loadcommand_t* L)
{
    struct_print(&struct_macho_dysymtab_command, L);
}

void
macho_dump_load_command_load_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_id_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}


extern_const field_t
macho_dylinker_command_t_fields[] = {
    FIELD_MACHO_STRING(macho_dylinker_command_t, name.offset),
};

struct_t
struct_macho_dylinker_command = STRUCT(macho_dylinker_command_t);

void
macho_dump_load_command_load_dylinker(macho_file_t* m, macho_loadcommand_t* L)
{
    struct_print(&struct_macho_dylinker_command, L);
}

void
macho_dump_load_command_id_dylinker(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_prebound_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_routines32(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_sub_framework(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_sub_umbrella(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_sub_client(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_sub_library(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_twolevel_hints(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_prebind_checksum(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_load_weak_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_routines64(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_uuid(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_rpath(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_code_signature(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_segment_split_info(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_reexport_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_lazy_load_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command_encryption_info(macho_file_t* m, macho_loadcommand_t* L)
{
}

void
macho_dump_load_command(macho_file_t* m, macho_loadcommand_t* L, uint i)
{
    uint cmd = m->swap32(L->cmd);
    printf("cmd %u %s\n", i, macho_loadcommand_name(cmd));
    switch (cmd)
    {
#define X(a) case macho_loadcommand_##a & ~macho_loadcommand_require_dyld: macho_dump_load_command_##a(m, (void* )L); break;
    X(segment32)
    X(symtab)
    X(symseg)
    X(thread)
    X(unixthread)
    X(fixed_vm_lib)
    X(id_fixed_vm_lib)
    X(ident)
    X(fixed_vm_file)
    X(prepage)
    X(dysymtab)
    X(load_dylib)
    X(id_dylib)
    X(load_dylinker)
    X(id_dylinker)
    X(prebound_dylib)
    X(routines32)
    X(sub_framework)
    X(sub_umbrella)
    X(sub_client)
    X(sub_library)
    X(twolevel_hints)
    X(prebind_checksum)
    X(load_weak_dylib)
    X(segment64)
    X(routines64)
    X(uuid)
    X(rpath)
    X(code_signature)
    X(segment_split_info)
    X(reexport_dylib)
    X(lazy_load_dylib)
    X(encryption_info)
#undef X
    }
}

void
macho_dump_load_commands(macho_file_t* m)
{
    uint ncmds = macho_ncmds(m);
    uint i;
    macho_loadcommand_t* L = macho_first_load_command(m);
    for (i = 0; i < ncmds; ++i)
    {
        macho_dump_load_command(m, L, i);
        L = macho_next_load_command(m, L);
    }
}

void
open_and_read_entire_file(const char* path, uchar** contents, size_t* size)
{
    uchar* buffer = { 0 };
    uchar* prev_buffer = { 0 };
    size_t buffer_size = { 0x10000 / 2 };
    size_t prev_buffer_size = { 0 };
    FILE* file = { 0 };
    size_t bytes_read = { 0 };
    
    *size = 0;
    *contents = 0;
    file = fopen(path, "rb");
    if (!file)
        goto Exit;
    while (1)
    {
        buffer_size *= 2;
        prev_buffer = buffer;
        buffer = (uchar* )malloc(buffer_size);
        if (!buffer)
            goto Exit;
        if (prev_buffer)
        {
            memcpy(buffer, prev_buffer, prev_buffer_size);
            free(prev_buffer);
            prev_buffer = 0;
        }
        bytes_read = fread(buffer + prev_buffer_size, 1, buffer_size - prev_buffer_size, file);
        if (bytes_read < (buffer_size - prev_buffer_size))
            break;
        prev_buffer_size = buffer_size;
    }
Exit:
    if (prev_buffer) free(prev_buffer);
    *size = prev_buffer_size + bytes_read;
    *contents = buffer;
    if (file) fclose(file);
}

int
main(int argc, char** argv)
{
    macho_file_t m = { 0 };
    uint32_t magic = { 0 };
    BOOLEAN swapped = { 0 };
    BOOLEAN m64 = { 0 };

    m.path = argv[1];
    if (!argv[1])
        exit(1);
    open_and_read_entire_file(m.path, &m.contents, &m.size);
    if (!m.contents)
      exit(1);
    m.macho_header = (macho_header32_t* )m.contents;
    magic = m.macho_header->magic;
    if (magic != macho_magic32 && magic != macho_magic32_reversed
        && magic != macho_magic64 && magic != macho_magic64_reversed)
    {
        exit(1);
    }
    m64 =     (magic == macho_magic64          || magic == macho_magic64_reversed);
    swapped = (magic == macho_magic32_reversed || magic == macho_magic64_reversed);
    m.swap32 = swapped ? swap32 : no_swap32;
    m.swap64 = swapped ? swap64 : no_swap64;
    m.macho_header_size = m64 ? sizeof(macho_header64_t) : sizeof(macho_header32_t);
    struct_print(&struct_macho_header32, m.contents);
    macho_dump_load_commands(&m);
    return 0;
}
*)

BEGIN
END macho.
