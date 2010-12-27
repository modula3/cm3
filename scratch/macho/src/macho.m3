UNSAFE MODULE macho EXPORTS macho, Main;
IMPORT Word, Long, Fmt, Text, IO;

TYPE enum_t = RECORD
  name: TEXT;
  value: uint32_t;
END;

TYPE field_t = RECORD
  name:         TEXT;
  offset:       ADDRESS; (* small integer *)
  size:         CARDINAL; (* small integer *)
  str:          BOOLEAN := FALSE;
  macho_string: BOOLEAN := FALSE;
  enum:         REF ARRAY OF enum_t := NIL;
END;

TYPE struct_t = RECORD
  name:           TEXT;
  size:           CARDINAL;
  fields:         REF ARRAY OF field_t := NIL;
  widest_field:   CARDINAL := 0; (* small integer *)
END;

PROCEDURE CopyEnumArray(READONLY a:ARRAY OF enum_t):REF ARRAY OF enum_t =
(* Modula-3 inefficiency.. *)
VAR b := NEW(REF ARRAY OF enum_t, NUMBER(a));
BEGIN
  b^ := a;
  RETURN b;
END CopyEnumArray;

PROCEDURE CopyFieldArray(READONLY a:ARRAY OF field_t):REF ARRAY OF field_t =
(* Modula-3 inefficiency.. *)
VAR b := NEW(REF ARRAY OF field_t, NUMBER(a));
BEGIN
  b^ := a;
  RETURN b;
END CopyFieldArray;

(* define STRUCT(a) {STRINGIZE(a), sizeof(a), NUMBER_OF(PASTE(a,_fields)), PASTE(a,_fields)} *)

CONST macho_magic_names = ARRAY OF enum_t{
  enum_t{"magic32", macho_magic32},
  enum_t{"magic64", macho_magic64}};

CONST macho_cputype_names = ARRAY OF enum_t{
  enum_t{"x86", macho_cpu_type_x86},
  enum_t{"amd64", macho_cpu_type_amd64},
  enum_t{"powerpc", macho_cpu_type_powerpc},
  enum_t{"powerpc64", macho_cpu_type_powerpc64}};

CONST macho_cpusubtype_names = ARRAY OF enum_t{
  enum_t{"powerpc_all", macho_cpu_subtype_powerpc_all},
  enum_t{"x86_all", macho_cpu_subtype_x86_all}};

CONST macho_filetype_names = ARRAY OF enum_t{
  enum_t{"object", macho_type_object},
  enum_t{"execute", macho_type_execute},
  enum_t{"fixed_vm_library", macho_type_fixed_vm_library},
  enum_t{"core", macho_type_core},
  enum_t{"preload", macho_type_preload},
  enum_t{"dylib", macho_type_dylib},
  enum_t{"dylinker", macho_type_dylinker},
  enum_t{"bundle", macho_type_bundle},
  enum_t{"dylib_stub", macho_type_dylib_stub},
  enum_t{"dsym", macho_type_dsym}};

CONST macho_header32: UNTRACED REF macho_header32_t = NIL;

VAR macho_header32_t_fields := ARRAY [0..6] OF field_t{
  field_t{"magic", ADR(macho_header32.magic), BYTESIZE(macho_header32.magic), enum := CopyEnumArray(macho_magic_names)},
  field_t{"cputype", ADR(macho_header32.cputype), BYTESIZE(macho_header32.cputype), enum := CopyEnumArray(macho_cputype_names)},
  field_t{"cpusubtype", ADR(macho_header32.cpusubtype), BYTESIZE(macho_header32.cpusubtype), enum := CopyEnumArray(macho_cpusubtype_names)},
  field_t{"filetype", ADR(macho_header32.filetype), BYTESIZE(macho_header32.filetype), enum := CopyEnumArray(macho_filetype_names)},
  field_t{"ncmds", ADR(macho_header32.ncmds), BYTESIZE(macho_header32.ncmds)},
  field_t{"sizeofcmds", ADR(macho_header32.sizeofcmds), BYTESIZE(macho_header32.sizeofcmds)},
  field_t{"flags", ADR(macho_header32.flags), BYTESIZE(macho_header32.flags)}};

VAR struct_macho_header32 := struct_t{"macho_header32_t",
                                      BYTESIZE(macho_header32_t),
                                      CopyFieldArray(macho_header32_t_fields)};

TYPE macho_file_t = RECORD
    path:               TEXT;
    contents:           ADDRESS;
    size:               CARDINAL;
    macho_header:       UNTRACED REF macho_header32_t;
    macho_header_size:  uchar; (* small integer *)
    swap32: PROCEDURE(a:uint32):uint32 := no_swap32;
    swap64: PROCEDURE(a:uint64):uint64 := no_swap64;
END;

PROCEDURE no_swap32(a:uint32):uint32 =
BEGIN
  RETURN a;
END no_swap32;

PROCEDURE no_swap64(a:uint64):uint64 =
BEGIN
  RETURN a;
END no_swap64;

PROCEDURE swap32(a:uint32):uint32 =
CONST Left = Word.LeftShift; Right = Word.RightShift; And = Word.And; Or = Word.Or;
BEGIN
  RETURN Or(    Right(a, 24),
         Or(And(Right(a,  8), 16_FF00),
         Or(And(Left (a,  8), 16_FF0000),
                Left (a, 24))));
END swap32;

PROCEDURE swap64(a:uint64):uint64 =
CONST FF:uint64 = 16_FFL;
      Left = Long.LeftShift; Right = Long.RightShift; And = Long.And; Or = Long.Or;
BEGIN
    RETURN Or(    Right(a, 56),
           Or(And(Right(a, 40), Left(FF,  8)),
           Or(And(Right(a, 24), Left(FF, 16)),
           Or(And(Right(a,  8), Left(FF, 24)),
           Or(And(Left (a,  8), Left(FF, 32)),
           Or(And(Left (a, 24), Left(FF, 40)),
           Or(And(Left (a, 40), Left(FF, 48)),
                  Left (a, 56))))))));
END swap64;

PROCEDURE strnlen(a: UNTRACED REF uchar; max: CARDINAL): CARDINAL =
VAR i: CARDINAL := 0;
BEGIN
  WHILE i < max AND a^ # 0 DO
    INC(i);
    INC(a);
  END;
  RETURN i;
END strnlen;

PROCEDURE field_print(VAR s: struct_t; READONLY f: field_t; p: ADDRESS) =
VAR buffer: TEXT;
    q := LOOPHOLE(p + LOOPHOLE(f.offset, INTEGER), UNTRACED REF uchar);
    q32 := LOOPHOLE(q, UNTRACED REF uint32);
    q64 := LOOPHOLE(q, UNTRACED REF uint64);
    size := f.size;
    str := f.str;
    macho_string := f.macho_string;
    enum := f.enum;
    value32: uint32 := 0;
    value32_swapped: uint32 := 0;
    value64: uint64 := 0L;
    value64_swapped: uint64 := 0L;
    i: uint := 0;
    length: uint := 0;
BEGIN
  buffer := s.name & "." & Fmt.Pad(f.name, s.widest_field, ' ');
  IF str THEN
    length := strnlen(q + i, size);
    IF length < size THEN
      buffer := buffer & Fmt.Pad("", size - length);
    END;
    IF length > 0 THEN
      FOR i := 0 TO length - 1 DO
        buffer := buffer & Text.FromChar(LOOPHOLE(q + i, UNTRACED REF CHAR)^);
      END;
    END;
  ELSE
    IF size = 4 THEN
      value32 := q32^;
      value64 := VAL(value32, uint64);
      value32_swapped := swap32(value32);
      value64_swapped := VAL(value32_swapped, uint64);
     ELSIF size = 8 THEN
       value64 := q64^;
       value64_swapped := swap64(value64);
    END;
    buffer := buffer & "0x" & Fmt.LongUnsigned(value64) & " 0x" & Fmt.LongUnsigned(value64_swapped);
    IF enum # NIL THEN
      FOR e := 0 TO NUMBER(enum^) - 1 DO
        IF enum[e].value = value32 THEN
          buffer := buffer & " (" & enum[e].name & ")";
          EXIT;
        ELSIF enum[e].value = value32_swapped THEN
          buffer := buffer & " (" & enum[e].name & ", swapped)";
          EXIT;
        END;
      END;
    ELSIF macho_string AND size = 4 THEN
      IF Word.LT(value32_swapped, value32) THEN
        value32 := value32_swapped;
      END;
      buffer := buffer & " (";
      q := p + value32;
      WHILE q^ # 0 DO
        buffer := buffer & Text.FromChar(VAL(q^, CHAR));
        INC(q);
      END;
      buffer := buffer & ")";
    END;
  END;
  buffer := buffer & "\n";
  IO.Put(buffer);
END field_print;

PROCEDURE struct_print(VAR t: struct_t; p: ADDRESS) =
VAR widest_field := t.widest_field;
BEGIN
  IF widest_field = 0 THEN
    FOR i := 0 TO NUMBER(t.fields^) - 1 DO
      widest_field := MAX(widest_field, Text.Length(t.fields[i].name));
    END;
    t.widest_field := widest_field;
  END;
  FOR i := 0 TO NUMBER(t.fields^) - 1 DO
    field_print(t, t.fields[i], p);
  END;
END struct_print;

PROCEDURE macho_loadcommand_name(cmd: uint32_t):TEXT =
CONST mask = Word.Not(macho_loadcommand_require_dyld);
      And = Word.And;
BEGIN
  cmd := And(cmd, mask);
  CASE cmd OF
    | And(macho_loadcommand_segment32, mask)            => RETURN "segment32";
    | And(macho_loadcommand_symtab, mask)               => RETURN "symtab";
    | And(macho_loadcommand_symseg, mask)               => RETURN "symseg";
    | And(macho_loadcommand_thread, mask)               => RETURN "thread";
    | And(macho_loadcommand_unixthread, mask)           => RETURN "unixthread";
    | And(macho_loadcommand_fixed_vm_lib, mask)         => RETURN "fixed_vm_lib";
    | And(macho_loadcommand_id_fixed_vm_lib, mask)      => RETURN "id_fixed_vm_lib";
    | And(macho_loadcommand_ident, mask)                => RETURN "ident";
    | And(macho_loadcommand_fixed_vm_file, mask)        => RETURN "fixed_vm_file";
    | And(macho_loadcommand_prepage, mask)              => RETURN "prepage";
    | And(macho_loadcommand_dysymtab, mask)             => RETURN "dysymtab";
    | And(macho_loadcommand_load_dylib, mask)           => RETURN "load_dylib";
    | And(macho_loadcommand_id_dylib, mask)             => RETURN "id_dylib";
    | And(macho_loadcommand_load_dylinker, mask)        => RETURN "load_dylinker";
    | And(macho_loadcommand_id_dylinker, mask)          => RETURN "id_dylinker";
    | And(macho_loadcommand_prebound_dylib, mask)       => RETURN "prebound_dylib";
    | And(macho_loadcommand_routines32, mask)           => RETURN "routines32";
    | And(macho_loadcommand_sub_framework, mask)        => RETURN "sub_framework";
    | And(macho_loadcommand_sub_umbrella, mask)         => RETURN "sub_umbrella";
    | And(macho_loadcommand_sub_client, mask)           => RETURN "sub_client";
    | And(macho_loadcommand_sub_library, mask)          => RETURN "sub_library";
    | And(macho_loadcommand_twolevel_hints, mask)       => RETURN "twolevel_hints";
    | And(macho_loadcommand_prebind_checksum, mask)     => RETURN "prebind_checksum";
    | And(macho_loadcommand_load_weak_dylib, mask)      => RETURN "load_weak_dylib";
    | And(macho_loadcommand_segment64, mask)            => RETURN "segment64";
    | And(macho_loadcommand_routines64, mask)           => RETURN "routines64";
    | And(macho_loadcommand_uuid, mask)                 => RETURN "uuid";
    | And(macho_loadcommand_rpath, mask)                => RETURN "rpath";
    | And(macho_loadcommand_code_signature, mask)       => RETURN "code_signature";
    | And(macho_loadcommand_segment_split_info, mask)   => RETURN "segment_split_info";
    | And(macho_loadcommand_reexport_dylib, mask)       => RETURN "reexport_dylib";
    | And(macho_loadcommand_lazy_load_dylib, mask)      => RETURN "lazy_load_dylib";
    | And(macho_loadcommand_encryption_info, mask)      => RETURN "encryption_info";
    ELSE                                                RETURN "unknown (" & Fmt.Int(cmd) & ")";
  END;
END macho_loadcommand_name;

PROCEDURE macho_first_load_command(READONLY m:macho_file_t):UNTRACED REF macho_loadcommand_t =
BEGIN
  RETURN LOOPHOLE(m.macho_header + m.macho_header_size, UNTRACED REF macho_loadcommand_t);
END macho_first_load_command;

PROCEDURE macho_next_load_command(READONLY m:macho_file_t; L:UNTRACED REF macho_loadcommand_t):UNTRACED REF macho_loadcommand_t =
BEGIN
  RETURN LOOPHOLE(L + m.swap32(L.cmdsize), UNTRACED REF macho_loadcommand_t);
END macho_next_load_command;

PROCEDURE macho_ncmds(READONLY m:macho_file_t):uint =
BEGIN
  RETURN m.swap32(m.macho_header.ncmds);
END macho_ncmds;

CONST macho_segment32: UNTRACED REF macho_segment32_t = NIL;

VAR macho_segment32_t_fields := ARRAY [0..10] OF field_t{
  field_t{"cmd",       ADR(macho_segment32.cmd),      BYTESIZE(macho_segment32.cmd)},
  field_t{"cmdsize",   ADR(macho_segment32.cmdsize),  BYTESIZE(macho_segment32.cmdsize)},
  field_t{"segname",   ADR(macho_segment32.segname),  BYTESIZE(macho_segment32.segname), str := TRUE},
  field_t{"vmaddr",    ADR(macho_segment32.vmaddr),   BYTESIZE(macho_segment32.vmaddr)},
  field_t{"vmsize",    ADR(macho_segment32.vmsize),   BYTESIZE(macho_segment32.vmsize)},
  field_t{"fileoff",   ADR(macho_segment32.fileoff),  BYTESIZE(macho_segment32.fileoff)},
  field_t{"filesize",  ADR(macho_segment32.filesize), BYTESIZE(macho_segment32.filesize)},
  field_t{"maxprot",   ADR(macho_segment32.maxprot),  BYTESIZE(macho_segment32.maxprot)},
  field_t{"initprot",  ADR(macho_segment32.initprot), BYTESIZE(macho_segment32.initprot)},
  field_t{"nsects",    ADR(macho_segment32.nsects),   BYTESIZE(macho_segment32.nsects)},
  field_t{"flags",     ADR(macho_segment32.flags),    BYTESIZE(macho_segment32.flags)}};

VAR struct_macho_segment32 := struct_t{"macho_segment32_t",
                                       BYTESIZE(macho_segment32_t),
                                       CopyFieldArray(macho_segment32_t_fields)};

CONST macho_segment64: UNTRACED REF macho_segment64_t = NIL;

VAR macho_segment64_t_fields := ARRAY [0..10] OF field_t{
  field_t{"cmd",       ADR(macho_segment64.cmd),      BYTESIZE(macho_segment64.cmd)},
  field_t{"cmdsize",   ADR(macho_segment64.cmdsize),  BYTESIZE(macho_segment64.cmdsize)},
  field_t{"segname",   ADR(macho_segment64.segname),  BYTESIZE(macho_segment64.segname), str := TRUE},
  field_t{"vmaddr",    ADR(macho_segment64.vmaddr),   BYTESIZE(macho_segment64.vmaddr)},
  field_t{"vmsize",    ADR(macho_segment64.vmsize),   BYTESIZE(macho_segment64.vmsize)},
  field_t{"fileoff",   ADR(macho_segment64.fileoff),  BYTESIZE(macho_segment64.fileoff)},
  field_t{"filesize",  ADR(macho_segment64.filesize), BYTESIZE(macho_segment64.filesize)},
  field_t{"maxprot",   ADR(macho_segment64.maxprot),  BYTESIZE(macho_segment64.maxprot)},
  field_t{"initprot",  ADR(macho_segment64.initprot), BYTESIZE(macho_segment64.initprot)},
  field_t{"nsects",    ADR(macho_segment64.nsects),   BYTESIZE(macho_segment64.nsects)},
  field_t{"flags",     ADR(macho_segment64.flags),    BYTESIZE(macho_segment64.flags)}};

VAR struct_macho_segment64 := struct_t{"macho_segment64_t",
                                       BYTESIZE(macho_segment64_t),
                                       CopyFieldArray(macho_segment64_t_fields)};

CONST macho_section32: UNTRACED REF macho_section32_t = NIL;

VAR macho_section32_t_fields := ARRAY [0..10] OF field_t{
  field_t{"sectname",   ADR(macho_section32.sectname),  BYTESIZE(macho_section32.sectname), str := TRUE},
  field_t{"segname",    ADR(macho_section32.segname),   BYTESIZE(macho_section32.segname), str := TRUE},
  field_t{"addr",       ADR(macho_section32.addr),      BYTESIZE(macho_section32.addr)},
  field_t{"size",       ADR(macho_section32.size),      BYTESIZE(macho_section32.size)},
  field_t{"offset",     ADR(macho_section32.offset),    BYTESIZE(macho_section32.offset)},
  field_t{"align",      ADR(macho_section32.align),     BYTESIZE(macho_section32.align)},
  field_t{"reloff",     ADR(macho_section32.reloff),    BYTESIZE(macho_section32.reloff)},
  field_t{"nreloc",     ADR(macho_section32.nreloc),    BYTESIZE(macho_section32.nreloc)},
  field_t{"flags",      ADR(macho_section32.flags),     BYTESIZE(macho_section32.flags)},
  field_t{"reserved1",  ADR(macho_section32.reserved1), BYTESIZE(macho_section32.reserved1)},
  field_t{"reserved2",  ADR(macho_section32.reserved2), BYTESIZE(macho_section32.reserved2)}};

VAR struct_macho_section32 := struct_t{"macho_section32_t",
                                       BYTESIZE(macho_section32_t),
                                       CopyFieldArray(macho_section32_t_fields)};



CONST macho_section64: UNTRACED REF macho_section64_t = NIL;

VAR macho_section64_t_fields := ARRAY [0..11] OF field_t{
  field_t{"sectname",   ADR(macho_section64.sectname),  BYTESIZE(macho_section64.sectname), str := TRUE},
  field_t{"segname",    ADR(macho_section64.segname),   BYTESIZE(macho_section64.segname), str := TRUE},
  field_t{"addr",       ADR(macho_section64.addr),      BYTESIZE(macho_section64.addr)},
  field_t{"size",       ADR(macho_section64.size),      BYTESIZE(macho_section64.size)},
  field_t{"offset",     ADR(macho_section64.offset),    BYTESIZE(macho_section64.offset)},
  field_t{"align",      ADR(macho_section64.align),     BYTESIZE(macho_section64.align)},
  field_t{"reloff",     ADR(macho_section64.reloff),    BYTESIZE(macho_section64.reloff)},
  field_t{"nreloc",     ADR(macho_section64.nreloc),    BYTESIZE(macho_section64.nreloc)},
  field_t{"flags",      ADR(macho_section64.flags),     BYTESIZE(macho_section64.flags)},
  field_t{"reserved1",  ADR(macho_section64.reserved1), BYTESIZE(macho_section64.reserved1)},
  field_t{"reserved2",  ADR(macho_section64.reserved2), BYTESIZE(macho_section64.reserved2)},
  field_t{"reserved3",  ADR(macho_section64.reserved3), BYTESIZE(macho_section64.reserved3)}};

VAR struct_macho_section64 := struct_t{"macho_section64_t",
                                       BYTESIZE(macho_section64_t),
                                       CopyFieldArray(macho_section64_t_fields)};

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
