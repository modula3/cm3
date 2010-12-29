UNSAFE MODULE macho EXPORTS macho, Main;
IMPORT Word, Long, Fmt, Text, IO, M3toC, Cstdio, Ctypes, Scheduler, Cstdlib,
       Cstring, Process, Params;

TYPE enum_t = RECORD
  name: TEXT;
  value: uint32_t;
END;

TYPE field_t = RECORD
  name:         TEXT;
  offset:       ADDRESS;    (* small integer *)
  size:         CARDINAL;   (* small integer *)
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

PROCEDURE CopyEnumArray(READONLY a: ARRAY OF enum_t): REF ARRAY OF enum_t =
(* Modula-3 inefficiency.. *)
VAR b := NEW(REF ARRAY OF enum_t, NUMBER(a));
BEGIN
  b^ := a;
  RETURN b;
END CopyEnumArray;

PROCEDURE CopyFieldArray(READONLY a: ARRAY OF field_t): REF ARRAY OF field_t =
(* Modula-3 inefficiency.. *)
VAR b := NEW(REF ARRAY OF field_t, NUMBER(a));
BEGIN
  b^ := a;
  RETURN b;
END CopyFieldArray;

(* define STRUCT(a) {STRINGIZE(a), sizeof(a), NUMBER_OF(PASTE(a,_fields)), PASTE(a,_fields)} *)

CONST magic_names = ARRAY OF enum_t{
  enum_t{"magic32", magic32},
  enum_t{"magic64", magic64}};

CONST cputype_names = ARRAY OF enum_t{
  enum_t{"x86", cpu_type_x86},
  enum_t{"amd64", cpu_type_amd64},
  enum_t{"powerpc", cpu_type_powerpc},
  enum_t{"powerpc64", cpu_type_powerpc64}};

CONST cpusubtype_names = ARRAY OF enum_t{
  enum_t{"powerpc_all", cpu_subtype_powerpc_all},
  enum_t{"x86_all", cpu_subtype_x86_all}};

CONST filetype_names = ARRAY OF enum_t{
  enum_t{"object", type_object},
  enum_t{"execute", type_execute},
  enum_t{"fixed_vm_library", type_fixed_vm_library},
  enum_t{"core", type_core},
  enum_t{"preload", type_preload},
  enum_t{"dylib", type_dylib},
  enum_t{"dylinker", type_dylinker},
  enum_t{"bundle", type_bundle},
  enum_t{"dylib_stub", type_dylib_stub},
  enum_t{"dsym", type_dsym}};

CONST header32: UNTRACED REF header32_t = NIL;

VAR header32_fields := ARRAY [0..6] OF field_t{
  field_t{"magic",      ADR(header32.magic),      BYTESIZE(header32.magic), enum := CopyEnumArray(magic_names)},
  field_t{"cputype",    ADR(header32.cputype),    BYTESIZE(header32.cputype), enum := CopyEnumArray(cputype_names)},
  field_t{"cpusubtype", ADR(header32.cpusubtype), BYTESIZE(header32.cpusubtype), enum := CopyEnumArray(cpusubtype_names)},
  field_t{"filetype",   ADR(header32.filetype),   BYTESIZE(header32.filetype), enum := CopyEnumArray(filetype_names)},
  field_t{"ncmds",      ADR(header32.ncmds),      BYTESIZE(header32.ncmds)},
  field_t{"sizeofcmds", ADR(header32.sizeofcmds), BYTESIZE(header32.sizeofcmds)},
  field_t{"flags",      ADR(header32.flags),      BYTESIZE(header32.flags)}};

VAR struct_header32 := struct_t{"header32_t",
                                BYTESIZE(header32_t),
                                CopyFieldArray(header32_fields)};

TYPE file_t = RECORD
  path:                 TEXT;
  contents:             ADDRESS;
  size:                 CARDINAL;
  header:               UNTRACED REF header32_t;
  header_size:          CARDINAL; (* small integer *)
  swap32: PROCEDURE(a: uint32): uint32 := no_swap32;
  swap64: PROCEDURE(a: uint64): uint64 := no_swap64;
END;

PROCEDURE no_swap32(a: uint32): uint32 =
BEGIN
  RETURN a;
END no_swap32;

PROCEDURE no_swap64(a: uint64): uint64 =
BEGIN
  RETURN a;
END no_swap64;

PROCEDURE swap32(a: uint32): uint32 =
CONST Left = Word.LeftShift; Right = Word.RightShift; And = Word.And; Or = Word.Or;
BEGIN
  RETURN Or(    Right(a, 24),
         Or(And(Right(a,  8), 16_FF00),
         Or(And(Left (a,  8), 16_FF0000),
                Left (a, 24))));
END swap32;

PROCEDURE swap64(a: uint64): uint64 =
CONST FF: uint64 = 16_FFL;
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

PROCEDURE field_print(READONLY file: file_t;
                      VAR s: struct_t;
                      READONLY f: field_t;
                      record: ADDRESS) =
VAR buffer: TEXT;
    q := LOOPHOLE(record + LOOPHOLE(f.offset, CARDINAL), UNTRACED REF uchar);
    q32 := LOOPHOLE(q, UNTRACED REF uint32);
    q64 := LOOPHOLE(q, UNTRACED REF uint64);
    size := f.size;
    enum := f.enum;
    value32: uint32 := 0;
    value32_swapped: uint32 := 0;
    value64: uint64 := 0L;
    value64_swapped: uint64 := 0L;
    length: CARDINAL := 0;
BEGIN
  <* ASSERT size = 4 OR size = 8 OR f.str *>
  <* ASSERT size = 4 OR f.macho_string = FALSE *>
  buffer := s.name & "." & Fmt.Pad(f.name, s.widest_field, ' ');
  IF f.str THEN
    length := strnlen(q, size);
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
    ELSIF f.macho_string AND size = 4 THEN
      buffer := buffer & " (" & M3toC.StoT(record + file.swap32(value32)) & ")";
    END;
  END;
  buffer := buffer & "\n";
  IO.Put(buffer);
END field_print;

PROCEDURE struct_print(READONLY file: file_t;
                       VAR t: struct_t;
                       record: ADDRESS) =
VAR widest_field := t.widest_field;
BEGIN
  IF widest_field = 0 THEN
    FOR i := 0 TO NUMBER(t.fields^) - 1 DO
      widest_field := MAX(widest_field, Text.Length(t.fields[i].name));
    END;
    t.widest_field := widest_field;
  END;
  FOR i := 0 TO NUMBER(t.fields^) - 1 DO
    field_print(file, t, t.fields[i], record);
  END;
END struct_print;

PROCEDURE loadcommand_name(cmd: uint32_t): TEXT =
CONST mask = Word.Not(loadcommand_require_dyld);
      And = Word.And;
BEGIN
  cmd := And(cmd, mask);
  CASE cmd OF
    | And(loadcommand_segment32, mask)            => RETURN "segment32";
    | And(loadcommand_symtab, mask)               => RETURN "symtab";
    | And(loadcommand_symseg, mask)               => RETURN "symseg";
    | And(loadcommand_thread, mask)               => RETURN "thread";
    | And(loadcommand_unixthread, mask)           => RETURN "unixthread";
    | And(loadcommand_fixed_vm_lib, mask)         => RETURN "fixed_vm_lib";
    | And(loadcommand_id_fixed_vm_lib, mask)      => RETURN "id_fixed_vm_lib";
    | And(loadcommand_ident, mask)                => RETURN "ident";
    | And(loadcommand_fixed_vm_file, mask)        => RETURN "fixed_vm_file";
    | And(loadcommand_prepage, mask)              => RETURN "prepage";
    | And(loadcommand_dysymtab, mask)             => RETURN "dysymtab";
    | And(loadcommand_load_dylib, mask)           => RETURN "load_dylib";
    | And(loadcommand_id_dylib, mask)             => RETURN "id_dylib";
    | And(loadcommand_load_dylinker, mask)        => RETURN "load_dylinker";
    | And(loadcommand_id_dylinker, mask)          => RETURN "id_dylinker";
    | And(loadcommand_prebound_dylib, mask)       => RETURN "prebound_dylib";
    | And(loadcommand_routines32, mask)           => RETURN "routines32";
    | And(loadcommand_sub_framework, mask)        => RETURN "sub_framework";
    | And(loadcommand_sub_umbrella, mask)         => RETURN "sub_umbrella";
    | And(loadcommand_sub_client, mask)           => RETURN "sub_client";
    | And(loadcommand_sub_library, mask)          => RETURN "sub_library";
    | And(loadcommand_twolevel_hints, mask)       => RETURN "twolevel_hints";
    | And(loadcommand_prebind_checksum, mask)     => RETURN "prebind_checksum";
    | And(loadcommand_load_weak_dylib, mask)      => RETURN "load_weak_dylib";
    | And(loadcommand_segment64, mask)            => RETURN "segment64";
    | And(loadcommand_routines64, mask)           => RETURN "routines64";
    | And(loadcommand_uuid, mask)                 => RETURN "uuid";
    | And(loadcommand_rpath, mask)                => RETURN "rpath";
    | And(loadcommand_code_signature, mask)       => RETURN "code_signature";
    | And(loadcommand_segment_split_info, mask)   => RETURN "segment_split_info";
    | And(loadcommand_reexport_dylib, mask)       => RETURN "reexport_dylib";
    | And(loadcommand_lazy_load_dylib, mask)      => RETURN "lazy_load_dylib";
    | And(loadcommand_encryption_info, mask)      => RETURN "encryption_info";
    ELSE                                                RETURN "unknown (" & Fmt.Int(cmd) & ")";
  END;
END loadcommand_name;

PROCEDURE first_load_command(READONLY file: file_t): UNTRACED REF loadcommand_t =
BEGIN
  RETURN LOOPHOLE(file.header + file.header_size, UNTRACED REF loadcommand_t);
END first_load_command;

PROCEDURE next_load_command(READONLY file: file_t; L: UNTRACED REF loadcommand_t): UNTRACED REF loadcommand_t =
BEGIN
  RETURN LOOPHOLE(L + file.swap32(L.cmdsize), UNTRACED REF loadcommand_t);
END next_load_command;

PROCEDURE ncmds(READONLY file: file_t): CARDINAL =
BEGIN
  RETURN file.swap32(file.header.ncmds);
END ncmds;

(* segment32_t *)

CONST segment32: UNTRACED REF segment32_t = NIL;

VAR segment32_fields := ARRAY [0..10] OF field_t{
  field_t{"cmd",      ADR(segment32.cmd),      BYTESIZE(segment32.cmd)},
  field_t{"cmdsize",  ADR(segment32.cmdsize),  BYTESIZE(segment32.cmdsize)},
  field_t{"segname",  ADR(segment32.segname),  BYTESIZE(segment32.segname), str := TRUE},
  field_t{"vmaddr",   ADR(segment32.vmaddr),   BYTESIZE(segment32.vmaddr)},
  field_t{"vmsize",   ADR(segment32.vmsize),   BYTESIZE(segment32.vmsize)},
  field_t{"fileoff",  ADR(segment32.fileoff),  BYTESIZE(segment32.fileoff)},
  field_t{"filesize", ADR(segment32.filesize), BYTESIZE(segment32.filesize)},
  field_t{"maxprot",  ADR(segment32.maxprot),  BYTESIZE(segment32.maxprot)},
  field_t{"initprot", ADR(segment32.initprot), BYTESIZE(segment32.initprot)},
  field_t{"nsects",   ADR(segment32.nsects),   BYTESIZE(segment32.nsects)},
  field_t{"flags",    ADR(segment32.flags),    BYTESIZE(segment32.flags)}};

VAR struct_segment32 := struct_t{"segment32_t",
                                 BYTESIZE(segment32_t),
                                 CopyFieldArray(segment32_fields)};

(* segment64_t *)

CONST segment64: UNTRACED REF segment64_t = NIL;

VAR segment64_fields := ARRAY [0..10] OF field_t{
  field_t{"cmd",      ADR(segment64.cmd),      BYTESIZE(segment64.cmd)},
  field_t{"cmdsize",  ADR(segment64.cmdsize),  BYTESIZE(segment64.cmdsize)},
  field_t{"segname",  ADR(segment64.segname),  BYTESIZE(segment64.segname), str := TRUE},
  field_t{"vmaddr",   ADR(segment64.vmaddr),   BYTESIZE(segment64.vmaddr)},
  field_t{"vmsize",   ADR(segment64.vmsize),   BYTESIZE(segment64.vmsize)},
  field_t{"fileoff",  ADR(segment64.fileoff),  BYTESIZE(segment64.fileoff)},
  field_t{"filesize", ADR(segment64.filesize), BYTESIZE(segment64.filesize)},
  field_t{"maxprot",  ADR(segment64.maxprot),  BYTESIZE(segment64.maxprot)},
  field_t{"initprot", ADR(segment64.initprot), BYTESIZE(segment64.initprot)},
  field_t{"nsects",   ADR(segment64.nsects),   BYTESIZE(segment64.nsects)},
  field_t{"flags",    ADR(segment64.flags),    BYTESIZE(segment64.flags)}};

VAR struct_segment64 := struct_t{"segment64_t",
                                 BYTESIZE(segment64_t),
                                 CopyFieldArray(segment64_fields)};

(* section32_t *)

CONST section32: UNTRACED REF section32_t = NIL;

VAR section32_fields := ARRAY [0..10] OF field_t{
  field_t{"sectname",  ADR(section32.sectname),  BYTESIZE(section32.sectname), str := TRUE},
  field_t{"segname",   ADR(section32.segname),   BYTESIZE(section32.segname), str := TRUE},
  field_t{"addr",      ADR(section32.addr),      BYTESIZE(section32.addr)},
  field_t{"size",      ADR(section32.size),      BYTESIZE(section32.size)},
  field_t{"offset",    ADR(section32.offset),    BYTESIZE(section32.offset)},
  field_t{"align",     ADR(section32.align),     BYTESIZE(section32.align)},
  field_t{"reloff",    ADR(section32.reloff),    BYTESIZE(section32.reloff)},
  field_t{"nreloc",    ADR(section32.nreloc),    BYTESIZE(section32.nreloc)},
  field_t{"flags",     ADR(section32.flags),     BYTESIZE(section32.flags)},
  field_t{"reserved1", ADR(section32.reserved1), BYTESIZE(section32.reserved1)},
  field_t{"reserved2", ADR(section32.reserved2), BYTESIZE(section32.reserved2)}};

VAR struct_section32 := struct_t{"section32_t",
                                 BYTESIZE(section32_t),
                                 CopyFieldArray(section32_fields)};

CONST section64: UNTRACED REF section64_t = NIL;

VAR section64_fields := ARRAY [0..11] OF field_t{
  field_t{"sectname",  ADR(section64.sectname),  BYTESIZE(section64.sectname), str := TRUE},
  field_t{"segname",   ADR(section64.segname),   BYTESIZE(section64.segname), str := TRUE},
  field_t{"addr",      ADR(section64.addr),      BYTESIZE(section64.addr)},
  field_t{"size",      ADR(section64.size),      BYTESIZE(section64.size)},
  field_t{"offset",    ADR(section64.offset),    BYTESIZE(section64.offset)},
  field_t{"align",     ADR(section64.align),     BYTESIZE(section64.align)},
  field_t{"reloff",    ADR(section64.reloff),    BYTESIZE(section64.reloff)},
  field_t{"nreloc",    ADR(section64.nreloc),    BYTESIZE(section64.nreloc)},
  field_t{"flags",     ADR(section64.flags),     BYTESIZE(section64.flags)},
  field_t{"reserved1", ADR(section64.reserved1), BYTESIZE(section64.reserved1)},
  field_t{"reserved2", ADR(section64.reserved2), BYTESIZE(section64.reserved2)},
  field_t{"reserved3", ADR(section64.reserved3), BYTESIZE(section64.reserved3)}};

VAR struct_section64 := struct_t{"section64_t",
                                 BYTESIZE(section64_t),
                                 CopyFieldArray(section64_fields)};

PROCEDURE dump_load_command_segmentX(READONLY file: file_t;
                                     L: ADDRESS;
                                     load_command_size: uint32;
                                     VAR struct_segmentX: struct_t;
                                     VAR struct_sectionX: struct_t;
                                     section_count: uint32;
                                     section_size: uint32) =
VAR sections := L + load_command_size;
BEGIN
  struct_print(file, struct_segmentX, L);
  FOR i := 0 TO section_count - 1 DO
    struct_print(file, struct_sectionX, sections);
    INC(sections, section_size);
  END;
END dump_load_command_segmentX;

PROCEDURE dump_load_command_segment32(READONLY file: file_t;
                                      L: UNTRACED REF segment32_t) =
BEGIN
    dump_load_command_segmentX(file,
                               L,
                               BYTESIZE(L^),
                               struct_segment32,
                               struct_section32,
                               file.swap32(L.nsects),
                               BYTESIZE(section32_t));
END dump_load_command_segment32;

PROCEDURE dump_load_command_segment64(READONLY file: file_t;
                                      L: UNTRACED REF segment64_t) =
BEGIN
    dump_load_command_segmentX(file,
                               L,
                               BYTESIZE(L^),
                               struct_segment64,
                               struct_section64,
                               file.swap32(L.nsects),
                               BYTESIZE(section64_t));
END dump_load_command_segment64;

CONST symtab_command: UNTRACED REF symtab_command_t = NIL;

VAR symtab_command_fields := ARRAY [0..3] OF field_t{
  field_t{"symoff",  ADR(symtab_command.symoff),  BYTESIZE(symtab_command.symoff)},
  field_t{"nsyms",   ADR(symtab_command.nsyms),   BYTESIZE(symtab_command.nsyms)},
  field_t{"stroff",  ADR(symtab_command.stroff),  BYTESIZE(symtab_command.stroff)},
  field_t{"strsize", ADR(symtab_command.strsize), BYTESIZE(symtab_command.strsize)}};

VAR struct_symtab_command := struct_t{"symtab_command_t",
                                      BYTESIZE(symtab_command_t),
                                      CopyFieldArray(symtab_command_fields)};

PROCEDURE dump_load_command_symtab(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
  struct_print(file, struct_symtab_command, ADR(L));
END dump_load_command_symtab;

<*NOWARN*>PROCEDURE dump_load_command_symseg(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_symseg;

<*NOWARN*>PROCEDURE dump_load_command_thread(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_thread;

<*NOWARN*>PROCEDURE dump_load_command_unixthread(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_unixthread;

<*NOWARN*>PROCEDURE dump_load_command_fixed_vm_lib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_fixed_vm_lib;

<*NOWARN*>PROCEDURE dump_load_command_id_fixed_vm_lib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_id_fixed_vm_lib;

<*NOWARN*>PROCEDURE dump_load_command_ident(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_ident;

<*NOWARN*>PROCEDURE dump_load_command_fixed_vm_file(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_fixed_vm_file;

<*NOWARN*>PROCEDURE dump_load_command_prepage(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_prepage;

CONST dysymtab_command: UNTRACED REF dysymtab_command_t = NIL;

VAR dysymtab_command_fields := ARRAY [0..16] OF field_t{
  field_t{"ilocalsym",      ADR(dysymtab_command.ilocalsym),      BYTESIZE(dysymtab_command.ilocalsym)},
  field_t{"nlocalsym",      ADR(dysymtab_command.nlocalsym),      BYTESIZE(dysymtab_command.nlocalsym)},
  field_t{"iextdefsym",     ADR(dysymtab_command.iextdefsym),     BYTESIZE(dysymtab_command.iextdefsym)},
  field_t{"nextdefsym",     ADR(dysymtab_command.nextdefsym),     BYTESIZE(dysymtab_command.nextdefsym)},
  field_t{"iundefsym",      ADR(dysymtab_command.iundefsym),      BYTESIZE(dysymtab_command.iundefsym)},
  field_t{"nundefsym",      ADR(dysymtab_command.nundefsym),      BYTESIZE(dysymtab_command.nundefsym)},
  field_t{"tocoff",         ADR(dysymtab_command.tocoff),         BYTESIZE(dysymtab_command.tocoff)},
  field_t{"ntoc",           ADR(dysymtab_command.ntoc),           BYTESIZE(dysymtab_command.ntoc)},
  field_t{"modtaboff",      ADR(dysymtab_command.modtaboff),      BYTESIZE(dysymtab_command.modtaboff)},
  field_t{"nmodtab",        ADR(dysymtab_command.nmodtab),        BYTESIZE(dysymtab_command.nmodtab)},
  field_t{"extrefsymoff",   ADR(dysymtab_command.extrefsymoff),   BYTESIZE(dysymtab_command.extrefsymoff)},
  field_t{"nextrefsyms",    ADR(dysymtab_command.nextrefsyms),    BYTESIZE(dysymtab_command.nextrefsyms)},
  field_t{"indirectsymoff", ADR(dysymtab_command.indirectsymoff), BYTESIZE(dysymtab_command.indirectsymoff)},
  field_t{"nindirectsyms",  ADR(dysymtab_command.nindirectsyms),  BYTESIZE(dysymtab_command.nindirectsyms)},
  field_t{"extreloff",      ADR(dysymtab_command.extreloff),      BYTESIZE(dysymtab_command.extreloff)},
  field_t{"locreloff",      ADR(dysymtab_command.locreloff),      BYTESIZE(dysymtab_command.locreloff)},
  field_t{"nlocrel",        ADR(dysymtab_command.nlocrel),        BYTESIZE(dysymtab_command.nlocrel)}};

VAR struct_dysymtab_command := struct_t{"dysymtab_command_t",
                                        BYTESIZE(dysymtab_command_t),
                                        CopyFieldArray(dysymtab_command_fields)};

PROCEDURE dump_load_command_dysymtab(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
    struct_print(file, struct_dysymtab_command, ADR(L));
END dump_load_command_dysymtab;

<*NOWARN*>PROCEDURE dump_load_command_load_dylib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_load_dylib;

<*NOWARN*>PROCEDURE dump_load_command_id_dylib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_id_dylib;

CONST dylinker_command: UNTRACED REF dylinker_command_t = NIL;

VAR dylinker_command_fields := ARRAY [0..0] OF field_t{
  field_t{"name.offset", ADR(dylinker_command.name.offset), BYTESIZE(dylinker_command.name.offset), macho_string := TRUE}};

VAR struct_dylinker_command := struct_t{"dylinker_command_t",
                                        BYTESIZE(dylinker_command_t),
                                        CopyFieldArray(dylinker_command_fields)};

PROCEDURE dump_load_command_load_dylinker(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
    struct_print(file, struct_dylinker_command, ADR(L));
END dump_load_command_load_dylinker;

<*NOWARN*>PROCEDURE dump_load_command_id_dylinker(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_id_dylinker;

<*NOWARN*>PROCEDURE dump_load_command_prebound_dylib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_prebound_dylib;

<*NOWARN*>PROCEDURE dump_load_command_routines32(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_routines32;

<*NOWARN*>PROCEDURE dump_load_command_sub_framework(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_sub_framework;

<*NOWARN*>PROCEDURE dump_load_command_sub_umbrella(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_sub_umbrella;

<*NOWARN*>PROCEDURE dump_load_command_sub_client(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_sub_client;

<*NOWARN*>PROCEDURE dump_load_command_sub_library(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_sub_library;

<*NOWARN*>PROCEDURE dump_load_command_twolevel_hints(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_twolevel_hints;

<*NOWARN*>PROCEDURE dump_load_command_prebind_checksum(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_prebind_checksum;

<*NOWARN*>PROCEDURE dump_load_command_load_weak_dylib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_load_weak_dylib;

<*NOWARN*>PROCEDURE dump_load_command_routines64(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_routines64;

<*NOWARN*>PROCEDURE dump_load_command_uuid(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_uuid;

<*NOWARN*>PROCEDURE dump_load_command_rpath(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_rpath;

<*NOWARN*>PROCEDURE dump_load_command_code_signature(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_code_signature;

<*NOWARN*>PROCEDURE dump_load_command_segment_split_info(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_segment_split_info;

<*NOWARN*>PROCEDURE dump_load_command_reexport_dylib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_reexport_dylib;

<*NOWARN*>PROCEDURE dump_load_command_lazy_load_dylib(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_lazy_load_dylib;

<*NOWARN*>PROCEDURE dump_load_command_encryption_info(READONLY file: file_t; L: UNTRACED REF loadcommand_t) =
BEGIN
END dump_load_command_encryption_info;

PROCEDURE dump_load_command(READONLY file: file_t; L: UNTRACED REF loadcommand_t; i: CARDINAL) =
CONST mask = Word.Not(loadcommand_require_dyld);
      And = Word.And;
VAR cmd := file.swap32(L.cmd);
    a := LOOPHOLE(L, ADDRESS);
BEGIN
  IO.Put("cmd " & Fmt.Int(i) & " " & loadcommand_name(cmd) & "\n");
  CASE And(cmd, mask) OF
    | And(loadcommand_segment32, mask)          => dump_load_command_segment32(file, a);
    | And(loadcommand_symtab, mask)             => dump_load_command_symtab(file, a);
    | And(loadcommand_symseg, mask)             => dump_load_command_symseg(file, a);
    | And(loadcommand_thread, mask)             => dump_load_command_thread(file, a);
    | And(loadcommand_unixthread, mask)         => dump_load_command_unixthread(file, a);
    | And(loadcommand_fixed_vm_lib, mask)       => dump_load_command_fixed_vm_lib(file, a);
    | And(loadcommand_id_fixed_vm_lib, mask)    => dump_load_command_id_fixed_vm_lib(file, a);
    | And(loadcommand_ident, mask)              => dump_load_command_ident(file, a);
    | And(loadcommand_fixed_vm_file, mask)      => dump_load_command_fixed_vm_file(file, a);
    | And(loadcommand_prepage, mask)            => dump_load_command_prepage(file, a);
    | And(loadcommand_dysymtab, mask)           => dump_load_command_dysymtab(file, a);
    | And(loadcommand_load_dylib, mask)         => dump_load_command_load_dylib(file, a);
    | And(loadcommand_id_dylib, mask)           => dump_load_command_id_dylib(file, a);
    | And(loadcommand_load_dylinker, mask)      => dump_load_command_load_dylinker(file, a);
    | And(loadcommand_id_dylinker, mask)        => dump_load_command_id_dylinker(file, a);
    | And(loadcommand_prebound_dylib, mask)     => dump_load_command_prebound_dylib(file, a);
    | And(loadcommand_routines32, mask)         => dump_load_command_routines32(file, a);
    | And(loadcommand_sub_framework, mask)      => dump_load_command_sub_framework(file, a);
    | And(loadcommand_sub_umbrella, mask)       => dump_load_command_sub_umbrella(file, a);
    | And(loadcommand_sub_client, mask)         => dump_load_command_sub_client(file, a);
    | And(loadcommand_sub_library, mask)        => dump_load_command_sub_library(file, a);
    | And(loadcommand_twolevel_hints, mask)     => dump_load_command_twolevel_hints(file, a);
    | And(loadcommand_prebind_checksum, mask)   => dump_load_command_prebind_checksum(file, a);
    | And(loadcommand_load_weak_dylib, mask)    => dump_load_command_load_weak_dylib(file, a);
    | And(loadcommand_segment64, mask)          => dump_load_command_segment64(file, a);
    | And(loadcommand_routines64, mask)         => dump_load_command_routines64(file, a);
    | And(loadcommand_uuid, mask)               => dump_load_command_uuid(file, a);
    | And(loadcommand_rpath, mask)              => dump_load_command_rpath(file, a);
    | And(loadcommand_code_signature, mask)     => dump_load_command_code_signature(file, a);
    | And(loadcommand_segment_split_info, mask) => dump_load_command_segment_split_info(file, a);
    | And(loadcommand_reexport_dylib, mask)     => dump_load_command_reexport_dylib(file, a);
    | And(loadcommand_lazy_load_dylib, mask)    => dump_load_command_lazy_load_dylib(file, a);
    | And(loadcommand_encryption_info, mask)    => dump_load_command_encryption_info(file, a);
    ELSE IO.Put("unknown load command\n");
  END;
END dump_load_command;

PROCEDURE dump_load_commands(READONLY file: file_t) =
VAR n := ncmds(file);
    L := first_load_command(file);
BEGIN
  FOR i := 0 TO n - 1 DO
    dump_load_command(file, L, i);
    L := next_load_command(file, L);
  END;
END dump_load_commands;

PROCEDURE open_and_read_entire_file(path: TEXT; VAR contents: ADDRESS; VAR size: CARDINAL) =
VAR buffer: ADDRESS := NIL;
    prev_buffer: ADDRESS := NIL;
    buffer_size: CARDINAL := 16_10000 DIV 2;
    prev_buffer_size: CARDINAL := 0;
    file: Cstdio.FILE_star := NIL;
    bytes_read: CARDINAL := 0;
    cpath: Ctypes.char_star := NIL;
BEGIN
  LOOP
    TRY
      Scheduler.DisableSwitching();
      cpath := M3toC.SharedTtoS(path);
      size := 0;
      contents := NIL;
      file := Cstdio.fopen(cpath, M3toC.FlatTtoS("rb"));
    FINALLY
      M3toC.FreeSharedS(path, cpath);
      Scheduler.EnableSwitching();
    END;
    IF file = NIL THEN EXIT END;
    WHILE TRUE DO
      INC(buffer_size, buffer_size);
      prev_buffer := buffer;
      buffer := Cstdlib.malloc(buffer_size);
      IF buffer = NIL THEN
        EXIT
      END;
      IF prev_buffer # NIL THEN
        EVAL Cstring.memcpy(buffer, prev_buffer, prev_buffer_size);
        Cstdlib.free(prev_buffer);
        prev_buffer := NIL;
      END;
      bytes_read := Cstdio.fread(buffer + prev_buffer_size, 1, buffer_size - prev_buffer_size, file);
      IF bytes_read < (buffer_size - prev_buffer_size) THEN
        EXIT;
      END;
      prev_buffer_size := buffer_size;
    END;
    EXIT;
  END;
  
  Cstdlib.free(prev_buffer);
  size := prev_buffer_size + bytes_read;
  contents := buffer;
  IF file # NIL THEN
    EVAL Cstdio.fclose(file);
  END;
END open_and_read_entire_file;

PROCEDURE main() =
VAR file: file_t;
    magic: uint32_t := 0;
    swapped: BOOLEAN := FALSE;
    m64: BOOLEAN := FALSE;
BEGIN
  file.path := Params.Get(0);
  open_and_read_entire_file(file.path, file.contents, file.size);
  IF file.contents = NIL THEN
    Process.Exit(1);
  END;
  file.header := LOOPHOLE(file.contents, UNTRACED REF header32_t);
  magic := file.header.magic;
  IF magic # magic32 AND magic # magic32_reversed
       AND magic # magic64 AND magic # magic64_reversed THEN
    Process.Exit(1);
  END;
  m64 := ((magic = magic64) OR (magic = magic64_reversed));
  swapped := ((magic = magic32_reversed) OR (magic = magic64_reversed));
  file.swap32 := no_swap32;
  file.swap64 := no_swap64;
  IF swapped THEN
    file.swap32 := swap32;
    file.swap64 := swap64;
  END;
  file.header_size := (ORD(m64) * BYTESIZE(header64_t)) + (ORD(NOT m64) * BYTESIZE(header32_t));
  struct_print(file, struct_header32, file.contents);
  dump_load_commands(file);
END main;

BEGIN
  main();
END macho.
