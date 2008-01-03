(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Jan 30 08:33:00 PST 1995 by kalsow   *)

MODULE NTObjFile;

IMPORT Text, Wr, Word, IntIntTbl, TextIntTbl;
IMPORT M3ObjFile, M3ID, CoffTime, Target;
FROM M3CG IMPORT Name, BitOffset, BitSize, ByteOffset, ByteSize, TypeUID;
IMPORT RTError;

TYPE
  Seg = M3ObjFile.Seg;

TYPE
  SymKind = { Text, Data, Bss, Extern, File, FileAux, Section };

CONST
  SegToKind = ARRAY Seg OF SymKind { SymKind.Text, SymKind.Data };

TYPE
  Alignment = [0..3];
CONST
  AlignBytes = ARRAY Alignment OF [0..8] { 1, 2, 4, 8 };

TYPE
  SymbolList = REF ARRAY OF Symbol;
  Symbol = RECORD
    id          : M3ID.T;
    offset      : INTEGER; (* segment offset *)
    kind        : SymKind;
    export      : BOOLEAN;
    used        : BOOLEAN;
    index       : INTEGER; (* final symbol table index *)
    next_func   : INTEGER; (* symtab index of next procedure entry *)
    first_line  : INTEGER; (* source line number *)
    last_line   : INTEGER;
    last_offset : INTEGER;
    lineno_offs : INTEGER;
    lineno_cnt  : INTEGER;
  END;

TYPE
  RelocKind = {Symbol, Offset, Segment};

TYPE
  RelocList = REF ARRAY OF Reloc;
  Reloc = RECORD
    kind                : RelocKind;
    src_sym, src_offset : INTEGER;
    target_sym          : INTEGER;
  END;

CONST (* relocation types *)
  REL_I386_DIR32   =  6; (* Direct 32-bit reference to symbol *)
  REL_I386_SECTION = 10; (* 16-bit reference to symbol's section number *)
  REL_I386_SECREL  = 11; (* 32-bit reference to symbol's section offset *)

TYPE
  LineNums = REF ARRAY OF LineNum;
  LineNum  = RECORD addr, line: INTEGER;  END;

TYPE
  Bytes    = REF ARRAY OF CHAR;
  Ints     = REF ARRAY OF INTEGER;
  TextList = REF ARRAY OF TEXT;

TYPE
  Chunk = RECORD
    cnt         : INTEGER := 0;
    n_bytes     : INTEGER := 0;
    entry_size  : INTEGER := 0;
    file_offset : INTEGER := 0;
  END;

TYPE
  Section = RECORD
    name          : TEXT    := NIL;
    id            : INTEGER := 0;
    flags         : INTEGER := 0;
    address       : INTEGER := 0;
    raw_data      : Chunk;
    relocation    : Chunk;
    line_numbers  : Chunk;
    data          : Bytes     := NIL;
    line_nums     : LineNums  := NIL;
    relocs        : RelocList := NIL;
  END;

TYPE
  SymbolTable = RECORD
    map  : IntIntTbl.T := NIL;
    list : SymbolList  := NIL;
    remap: Ints        := NIL;
    chunk: Chunk;
  END;

TYPE
  OutputStream = RECORD
    wr    : Wr.T := NIL;
    len   : INTEGER := 0;
    buf   : ARRAY [0..2047] OF CHAR;
  END;

TYPE
  StringTable = RECORD
    cnt     : INTEGER      := 0;
    n_bytes : INTEGER      := 0;
    list    : TextList     := NIL;
    map     : TextIntTbl.T := NIL;
  END;

REVEAL
  T = M3ObjFile.T BRANDED "NTObjFile.T" OBJECT
    n_sections       : INTEGER;
    drectve          : Section;
    debug_S          : Section;
    text             : Section;
    data             : Section;
    bss              : Section;
    debug_T          : Section;
    symtab           : SymbolTable;
    strings          : StringTable;
    out              : OutputStream;
    filename         : TEXT;
    filesym          : INTEGER;
    gen_debugging    : BOOLEAN;
    sect_syms        : BOOLEAN;
    in_procedure     : BOOLEAN;
    first_proc       : INTEGER;
    last_proc        : INTEGER;
    last_source_line : INTEGER;
  OVERRIDES
    cursor            := Cursor;
    append            := Append;
    patch             := Patch;
    relocate          := Relocate;
    import_symbol     := ImportSymbol;
    define_symbol     := DefineSymbol;
    define_bss_symbol := DefineBssSymbol;
    move_symbol       := MoveSymbol;
    export_symbol     := ExportSymbol;
    set_source_file   := SetSourceFile;
    set_source_line   := SetSourceLine;

    declare_typename    := DeclareTypename;
    declare_array       := DeclareArray;
    declare_open_array  := DeclareOpenArray;
    declare_enum        := DeclareEnum;
    declare_enum_elt    := DeclareEnumElt;
    declare_packed      := DeclarePacked ;
    declare_record      := DeclareRecord;
    declare_field       := DeclareField;
    declare_set         := DeclareSet;
    declare_subrange    := DeclareSubrange;
    declare_pointer     := DeclarePointer;
    declare_indirect    := DeclareIndirect;
    declare_proctype    := DeclareProctype;
    declare_formal      := DeclareFormal;
    declare_raises      := DeclareRaises;
    declare_object      := DeclareObject;
    declare_method      := DeclareMethod;
    declare_opaque      := DeclareOpaque;
    reveal_opaque       := RevealOpaque;

    declare_exception   := DeclareException;
    declare_global      := DeclareGlobal;
    declare_constant    := DeclareConstant;

    declare_local       := DeclareLocal;
    declare_param       := DeclareParam;

    declare_procedure   := DeclareProcedure;
    begin_procedure     := BeginProcedure;
    end_procedure       := EndProcedure;

    begin_block         := BeginBlock;
    end_block           := EndBlock;

    note_procedure_origin := NoteProcedureOrigin;
  END;

CONST (* byte sizes of the structures in the object file *)
  HeaderSize  = 20;
  SectionSize = 40;
  RelocSize   = 10;
  LineNumSize = 6;
  SymTabSize  = 18;

CONST (* section flags *)
  S_NO_PAD  = 16_00000008;
  S_CODE    = 16_00000020;
  S_DATA    = 16_00000040;
  S_BSS     = 16_00000080;
  S_INFO    = 16_00000200;
  S_REMOVE  = 16_00000800;
  S_ALIGN1  = 16_00100000;
(*S_ALIGN2  = 16_00200000; *)
(*S_ALIGN4  = 16_00300000; *)
  S_ALIGN8  = 16_00400000;
  S_ALIGN16 = 16_00500000;
(*S_ALIGN32 = 16_00600000; *)
(*S_ALIGN64 = 16_00700000; *)
  S_DISCARD = 16_02000000;
  S_EXEC    = 16_20000000;
  S_READ    = 16_40000000;
  S_WRITE   = 16_80000000;

CONST (* CV4 symbol types *)
  S_COMPILE = 1;
  S_END     = 6;
  S_OBJNAME = 9;
  S_LPROC32 = 16_204;
  S_GPROC32 = 16_205;

CONST
  CompilerName = "Digital SRC Modula-3 (version 3.4)";

CONST (* constants defined by the code generator *)
  PrologueLength = 4;
  EpilogueLength = 4;

CONST
  ProcTag = ARRAY BOOLEAN OF INTEGER { S_LPROC32, S_GPROC32 };

(*-------------------------------------------------------- initialization ---*)

PROCEDURE New (): T =
  CONST DebugFlags = S_READ + S_DISCARD + S_ALIGN1 + S_DATA + S_NO_PAD;
  VAR t := NEW (T);
  BEGIN
    t.gen_debugging := FALSE;
    t.n_sections    := 0;

    IF (t.gen_debugging) THEN
      InitSection (t, t.drectve, ".drectve", S_ALIGN1 + S_REMOVE + S_INFO);
      InitSection (t, t.debug_S, ".debug$S", DebugFlags);
    END;
    InitSection (t, t.text, ".text", S_READ + S_EXEC + S_ALIGN16 + S_CODE);
    InitSection (t, t.data, ".data", S_WRITE + S_READ + S_ALIGN8 + S_DATA);
    InitSection (t, t.bss,  ".bss",  S_WRITE + S_READ + S_ALIGN8 + S_BSS);
    IF (t.gen_debugging) THEN
      InitSection (t, t.debug_T, ".debug$T", DebugFlags);
    END;

    WITH s = t.symtab DO
      s.map  := NIL;
      (* preserve the space in s.list *)
      InitChunk (s.chunk, SymTabSize);
    END;

    WITH s = t.strings DO
      s.cnt     := 0;
      s.n_bytes := 0;
      s.list    := NIL;
      s.map     := NIL;
    END;

    WITH o = t.out DO
      o.wr  := NIL;
      o.len := 0;
    END;

    t.filename         := NIL;
    t.filesym          := -1;
    t.sect_syms        := FALSE;
    t.in_procedure     := FALSE;
    t.first_proc       := 0;
    t.last_proc        := 0;
    t.last_source_line := 0;

    AddSectSym (t, t.drectve);
    AddSectSym (t, t.debug_S);
    AddSectSym (t, t.text);
    AddSectSym (t, t.data);
    AddSectSym (t, t.bss);
    AddSectSym (t, t.debug_T);

    IF t.gen_debugging THEN
      AddRaw (t.debug_T, 1, 4); (* CV4 version stamp *)
      AddRaw (t.debug_S, 1, 4); (* CV4 version stamp *)

      AddRaw (t.drectve, 16_6665642d, 4); (* "-defaultlib:LIBC" *)
      AddRaw (t.drectve, 16_746c7561, 4);
      AddRaw (t.drectve, 16_3a62696c, 4);
      AddRaw (t.drectve, 16_4342494c, 4);
      AddRaw (t.drectve, 16_20,       1);
    END;

    RETURN t;
  END New;

PROCEDURE InitSection (t: T;  VAR s: Section;  name: TEXT;  flags: INTEGER) =
  BEGIN
    INC (t.n_sections);
    s.id      := t.n_sections;
    s.name    := name;
    s.flags   := flags;
    s.address := 0;
    InitChunk (s.raw_data,     1);
    InitChunk (s.relocation,   RelocSize);
    InitChunk (s.line_numbers, LineNumSize);
    (* preserve the space allocated for data, line_nums & relocs *)
  END InitSection;

PROCEDURE InitChunk (VAR c: Chunk;  sz: INTEGER) =
  BEGIN
    c.cnt         := 0;
    c.n_bytes     := 0;
    c.entry_size  := sz;
    c.file_offset := 0;
  END InitChunk;

PROCEDURE AddSectSym (t: T;  READONLY s: Section) =
  VAR z: INTEGER;
  BEGIN
    IF (s.id <= 0) THEN RETURN; END;
    z := NewSym (t, M3ID.Add (s.name));
    WITH sym = t.symtab.list[z] DO
      sym.kind        := SymKind.Section;
      sym.offset      := s.id;
      sym.used        := TRUE;
      sym.index       := -1;
      sym.next_func   := 0;
      sym.first_line  := 0;
      sym.last_line   := 0;
      sym.last_offset := 0;
      sym.lineno_offs := 0;
      sym.lineno_cnt  := 0;
    END;
  END AddSectSym;

(*---------------------------------------------------------- construction ---*)

PROCEDURE Cursor (t: T;  s: Seg): INTEGER =
  BEGIN
    IF (s = Seg.Text)
      THEN RETURN t.text.raw_data.n_bytes;
      ELSE RETURN t.data.raw_data.n_bytes;
    END;
  END Cursor;

PROCEDURE Append (t: T;  s: Seg;  value, length: INTEGER) =
  BEGIN
    IF (s = Seg.Text)
      THEN AddRaw (t.text, value, length);
      ELSE AddRaw (t.data, value, length);
    END;
  END Append;

PROCEDURE AddRaw (VAR s: Section;  value, length: INTEGER) =
  VAR
    offs := s.raw_data.n_bytes;
    seg  := EnsureLength (s.data, offs + length);
  BEGIN
    WHILE (length > 0) DO
      seg[offs] := VAL (Word.And (value, 16_ff), CHAR);
      value := Word.RightShift (value, 8);
      INC (offs);
      DEC (length);
    END;
    s.raw_data.n_bytes := offs;
    s.raw_data.cnt     := offs;
  END AddRaw;

PROCEDURE AddName (VAR s: Section;  name: TEXT) =
  VAR
    len  := MIN (Text.Length (name), 255);
    offs := s.raw_data.n_bytes;
    seg  := EnsureLength (s.data, offs + len + 1);
  BEGIN
    (* add the length prefix byte *)
    seg[offs] := VAL (Word.And (len, 16_ff), CHAR);
    INC (offs);

    (* add the bytes *)
    FOR i := 0 TO len-1 DO
      seg[offs] := Text.GetChar (name, i);
      INC (offs);
    END;

    s.raw_data.n_bytes := offs;
    s.raw_data.cnt     := offs;
  END AddName;

PROCEDURE EnsureLength (VAR b: Bytes;  length: INTEGER): Bytes =
  VAR n, m: INTEGER;
  BEGIN
    IF (b = NIL) THEN  b := NEW (Bytes, 1024);  END;
    n := NUMBER (b^);
    IF (n < length) THEN
      m := n;
      WHILE (m < length) DO INC (m, m); END;
      VAR new := NEW (Bytes, m); BEGIN
        SUBARRAY (new^, 0, n) := b^;
        b := new;
      END;
    END;
    RETURN b;
  END EnsureLength;

PROCEDURE Patch (t: T;  s: Seg;  offset, value, length: INTEGER) =
  BEGIN
    IF (s = Seg.Text)
      THEN PatchRaw (t.text, offset, value, length);
      ELSE PatchRaw (t.data, offset, value, length);
    END;
  END Patch;

PROCEDURE PatchRaw (VAR s: Section;  offset, value, length: INTEGER) =
  BEGIN
    <*ASSERT s.raw_data.n_bytes >= offset + length *>
    WHILE (length > 0) DO
      s.data[offset] := VAL (Word.And (value, 16_ff), CHAR);
      value := Word.RightShift (value, 8);
      INC (offset);
      DEC (length);
    END;
  END PatchRaw;

PROCEDURE Relocate (t: T;  src_sym, src_offs, tar_sym: INTEGER) =
  BEGIN
    t.symtab.list[src_sym].used := TRUE;
    t.symtab.list[tar_sym].used := TRUE;
    CASE t.symtab.list[src_sym].kind OF <*NOWARN*>
    | SymKind.Text =>
        AddReloc (t.text, RelocKind.Symbol, src_sym, src_offs, tar_sym);
    | SymKind.Data =>
        AddReloc (t.data, RelocKind.Symbol, src_sym, src_offs, tar_sym);
    | SymKind.Bss  =>
        AddReloc (t.bss,  RelocKind.Symbol, src_sym, src_offs, tar_sym);
    END;
  END Relocate;

PROCEDURE AddReloc (VAR s: Section;  kind: RelocKind;
                    src_sym, src_offs, tar_sym: INTEGER) =
  BEGIN
    IF (s.relocs = NIL) OR (s.relocation.cnt >= NUMBER (s.relocs^)) THEN
      ExpandRelocs (s);
    END;
    WITH r = s.relocs [s.relocation.cnt] DO
      r.kind          := kind;
      r.src_sym       := src_sym;
      r.src_offset    := src_offs;
      r.target_sym    := tar_sym;
    END;
    INC (s.relocation.cnt);
    INC (s.relocation.n_bytes, RelocSize);
  END AddReloc;

PROCEDURE ExpandRelocs (VAR s: Section) =
  VAR n: INTEGER;  new: RelocList;
  BEGIN
    IF (s.relocs = NIL) THEN
      s.relocs := NEW (RelocList, 100);
    ELSE
      n := NUMBER (s.relocs^);
      new := NEW (RelocList, n + n);
      SUBARRAY (new^, 0, n) := s.relocs^;
      s.relocs := new;
    END;
  END ExpandRelocs;

PROCEDURE ImportSymbol (t: T;  id: M3ID.T): INTEGER =
  VAR z := NewSym (t, id);
  BEGIN
    WITH sym = t.symtab.list[z] DO
      sym.kind   := SymKind.Extern;
      sym.export := FALSE;
      sym.offset := z;
    END;
    RETURN z;
  END ImportSymbol;

PROCEDURE DefineSymbol (t: T;  id: M3ID.T;  s: Seg;  offset: INTEGER): INTEGER=
  VAR z := NewSym (t, id);
  BEGIN
    WITH sym = t.symtab.list[z] DO
      sym.kind   := SegToKind [s];
      sym.offset := offset;
    END;
    RETURN z;
  END DefineSymbol;

PROCEDURE DefineBssSymbol (t: T;  id: M3ID.T;  size, align: INTEGER): INTEGER=
  VAR z := NewSym (t, id);  a := FindAlign (align);  ab := AlignBytes[a];
  BEGIN
    (* align the symbol in the segment *)
    t.bss.raw_data.n_bytes := (t.bss.raw_data.n_bytes + ab - 1) DIV ab * ab;

    WITH sym = t.symtab.list[z] DO
      sym.kind   := SymKind.Bss;
      sym.offset := t.bss.raw_data.n_bytes;
    END;

    (* add the space to the segment *)
    INC (t.bss.raw_data.n_bytes, size);

    RETURN z;
  END DefineBssSymbol;

PROCEDURE MoveSymbol (t: T;  sym: INTEGER;  new_offset: INTEGER) =
  BEGIN
    t.symtab.list[sym].offset := new_offset;
  END MoveSymbol;

PROCEDURE ExportSymbol (t: T;  sym: INTEGER) =
  BEGIN
    WITH s = t.symtab.list[sym] DO
      s.export := TRUE;
      s.used   := TRUE;
    END;
  END ExportSymbol;

PROCEDURE FindAlign (align: INTEGER): Alignment =
  BEGIN
    FOR i := FIRST (AlignBytes) TO LAST (AlignBytes) DO
      IF (AlignBytes[i] = align) THEN RETURN i; END;
    END;
    <*ASSERT FALSE*>
  END FindAlign;

PROCEDURE NewSym (t: T;  id: M3ID.T): INTEGER =
  VAR x: INTEGER;
  BEGIN
    IF (t.symtab.map = NIL) THEN
      t.symtab.map := NEW (IntIntTbl.Default).init();
    END;

    IF t.symtab.map.get (id, x) THEN
      (*duplicate symbol *)
      RTError.Msg ("NTObjFile.m3", 534, "duplicate symbol: " & M3ID.ToText (id));
      <*ASSERT FALSE*>
    END;

    x := NextSym (t);
    EVAL t.symtab.map.put (id, x);

    WITH sym = t.symtab.list[x] DO
      sym.id          := id;
      sym.offset      := 0;
      sym.kind        := SymKind.Extern;
      sym.export      := FALSE;
      sym.used        := FALSE;
      sym.index       := -1;
      sym.next_func   := 0;
      sym.first_line  := 0;
      sym.last_line   := 0;
      sym.last_offset := 0;
      sym.lineno_offs := 0;
      sym.lineno_cnt  := 0;
    END;
    RETURN x;
  END NewSym;

PROCEDURE NextSym (t: T): INTEGER =
  VAR x := t.symtab.chunk.cnt;
  BEGIN
    INC (t.symtab.chunk.cnt);
    INC (t.symtab.chunk.n_bytes, SymTabSize);
    IF (t.symtab.list = NIL) OR (x >= NUMBER (t.symtab.list^)) THEN
      ExpandSyms(t);
    END;
    RETURN x;
  END NextSym;

PROCEDURE ExpandSyms (t: T) =
  VAR n: INTEGER;  new: SymbolList;
  BEGIN
    IF (t.symtab.list = NIL) THEN
      t.symtab.list := NEW (SymbolList, 100);
    ELSE
      n := NUMBER (t.symtab.list^);
      new := NEW (SymbolList, n + n);
      SUBARRAY (new^, 0, n) := t.symtab.list^;
      t.symtab.list := new;
    END;
  END ExpandSyms;

PROCEDURE SetSourceFile (t: T;  filename: TEXT) =
  VAR
    z     : INTEGER;
    len   := Text.Length (filename);
    n_aux := (len + SymTabSize - 1) DIV SymTabSize;
    obj   : TEXT;
  BEGIN
    IF (t.filename # NIL) THEN RETURN END;
    t.filename := filename;

    z := NewSym (t, M3ID.Add (".file"));
    t.filesym := z;
    WITH sym = t.symtab.list[z] DO
      sym.kind   := SymKind.File;
      sym.offset := n_aux;
      sym.used   := TRUE;
    END;

    (* add the auxillary symbols for the filename *)
    FOR i := 0 TO n_aux-1 DO
      z := NextSym (t);
      WITH sym = t.symtab.list[z] DO
        sym.id          := M3ID.NoID;
        sym.kind        := SymKind.FileAux;
        sym.offset      := i * SymTabSize;
        sym.used        := TRUE;
        sym.index       := -1;
        sym.next_func   := 0;
        sym.first_line  := 0;
        sym.last_line   := 0;
        sym.last_offset := 0;
        sym.lineno_offs := 0;
        sym.lineno_cnt  := 0;
      END;
    END;

    IF (t.gen_debugging) THEN
      (* add the debugging info *)
      obj := ObjectName (filename);
      AddRaw  (t.debug_S, 7 + Text.Length (obj), 2);
      AddRaw  (t.debug_S, S_OBJNAME, 2);
      AddRaw  (t.debug_S, 0, 4); (* checksum for precompiled types *)
      AddName (t.debug_S, obj);
    END;

  END SetSourceFile;

PROCEDURE ObjectName (src: TEXT): TEXT =
  VAR
    len := Text.Length (src);
    last_slash := -1;
    ext : TEXT;
    ch: CHAR;
  BEGIN
    (* chop off any path prefix *)
    FOR i := 0 TO len-1 DO
      ch := Text.GetChar (src, i);
      IF (ch = '/') OR (ch = '\134') THEN last_slash := i; END;
    END;
    INC (last_slash);
    src := Text.Sub (src, last_slash);
    DEC (len, last_slash);

    (* fix the extension *)
    IF (len > 3) THEN
      ext := Text.Sub (src, len-3);
      IF Text.Equal (ext, ".m3") OR Text.Equal (ext, ".i3") THEN
        src := Text.Sub (src, 0, len-1) & "o";
      END;
    END;

    RETURN src;
  END ObjectName;

PROCEDURE SetSourceLine (t: T;  source_line: INTEGER) =
  BEGIN
    IF (source_line <= 0) THEN RETURN END;
    IF (t.in_procedure) THEN
      AddSourceLine (t, t.text.raw_data.n_bytes, source_line);
      WITH s = t.symtab.list [t.last_proc] DO
        s.first_line := MIN (s.first_line, source_line);
      END;
    END;
    t.last_source_line := source_line;
  END SetSourceLine;

PROCEDURE AddSourceLine (t: T;  addr, line: INTEGER) =
  BEGIN
    WITH c = t.text.line_numbers, list = t.text.line_nums DO
      IF (list = NIL) OR (c.cnt >= NUMBER (list^)) THEN
        ExpandLines (t.text);
      END;
      IF (line # 0) AND (c.cnt > 0) THEN
        WITH last_ln = list[c.cnt-1] DO
          IF (last_ln.line # 0) AND (last_ln.addr = addr) THEN
            (* forget the last line number, it's at the same pc *)
            DEC (c.cnt);
            DEC (c.n_bytes, LineNumSize);
          END;
        END;
      END;
      WITH ln = list [c.cnt] DO
        ln.addr := addr;
        ln.line := line;
      END;
      INC (c.cnt);
      INC (c.n_bytes, LineNumSize);
    END;
  END AddSourceLine;

PROCEDURE ExpandLines (VAR s: Section) =
  VAR n: INTEGER;  new: LineNums;
  BEGIN
    IF (s.line_nums = NIL) THEN
      s.line_nums := NEW (LineNums, 100);
    ELSE
      n := NUMBER (s.line_nums^);
      new := NEW (LineNums, n + n);
      SUBARRAY (new^, 0, n) := s.line_nums^;
      s.line_nums := new;
    END;
  END ExpandLines;

(*----------------------------------------------------- debugging support ---*)

PROCEDURE DeclareTypename (t: T;  type: TypeUID;  n: Name) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL n;
  END DeclareTypename;

PROCEDURE DeclareArray (t: T;  type, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL index; EVAL elt; EVAL s;
  END DeclareArray;

PROCEDURE DeclareOpenArray (t: T;  type, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL elt; EVAL s;
  END DeclareOpenArray;

PROCEDURE DeclareEnum (t: T;  type: TypeUID; n_elts: INTEGER;  s: BitSize) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL n_elts; EVAL s;
  END DeclareEnum;

PROCEDURE DeclareEnumElt (t: T;  n: Name) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n;
  END DeclareEnumElt;

PROCEDURE DeclarePacked (t: T;  type: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL s; EVAL base;
  END DeclarePacked;

PROCEDURE DeclareRecord (t: T;  type: TypeUID;  s: BitSize;  n_fields: INTEGER) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL s; EVAL n_fields;
  END DeclareRecord;

PROCEDURE DeclareField (t: T;  n: Name;  o: BitOffset;  s: BitSize;  type: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n; EVAL o; EVAL s; EVAL type;
  END DeclareField;

PROCEDURE DeclareSet (t: T;  type, domain: TypeUID;  s: BitSize) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL domain; EVAL s;
  END DeclareSet;

PROCEDURE DeclareSubrange (t: T;  type, domain: TypeUID; READONLY min,max: Target.Int;
                           s: BitSize) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL domain; EVAL min; EVAL max; EVAL s;
  END DeclareSubrange;

PROCEDURE DeclarePointer (t: T;  type, target: TypeUID;  brand: TEXT;  traced: BOOLEAN) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL target; EVAL brand; EVAL traced;
  END DeclarePointer;

PROCEDURE DeclareIndirect (t: T;  type, target: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL target;
  END DeclareIndirect;

PROCEDURE DeclareProctype (t: T;  type: TypeUID;  n_formals: INTEGER;
                           result: TypeUID;  n_raises: INTEGER) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL n_formals; EVAL result; EVAL n_raises;
  END DeclareProctype;

PROCEDURE DeclareFormal (t: T;  n: Name;  type: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n; EVAL type;
  END DeclareFormal;

PROCEDURE DeclareRaises (t: T;  n: Name) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n;
  END DeclareRaises;

PROCEDURE DeclareObject (t: T;  type, super: TypeUID;  brand: TEXT;
                         traced: BOOLEAN;  n_fields, n_methods: INTEGER;
                         field_size: BitSize) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL super; EVAL brand;
    EVAL traced; EVAL n_fields; EVAL n_methods;
    EVAL field_size;
  END DeclareObject;

PROCEDURE DeclareMethod (t: T;  n: Name;  signature: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n; EVAL signature;
  END DeclareMethod;

PROCEDURE DeclareOpaque (t: T;  type, super: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL super;
  END DeclareOpaque;

PROCEDURE RevealOpaque (t: T;  lhs, rhs: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL lhs; EVAL rhs;
  END RevealOpaque;

PROCEDURE DeclareException (t: T;  sym: INTEGER;  arg_type: TypeUID;
                            raise_proc: BOOLEAN) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym; EVAL arg_type; EVAL raise_proc;
  END DeclareException;

PROCEDURE DeclareGlobal (t: T;  sym: INTEGER;  s: ByteSize;  m3t: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym; EVAL s; EVAL m3t;
  END DeclareGlobal;

PROCEDURE DeclareConstant  (t: T;  sym: INTEGER;  s: ByteSize;  m3t: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym; EVAL s; EVAL m3t;
  END DeclareConstant;

PROCEDURE DeclareLocal (t: T;  n: Name;  s: ByteSize;  frame: ByteOffset;
                        m3t: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n; EVAL s; EVAL frame; EVAL m3t;
  END DeclareLocal;

PROCEDURE DeclareParam (t: T;  n: Name;  s: ByteSize;  frame: ByteOffset;
                        m3t: TypeUID) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL n; EVAL s; EVAL frame; EVAL m3t;
  END DeclareParam;

PROCEDURE DeclareProcedure (t: T;  sym: INTEGER;  n_params: INTEGER;
                            nested, exported: BOOLEAN) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym; EVAL n_params; EVAL nested;  EVAL exported;
  END DeclareProcedure;

PROCEDURE BeginProcedure (t: T;  sym: INTEGER) =
  BEGIN
    <*ASSERT NOT t.in_procedure*>
    t.in_procedure := TRUE;
    t.last_source_line := t.last_source_line;
    WITH s = t.symtab.list[sym] DO
      <*ASSERT s.first_line = 0*>
      s.first_line  := t.last_source_line;
      s.lineno_offs := t.text.line_numbers.n_bytes;
      s.lineno_cnt  := t.text.line_numbers.cnt;
      s.used        := TRUE;
    END;
    AddSourceLine (t, sym, 0);
    AddSourceLine (t, t.text.raw_data.n_bytes, t.last_source_line);

    IF (t.last_proc = 0)
      THEN t.first_proc := sym;
      ELSE t.symtab.list [t.last_proc].next_func := sym;
    END;
    t.last_proc := sym;
  END BeginProcedure;

PROCEDURE EndProcedure (t: T;  sym: INTEGER) =
  VAR
    export     : BOOLEAN;
    procname   : TEXT;
    code_len   : INTEGER;
    reloc_offs : INTEGER;
  BEGIN
    <*ASSERT t.in_procedure*>
    WITH s = t.symtab.list [sym] DO
      s.last_offset := t.text.raw_data.n_bytes;
      s.last_line   := t.last_source_line;
      s.lineno_cnt  := t.text.line_numbers.cnt - s.lineno_cnt;
      export   := s.export;
      procname := M3ID.ToText (s.id);
      code_len := s.last_offset - s.offset;
    END;
    t.in_procedure := FALSE;

    IF NOT t.gen_debugging THEN RETURN END;

    (* generate a fake debugging entry *)
    AddRaw (t.debug_S, 36 + Text.Length (procname), 2);
    AddRaw (t.debug_S, ProcTag [export], 2);
    AddRaw (t.debug_S, 0, 4); (* pParent *)
    AddRaw (t.debug_S, 0, 4); (* pEnd *)
    AddRaw (t.debug_S, 0, 4); (* pNext *)
    AddRaw (t.debug_S, code_len, 4);
    AddRaw (t.debug_S, PrologueLength, 4);
    AddRaw (t.debug_S, code_len - EpilogueLength, 4);
    reloc_offs := t.debug_S.raw_data.n_bytes;
    AddRaw (t.debug_S, 0, 4);  (* procedure offset *)
    AddRaw (t.debug_S, 0, 2);  (* procedure segment *)
    AddRaw (t.debug_S, 16_104, 2); (* type = proc():INTEGER;  HACK! *)
    AddRaw (t.debug_S, 0, 1);      (* flags = {} *)
    AddName (t.debug_S, procname);

    (* relocate the offset and segment *)
    AddReloc (t.debug_S, RelocKind.Offset,  0, reloc_offs,   sym);
    AddReloc (t.debug_S, RelocKind.Segment, 0, reloc_offs+4, sym);

    (* generate the matching end *)
    AddRaw (t.debug_S, 2, 2);
    AddRaw (t.debug_S, S_END, 2);

  END EndProcedure;

PROCEDURE BeginBlock (t: T) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
  END BeginBlock;

PROCEDURE EndBlock (t: T) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
  END EndBlock;

PROCEDURE NoteProcedureOrigin (t: T;  sym: INTEGER) =
  BEGIN
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym;
  END NoteProcedureOrigin;

(*---------------------------------------------------------------- output ---*)

PROCEDURE Dump (t: T;  wr: Wr.T) =
  VAR
    now := CoffTime.Now ();
    file_offset : INTEGER;
    cur_virtual : INTEGER;
    n_symbols   : INTEGER;
  BEGIN
    t.out.wr := wr;

    IF (t.gen_debugging) THEN
      (* add a final debugging entry that identifies the compiler *)
      AddRaw  (t.debug_S, 7 + Text.Length (CompilerName), 2);
      AddRaw  (t.debug_S, S_COMPILE, 2);
      AddRaw  (t.debug_S, 4, 1); (* Intel 486 *)
      AddRaw  (t.debug_S, 0, 3); (* lang=C, near data & code, hardware FP *)
      AddName (t.debug_S, CompilerName);
    END;

    n_symbols := ReorderSymbols (t);

    (* initialize a fresh string space *)
    WITH s = t.strings DO
      s.cnt     := 0;
      s.n_bytes := 4; (* leave room for the count *)
      s.map     := NEW (TextIntTbl.Default).init ();
      IF (s.list = NIL) THEN s.list := NEW (TextList, 100); END;
    END;

    (* compute the file and address space layouts *)
    file_offset := HeaderSize + t.n_sections * SectionSize;
    cur_virtual := 0;
    LayoutSection (t.drectve, file_offset, cur_virtual);
    LayoutSection (t.debug_S, file_offset, cur_virtual);
    LayoutSection (t.text,    file_offset, cur_virtual);
    LayoutSection (t.data,    file_offset, cur_virtual);
    LayoutSection (t.bss,     file_offset, cur_virtual);
    LayoutSection (t.debug_T, file_offset, cur_virtual);
    LayoutChunk   (t.symtab.chunk, file_offset, t.symtab.list);

    (* write the file header *)
    OutS (t, 16_14c);  (* == Intel 386 *)
    OutS (t, t.n_sections);
    OutI (t, now);
    OutI (t, t.symtab.chunk.file_offset);
    OutI (t, t.symtab.chunk.cnt);
    OutS (t, 0);       (* size of optional header *)
    OutS (t, 0);       (* flags => +reloc, -exec, +lineno, +locals *)

    WriteSectionHeader (t, t.drectve);
    WriteSectionHeader (t, t.debug_S);
    WriteSectionHeader (t, t.text);
    WriteSectionHeader (t, t.data);
    WriteSectionHeader (t, t.bss);
    WriteSectionHeader (t, t.debug_T);

    WriteSection (t, t.drectve);
    WriteSection (t, t.debug_S);
    WriteSection (t, t.text);
    WriteSection (t, t.data);
    WriteSection (t, t.bss);
    WriteSection (t, t.debug_T);

    WriteSymbols (t, n_symbols);
    WriteStrings (t);

    Flush (t);
    t.out.wr := NIL;  (* give the collector a chance... *)
  END Dump;

PROCEDURE ReorderSymbols (t: T): INTEGER =
  VAR next_index := 0;  next_remap := 0;   p: INTEGER;
  BEGIN
    t.symtab.remap := NEW (Ints, t.symtab.chunk.cnt);

    IF (t.filename # NIL) THEN
      SetSym (t, t.filesym, next_index, next_remap);
      FOR i := 1 TO t.symtab.list[t.filesym].offset DO
        SetSym (t, t.filesym + i, next_index, next_remap);
      END;
    END;

    (* set the section number symbols *)
    FOR i := 0 TO 9 DO
      WITH sym = t.symtab.list[i] DO
        IF (sym.kind = SymKind.Section) AND (sym.used) AND (sym.index < 0) THEN
          SetSym (t, i, next_index, next_remap);
        END;
      END;
    END;

    (* set the symbols for procedures with bodies (in object file order) *)
    p := t.first_proc;
    WHILE (p # 0) DO
      WITH sym = t.symtab.list[p] DO
        IF (sym.used) AND (sym.index < 0) THEN
          SetSym (t, p, next_index, next_remap);
        END;
        p := sym.next_func;
      END;
    END;

    (* set the remaining symbols *)
    FOR i := 0 TO t.symtab.chunk.cnt - 1 DO
      WITH sym = t.symtab.list[i] DO
        IF (sym.used) AND (sym.index < 0) THEN
          SetSym (t, i, next_index, next_remap);
        END;
      END;
    END;

    t.symtab.chunk.cnt := next_index;
    RETURN next_remap;
  END ReorderSymbols;

PROCEDURE SetSym (t: T;  old: INTEGER;  VAR next_sym, next_remap: INTEGER) =
  BEGIN
    WITH sym = t.symtab.list[old] DO
      sym.index := next_sym;              INC (next_sym);
      t.symtab.remap[next_remap] := old;  INC (next_remap);
      CASE sym.kind OF
      | SymKind.Text => IF (sym.first_line # 0) THEN INC (next_sym, 6); END;
      | SymKind.Section => INC (next_sym); (* 1 aux symbol *)
      ELSE (* skip *)
      END;
    END;
  END SetSym;

PROCEDURE LayoutSection (VAR s: Section;  VAR offs, addr: INTEGER) =
  BEGIN
    IF (s.id <= 0) THEN RETURN END;
    s.address := addr;   INC (addr, s.raw_data.n_bytes);
    LayoutChunk (s.raw_data, offs, s.data);
    LayoutChunk (s.relocation, offs, s.relocs);
    LayoutChunk (s.line_numbers, offs, s.line_nums);
  END LayoutSection;

PROCEDURE LayoutChunk (VAR c: Chunk;  VAR offs: INTEGER;  data: REFANY) =
  BEGIN
    IF (c.n_bytes > 0) AND (data # NIL) THEN
      c.file_offset := offs;
      INC (offs, c.n_bytes);
    ELSE
      c.file_offset := 0;
    END;
  END LayoutChunk;

PROCEDURE WriteSectionHeader (t: T;  VAR s: Section) =
  BEGIN
    IF (s.id <= 0) THEN RETURN END;
    OutN (t, s.name);
    OutI (t, s.address);  (* physical address *)
    OutI (t, 0);  (* virtual address *)
    OutI (t, s.raw_data.n_bytes);
    OutI (t, s.raw_data.file_offset);
    OutI (t, s.relocation.file_offset);
    OutI (t, s.line_numbers.file_offset);
    OutS (t, s.relocation.cnt);
    OutS (t, s.line_numbers.cnt);
    OutI (t, s.flags);
  END WriteSectionHeader;

PROCEDURE WriteSection (t: T;  VAR s: Section) =
  VAR base_line: INTEGER;
  BEGIN
    IF (s.id <= 0) THEN RETURN END;

    (* raw data *)
    IF (s.data # NIL) THEN
      FOR i := 0 TO s.raw_data.n_bytes-1 DO
        OutC (t, s.data[i]);
      END;
    END;

    (* relocation info *)
    FOR i := 0 TO s.relocation.cnt-1 DO
      WITH r = s.relocs[i] DO
        CASE r.kind OF
        | RelocKind.Symbol =>
            OutI (t, r.src_offset + t.symtab.list[r.src_sym].offset);
            OutI (t, t.symtab.list[r.target_sym].index);
            OutS (t, REL_I386_DIR32);  (* 32-bit direct relocation *)
        | RelocKind.Offset =>
            OutI (t, r.src_offset);
            OutI (t, t.symtab.list[r.target_sym].index);
            OutS (t, REL_I386_SECREL); (* section offset *)
        | RelocKind.Segment =>
            OutI (t, r.src_offset);
            OutI (t, t.symtab.list[r.target_sym].index);
            OutS (t, REL_I386_SECTION);  (* section id *)
        END;
      END;
    END;

    (* line numbers *)
    base_line := 0;
    FOR i := 0 TO s.line_numbers.cnt-1 DO
      WITH ln = s.line_nums[i] DO
        IF (ln.line = 0) THEN
          WITH sym = t.symtab.list[ln.addr] DO
            base_line := sym.first_line - 1;
            OutI (t, sym.index);
            OutS (t, 0);
          END;
        ELSE
          OutI (t, ln.addr);
          OutS (t, ln.line - base_line);
        END;
      END;
    END;
  END WriteSection;

PROCEDURE WriteSymbols (t: T;  n_symbols: INTEGER) =
  BEGIN
    WITH s = t.symtab DO
      FOR i := 0 TO n_symbols-1 DO
        WriteSym (t, s.list [t.symtab.remap [i]]);
      END;
    END;
  END WriteSymbols;

PROCEDURE WriteSym (t: T;  READONLY sym: Symbol) =
  CONST SClass = ARRAY BOOLEAN OF INTEGER { 3, 2 };
  VAR next_func: INTEGER;  has_body: BOOLEAN;
  BEGIN
    CASE sym.kind OF

    | SymKind.Text =>
        has_body := (sym.first_line # 0);
        OutN (t, M3ID.ToText (sym.id));
        OutI (t, sym.offset);
        OutS (t, t.text.id);       (* section = Text *)
        IF (has_body) THEN
          OutS (t, 16_20);         (* type = function *)
          OutC (t, VAL (SClass [TRUE], CHAR));
                                   (* storage class = static/extern *)
          OutC (t, '\001');        (* #aux = 1 *)
        ELSE
          OutS (t, 0);             (* type = no type *)
          OutC (t, VAL (SClass [sym.export], CHAR));
                                   (* storage class = static/extern *)
          OutC (t, '\000');        (* #aux = 0 *)
        END;

        (** HACK: We make all procedures with source lines external so
            they show up in windbg call stacks.  When we generate full
            CodeView debugging info, we can probably remove the hack. **)

        IF (has_body) THEN
          next_func := 0;
          IF (sym.next_func # 0) THEN
            next_func := t.symtab.list [sym.next_func].index;
          END;

          (* function definiton auxillary info *)
          OutI (t, sym.index + 2); (* pointer to ".bf" entry *)
          OutI (t, sym.last_offset - sym.offset);  (* size of code *)
          OutI (t, t.text.line_numbers.file_offset + sym.lineno_offs);
                                   (* pointer into line number table *)
          OutI (t, next_func);     (* pointer to next function *)
          OutS (t, 0); (* unused *)

          (* ".bf" entry *)
          OutN (t, ".bf");
          OutI (t, sym.offset);      (* initial pc *)
          OutS (t, t.text.id);       (* section = Text *)
          OutS (t, 0);               (* type = none *)
          OutS (t, 101 + 256);       (* class = function, #aux = 1 *)

          (* ".bf" aux entry *)
          OutI (t, 0);               (* unused *)
          OutS (t, sym.first_line-1);  (* actual source line number *)
          OutI (t, 0);               (* unused *)
          OutS (t, 0);               (* unused *)
          IF (next_func # 0)         (* next ".bf" entry *)
            THEN OutI (t, next_func+2);
            ELSE OutI (t, 0);
          END;
          OutS (t, 0);               (* unused *)

          (* ".lf" entry *)
          OutN (t, ".lf");
          (***
          OutI (t, sym.last_line - sym.first_line + 1);  (* # source lines *)
          ***)
          OutI (t, sym.lineno_cnt);  (* # line number entries *)
          OutS (t, t.text.id);       (* section = Text *)
          OutS (t, 0);               (* type = none *)
          OutS (t, 101);             (* class = function, #aux = 0 *)

          (* ".ef" entry *)
          OutN (t, ".ef");
          OutI (t, sym.last_offset); (* final pc *)
          OutS (t, t.text.id);       (* section = Text *)
          OutS (t, 0);               (* type = none *)
          OutS (t, 101 + 256);       (* class = function, #aux = 1 *)

          (* ".ef" aux entry *)
          OutI (t, 0);               (* unused *)
          OutS (t, sym.last_line);   (* actual source line number *)
          OutI (t, 0);               (* unused *)
          OutS (t, 0);               (* unused *)
          OutI (t, 0);               (* unused *)
          OutS (t, 0);               (* unused *)
        END;

    | SymKind.Data =>
        OutN (t, M3ID.ToText (sym.id));
        OutI (t, sym.offset);
        OutS (t, t.data.id);   (* section = Data *)
        OutS (t, 0);           (* type = no type *)
        OutS (t, SClass [sym.export]);
                               (* storage class = static/extern, #aux = 0 *)

    | SymKind.Bss  =>
        OutN (t, M3ID.ToText (sym.id));
        OutI (t, sym.offset);
        OutS (t, t.bss.id);    (* section = bss *)
        OutS (t, 0);           (* type = no type *)
        OutS (t, SClass [sym.export]);
                               (* storage class = static/extern, #aux = 0 *)

    | SymKind.Extern =>
        OutN (t, M3ID.ToText (sym.id));
        OutI (t, 0);           (* value = 0 *)
        OutS (t, 0);           (* section = extern *)
        OutS (t, 0);           (* type = no type *)
        OutS (t, 2);           (* storage class = extern, #aux = 0 *)

    | SymKind.File =>
        OutN (t, ".file");
        OutI (t, 0);           (* value = 0 *)
        OutS (t, -2);          (* section = debug *)
        OutS (t, 0);           (* type = no type *)
        OutC (t, '\147');      (* storage class = file *)
        OutC (t, VAL (sym.offset, CHAR)); (* #aux *)

    | SymKind.FileAux =>
        VAR
          name  := t.filename;
          len   := Text.Length (name);
          start := sym.offset;
          stop  := MIN (start + SymTabSize, len);
        BEGIN
          FOR i := start TO stop-1 DO OutP (t, Text.GetChar (name, i)); END;
          FOR i := stop TO start + SymTabSize - 1 DO OutC (t, '\000'); END;
        END;

    | SymKind.Section =>
        OutN (t, M3ID.ToText (sym.id));
        OutI (t, 0);           (* value = 0 *)
        OutS (t, sym.offset);  (* section # *)
        OutS (t, 0);           (* type = no type *)
        OutC (t, '\003');      (* storage class = static *)
        OutC (t, '\001');      (* #aux = 1*)

        IF    (sym.offset = t.drectve.id) THEN WriteSectAux (t, t.drectve);
        ELSIF (sym.offset = t.debug_S.id) THEN WriteSectAux (t, t.debug_S);
        ELSIF (sym.offset = t.text.id)    THEN WriteSectAux (t, t.text);
        ELSIF (sym.offset = t.data.id)    THEN WriteSectAux (t, t.data);
        ELSIF (sym.offset = t.bss.id)     THEN WriteSectAux (t, t.bss);
        ELSIF (sym.offset = t.debug_T.id) THEN WriteSectAux (t, t.debug_T);
        END;
    END;
  END WriteSym;

PROCEDURE WriteSectAux (t: T;  READONLY s: Section) =
  BEGIN
    <*ASSERT s.id > 0 *>
    OutI (t, s.raw_data.n_bytes);
    OutS (t, s.relocation.cnt);
    OutS (t, s.line_numbers.cnt);
    OutI (t, 0);  (* checksum *)
    OutS (t, 0 (*** s.id -- Microsoft C compiler generates zero *****));
    OutI (t, 0);  (* unused *)
  END WriteSectAux;

PROCEDURE WriteStrings (t: T) =
  BEGIN
    WITH s = t.strings DO
      IF (s.cnt = 0) THEN OutI (t, 0); RETURN; END;
      OutI (t, s.n_bytes);
      FOR i := 0 TO s.cnt-1 DO
        OutT (t, s.list[i]);
        OutC (t, '\000');
      END;
    END;
  END WriteStrings;

(*--------------------------------------------- low-level output routines ---*)

PROCEDURE OutN (t: T;  nm: TEXT) =
  (* writes an 8-byte name *)
  VAR len := Text.Length (nm);
  BEGIN
    IF (len <= 8) THEN
      FOR i := 0 TO len-1 DO OutC (t, Text.GetChar (nm, i)); END;
      FOR i := len TO 7   DO OutC (t, '\000'); END;
    ELSE
      OutI (t, 0);
      OutI (t, AddString (t, nm));
    END;
  END OutN;

PROCEDURE OutI (t: T;  i: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));
  END OutI;

PROCEDURE OutS (t: T;  i: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));
  END OutS;

PROCEDURE OutT (t: T;  txt: TEXT) =
  VAR len := Text.Length (txt);
  BEGIN
    FOR i := 0 TO len-1 DO OutC (t, Text.GetChar (txt, i)); END;
  END OutT;

PROCEDURE OutP (t: T;  c: CHAR) =
  (* convert Unix path name characters to NT characters *)
  BEGIN
    IF (c = '/') THEN c := '\134'; END;
    OutC (t, c);
  END OutP;

PROCEDURE OutC (t: T;  c: CHAR) =
  BEGIN
    IF (t.out.len >= NUMBER (t.out.buf)) THEN Flush (t); END;
    t.out.buf [t.out.len] := c;
    INC (t.out.len);
  END OutC;

PROCEDURE Flush (t: T) =
  <*FATAL ANY*>
  BEGIN
    Wr.PutString (t.out.wr, SUBARRAY (t.out.buf, 0, t.out.len));
    t.out.len := 0;
  END Flush;

PROCEDURE AddString (t: T;  txt: TEXT): INTEGER =
  VAR offset: INTEGER;
  BEGIN
    WITH s = t.strings DO
      IF NOT s.map.get (txt, offset) THEN
        offset := s.n_bytes;
        INC (s.n_bytes, 1 + Text.Length (txt));
        EVAL s.map.put (txt, offset);
        IF (s.list = NIL) OR (s.cnt >= NUMBER (s.list^)) THEN
          ExpandStrings (s);
        END;
        s.list[s.cnt] := txt;
        INC (s.cnt);
      END;
    END;
    RETURN offset;
  END AddString;

PROCEDURE ExpandStrings (VAR s: StringTable) =
  VAR n: INTEGER;  new: TextList;
  BEGIN
    IF (s.list = NIL) THEN
      s.list := NEW (TextList, 100);
    ELSE
      n := NUMBER (s.list^);
      new := NEW (TextList, n+n);
      SUBARRAY (new^, 0, n) := s.list^;
      s.list := new;
    END;
  END ExpandStrings;

BEGIN
END NTObjFile.
