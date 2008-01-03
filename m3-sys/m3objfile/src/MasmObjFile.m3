(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Jan 30 08:32:52 PST 1995 by kalsow   *)

MODULE MasmObjFile;

IMPORT Wr, Word, IntIntTbl, IntArraySort;
IMPORT M3ObjFile, M3ID, M3Buf, Target;
FROM M3CG IMPORT Name, BitOffset, BitSize, ByteOffset, ByteSize, TypeUID;

TYPE
  Seg = M3ObjFile.Seg;

TYPE
  Byte = BITS 8 FOR [0..255];
  Bytes = REF ARRAY OF Byte;

TYPE
  SKind = { Text, Data, Bss, Extern };

CONST
  SegToKind = ARRAY Seg OF SKind { SKind.Text, SKind.Data };

TYPE
  SymbolList = REF ARRAY OF Symbol;
  Symbol = RECORD
    id     : M3ID.T;
    kind   : SKind;
    offset : INTEGER; (* align for SKind = Bss *)
    size   : INTEGER;
    export : BOOLEAN;
    used   : BOOLEAN;
  END;

TYPE
  Alignment = [0..3];
CONST
  AlignBytes = ARRAY Alignment OF [0..8] { 1, 2, 4, 8 };
  AlignName  = ARRAY Alignment OF TEXT { "BYTE", "WORD", "DWORD", "QWORD" };
  AlignDefn  = ARRAY Alignment OF TEXT { "DB", "DW", "DD", "DQ" };

TYPE
  RelocList = REF ARRAY OF Reloc;
  Reloc = RECORD
    src_sym, src_offset : INTEGER;
    target_sym          : INTEGER;
  END;

TYPE
  LineNumList = REF ARRAY OF LineNum;
  LineNum = RECORD pc, line: INTEGER; END;

REVEAL
  T = M3ObjFile.T BRANDED "MasmObjFile.T" OBJECT
    size     := ARRAY Seg OF INTEGER { 0, .. };
    bytes    := ARRAY Seg OF Bytes { NIL, .. };
    sym_map  : IntIntTbl.T := NIL;
    symbols  : SymbolList := NIL;
    n_syms   := 0;
    relocs   : RelocList := NIL;
    n_relocs := 0;
    n_bss    := 0;
    n_lines  := 0;
    last_pc  := 0;
    lines    : LineNumList := NIL;
    file     : TEXT := NIL;
  OVERRIDES
    cursor              := Cursor;
    append              := Append;
    patch               := Patch;
    relocate            := Relocate;
    import_symbol       := ImportSymbol;
    define_symbol       := DefineSymbol;
    define_bss_symbol   := DefineBssSymbol;
    move_symbol         := MoveSymbol;
    export_symbol       := ExportSymbol;
    set_source_file     := SetSourceFile;
    set_source_line     := SetSourceLine;

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

PROCEDURE New (): T =
  BEGIN
    RETURN NEW (T);
  END New;

PROCEDURE Cursor (t: T;  s: Seg): INTEGER =
  BEGIN
    RETURN t.size [s];
  END Cursor;

PROCEDURE Append (t: T;  s: Seg;  value, length: INTEGER) =
  VAR
    offs := t.size [s];
    seg  := EnsureLength (t.bytes[s], offs + length);
  BEGIN
    WHILE (length > 0) DO
      seg[offs] := Word.And (value, 16_ff);
      value := Word.RightShift (value, 8);
      INC (offs);
      DEC (length);
    END;
    t.size[s] := offs;
  END Append;

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
  VAR seg := t.bytes[s];
  BEGIN
    <*ASSERT t.size [s] > offset *>
    WHILE (length > 0) DO
      seg[offset] := Word.And (value, 16_ff);
      value := Word.RightShift (value, 8);
      INC (offset);
      DEC (length);
    END;
  END Patch;

PROCEDURE Relocate (t: T;  src_sym, src_offs, tar_sym: INTEGER) =
  BEGIN
    IF (t.relocs = NIL) OR (t.n_relocs >= NUMBER (t.relocs^)) THEN
      ExpandRelocs (t);
    END;
    WITH r = t.relocs [t.n_relocs] DO
      r.src_sym       := src_sym;
      r.src_offset    := src_offs;
      r.target_sym    := tar_sym;
    END;
    t.symbols [src_sym].used := TRUE;
    t.symbols [tar_sym].used := TRUE;
    INC (t.n_relocs);
  END Relocate;

PROCEDURE ExpandRelocs (t: T) =
  VAR n: INTEGER;  new: RelocList;
  BEGIN
    IF (t.relocs = NIL) THEN
      t.relocs := NEW (RelocList, 100);
    ELSE
      n := NUMBER (t.relocs^);
      new := NEW (RelocList, n + n);
      SUBARRAY (new^, 0, n) := t.relocs^;
      t.relocs := new;
    END;
  END ExpandRelocs;

PROCEDURE ImportSymbol (t: T;  id: M3ID.T): INTEGER =
  VAR z := NewSym (t, id);
  BEGIN
    WITH sym = t.symbols[z] DO
      sym.kind   := SKind.Extern;
      sym.export := FALSE;
      sym.offset := z;
      sym.size   := 0;
    END;
    RETURN z;
  END ImportSymbol;

PROCEDURE DefineSymbol (t: T;  id: M3ID.T;  s: Seg;  offset: INTEGER): INTEGER=
  VAR z := NewSym (t, id);
  BEGIN
    WITH sym = t.symbols[z] DO
      sym.kind   := SegToKind [s];
      sym.offset := offset;
      sym.size   := 0;
    END;
    RETURN z;
  END DefineSymbol;

PROCEDURE DefineBssSymbol (t: T;  id: M3ID.T;  size, align: INTEGER): INTEGER=
  VAR z := NewSym (t, id);  a := FindAlign (align);
  BEGIN
    WITH sym = t.symbols[z] DO
      sym.kind   := SKind.Bss;
      sym.offset := a;
      sym.size   := (size + AlignBytes[a] - 1) DIV AlignBytes[a];
    END;
    INC (t.n_bss);
    RETURN z;
  END DefineBssSymbol;

PROCEDURE MoveSymbol (t: T;  sym: INTEGER;  new_offset: INTEGER) =
  BEGIN
    t.symbols[sym].offset := new_offset;
  END MoveSymbol;

PROCEDURE ExportSymbol (t: T;  sym: INTEGER) =
  BEGIN
    WITH s = t.symbols[sym] DO
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
    IF (t.sym_map = NIL) THEN t.sym_map := NEW (IntIntTbl.Default).init(); END;
    IF t.sym_map.get (id, x) THEN <*ASSERT FALSE*> (* duplicate symbol *) END;
    x := t.n_syms;  INC (t.n_syms);
    EVAL t.sym_map.put (id, x);
    IF (t.symbols = NIL) OR (x >= NUMBER (t.symbols^)) THEN ExpandSyms(t); END;
    WITH sym = t.symbols[x] DO
      sym.id     := id;
      sym.kind   := SKind.Extern;
      sym.offset := 0;
      sym.size   := 0;
      sym.export := FALSE;
      sym.used   := FALSE;
    END;
    RETURN x;
  END NewSym;

PROCEDURE ExpandSyms (t: T) =
  VAR n: INTEGER;  new: SymbolList;
  BEGIN
    IF (t.symbols = NIL) THEN
      t.symbols := NEW (SymbolList, 100);
    ELSE
      n := NUMBER (t.symbols^);
      new := NEW (SymbolList, n + n);
      SUBARRAY (new^, 0, n) := t.symbols^;
      t.symbols := new;
    END;
  END ExpandSyms;

PROCEDURE SetSourceFile (t: T;  filename: TEXT) =
  BEGIN
    t.file := filename;
  END SetSourceFile;

PROCEDURE SetSourceLine (t: T;  source_line: INTEGER) =
  BEGIN
    IF (t.lines = NIL) OR (t.n_lines >= NUMBER (t.lines^)) THEN
      ExpandLines(t);
    END;
    IF (t.n_lines > 0) AND (t.last_pc = t.size [Seg.Text]) THEN
      (* forget the last line number *)
      DEC (t.n_lines);
    END;
    WITH ln = t.lines [t.n_lines] DO
      ln.pc   := t.size [Seg.Text];
      ln.line := source_line;
    END;
    INC (t.n_lines);
  END SetSourceLine;

PROCEDURE ExpandLines (t: T) =
  VAR n: INTEGER;  new: LineNumList;
  BEGIN
    IF (t.lines = NIL) THEN
      t.lines := NEW (LineNumList, 100);
    ELSE
      n := NUMBER (t.lines^);
      new := NEW (LineNumList, n + n);
      SUBARRAY (new^, 0, n) := t.lines^;
      t.lines := new;
    END;
  END ExpandLines;

(*----------------------------------------------------- debugging support ---*)

PROCEDURE DeclareTypename (t: T;  type: TypeUID;  n: Name) =
  BEGIN
    EVAL t; EVAL type; EVAL n;
  END DeclareTypename;

PROCEDURE DeclareArray (t: T;  type, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    EVAL t; EVAL type; EVAL index; EVAL elt; EVAL s;
  END DeclareArray;

PROCEDURE DeclareOpenArray (t: T;  type, elt: TypeUID;  s: BitSize) =
  BEGIN
    EVAL t; EVAL type; EVAL elt; EVAL s;
  END DeclareOpenArray;

PROCEDURE DeclareEnum (t: T;  type: TypeUID; n_elts: INTEGER;  s: BitSize) =
  BEGIN
    EVAL t; EVAL type; EVAL n_elts; EVAL s;
  END DeclareEnum;

PROCEDURE DeclareEnumElt (t: T;  n: Name) =
  BEGIN
    EVAL t; EVAL n;
  END DeclareEnumElt;

PROCEDURE DeclarePacked (t: T;  type: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    EVAL t;  EVAL type; EVAL s; EVAL base;
  END DeclarePacked;

PROCEDURE DeclareRecord (t: T;  type: TypeUID;  s: BitSize;  n_fields: INTEGER) =
  BEGIN
    EVAL t; EVAL type; EVAL s; EVAL n_fields;
  END DeclareRecord;

PROCEDURE DeclareField (t: T;  n: Name;  o: BitOffset;  s: BitSize;  type: TypeUID) =
  BEGIN
    EVAL t; EVAL n; EVAL o; EVAL s; EVAL type;
  END DeclareField;

PROCEDURE DeclareSet (t: T;  type, domain: TypeUID;  s: BitSize) =
  BEGIN
    EVAL t; EVAL type; EVAL domain; EVAL s;
  END DeclareSet;

PROCEDURE DeclareSubrange (t: T;  type, domain: TypeUID; READONLY min,max: Target.Int;
                           s: BitSize) =
  BEGIN
    EVAL t; EVAL type; EVAL domain; EVAL min; EVAL max; EVAL s;
  END DeclareSubrange;

PROCEDURE DeclarePointer (t: T;  type, target: TypeUID;  brand: TEXT;  traced: BOOLEAN) =
  BEGIN
    EVAL t; EVAL type; EVAL target; EVAL brand; EVAL traced;
  END DeclarePointer;

PROCEDURE DeclareIndirect (t: T;  type, target: TypeUID) =
  BEGIN
    EVAL t; EVAL type; EVAL target;
  END DeclareIndirect;

PROCEDURE DeclareProctype (t: T;  type: TypeUID;  n_formals: INTEGER;
                           result: TypeUID;  n_raises: INTEGER) =
  BEGIN
    EVAL t; EVAL type; EVAL n_formals; EVAL result; EVAL n_raises;
  END DeclareProctype;

PROCEDURE DeclareFormal (t: T;  n: Name;  type: TypeUID) =
  BEGIN
    EVAL t; EVAL n; EVAL type;
  END DeclareFormal;

PROCEDURE DeclareRaises (t: T;  n: Name) =
  BEGIN
    EVAL t;  EVAL n;
  END DeclareRaises;

PROCEDURE DeclareObject (t: T;  type, super: TypeUID;  brand: TEXT;
                         traced: BOOLEAN;  n_fields, n_methods: INTEGER;
                         field_size: BitSize) =
  BEGIN
    EVAL t; EVAL type; EVAL super; EVAL brand;
    EVAL traced; EVAL n_fields; EVAL n_methods;
    EVAL field_size;
  END DeclareObject;

PROCEDURE DeclareMethod (t: T;  n: Name;  signature: TypeUID) =
  BEGIN
    EVAL t;  EVAL n; EVAL signature;
  END DeclareMethod;

PROCEDURE DeclareOpaque (t: T;  type, super: TypeUID) =
  BEGIN
    EVAL t; EVAL type; EVAL super;
  END DeclareOpaque;

PROCEDURE RevealOpaque (t: T;  lhs, rhs: TypeUID) =
  BEGIN
    EVAL t; EVAL lhs; EVAL rhs;
  END RevealOpaque;

PROCEDURE DeclareException (t: T;  sym: INTEGER;  arg_type: TypeUID;
                            raise_proc: BOOLEAN) =
  BEGIN
    EVAL t;  EVAL sym; EVAL arg_type; EVAL raise_proc;
  END DeclareException;

PROCEDURE DeclareGlobal (t: T;  sym: INTEGER;  s: ByteSize;  m3t: TypeUID) =
  BEGIN
    EVAL t;  EVAL sym; EVAL s; EVAL m3t;
  END DeclareGlobal;

PROCEDURE DeclareConstant  (t: T;  sym: INTEGER;  s: ByteSize;  m3t: TypeUID) =
  BEGIN
    EVAL t; EVAL sym; EVAL s; EVAL m3t;
  END DeclareConstant;

PROCEDURE DeclareLocal (t: T;  n: Name;  s: ByteSize;  frame: ByteOffset;
                        m3t: TypeUID) =
  BEGIN
    EVAL t;  EVAL n; EVAL s; EVAL frame; EVAL m3t;
  END DeclareLocal;

PROCEDURE DeclareParam (t: T;  n: Name;  s: ByteSize;  frame: ByteOffset;
                        m3t: TypeUID) =
  BEGIN
    EVAL t;  EVAL n; EVAL s; EVAL frame; EVAL m3t;
  END DeclareParam;

PROCEDURE DeclareProcedure (t: T;  sym: INTEGER;  n_params: INTEGER;
                            nested, exported: BOOLEAN) =
  BEGIN
    EVAL t;  EVAL sym; EVAL n_params; EVAL nested;  EVAL exported;
  END DeclareProcedure;

PROCEDURE BeginProcedure (t: T;  sym: INTEGER) =
  BEGIN
    EVAL t;  EVAL sym;
  END BeginProcedure;

PROCEDURE EndProcedure (t: T;  sym: INTEGER) =
  BEGIN
    EVAL t; EVAL sym;
  END EndProcedure;

PROCEDURE BeginBlock (t: T) =
  BEGIN
    EVAL t;
  END BeginBlock;

PROCEDURE EndBlock (t: T) =
  BEGIN
    EVAL t;
  END EndBlock;

PROCEDURE NoteProcedureOrigin (t: T;  sym: INTEGER) =
  BEGIN
    EVAL t; EVAL sym;
  END NoteProcedureOrigin;

(*---------------------------------------------------------------- output ---*)

TYPE
  DState = RECORD
    t          : T;
    buf        : M3Buf.T;
    sym_map    : REF ARRAY OF INTEGER;
    reloc_map  : REF ARRAY OF INTEGER;
    next_sym   : INTEGER;
    next_reloc : INTEGER;
    next_line  : INTEGER;
  END;

PROCEDURE Dump (t: T;  wr: Wr.T) =
  CONST SegName = ARRAY Seg OF TEXT { "_TEXT", "_DATA" };
  VAR
    s                 : DState;
    offset            : INTEGER;
    seg_size          : INTEGER;
    next_sym_offset   : INTEGER;
    next_reloc_offset : INTEGER;
    seg_kind          : SKind;
  BEGIN
    s.t   := t;
    s.buf := M3Buf.New ();
    M3Buf.AttachDrain (s.buf, wr);

    (* write the object file header *)
    M3Buf.PutText (s.buf, "\t.386P"); NL(s);
    M3Buf.PutText (s.buf, "_TEXT\tSEGMENT PARA USE32 PUBLIC 'CODE'"); NL(s);
    M3Buf.PutText (s.buf, "_TEXT\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "_DATA\tSEGMENT DWORD USE32 PUBLIC 'DATA'"); NL(s);
    M3Buf.PutText (s.buf, "_DATA\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "CONST\tSEGMENT DWORD USE32 PUBLIC 'CONST'"); NL(s);
    M3Buf.PutText (s.buf, "CONST\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "_BSS\tSEGMENT DWORD USE32 PUBLIC 'BSS'"); NL(s);
    M3Buf.PutText (s.buf, "_BSS\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "_TLS\tSEGMENT DWORD USE32 PUBLIC 'TLS'"); NL(s);
    M3Buf.PutText (s.buf, "_TLS\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "$$SYMBOLS\tSEGMENT BYTE USE32 PUBLIC 'DEBSYM'"); NL(s);
    M3Buf.PutText (s.buf, "$$SYMBOLS\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "$$TYPES\tSEGMENT BYTE USE32 PUBLIC 'DEBTYP'"); NL(s);
    M3Buf.PutText (s.buf, "$$TYPES\tENDS"); NL(s);
    M3Buf.PutText (s.buf, "FLAT\tGROUP _DATA, CONST, _BSS"); NL(s);
    M3Buf.PutText (s.buf, "\tASSUME CS: FLAT, DS: FLAT, SS: FLAT"); NL(s);

    IF (t.file # NIL) THEN
      M3Buf.PutText (s.buf, "; File ");
      M3Buf.PutText (s.buf, t.file);
      NL (s);
    END;

    (* dump the imported symbols *)
    NL(s);
    FOR i := 0 TO t.n_syms-1 DO
      WITH sym = t.symbols[i] DO
        IF (sym.kind = SKind.Extern) AND (sym.used) THEN
          M3Buf.PutText (s.buf, "EXTRN\t");
          M3ID.Put (s.buf, sym.id);
          M3Buf.PutText (s.buf, ":NEAR"); NL(s);
        END;
      END;
    END;

    (* dump the exported symbols *)
    NL(s);
    FOR i := 0 TO t.n_syms-1 DO
      WITH sym = t.symbols[i] DO
        IF sym.export AND (sym.kind <= SKind.Data) THEN
          M3Buf.PutText (s.buf, "PUBLIC\t");
          M3ID.Put (s.buf, sym.id);
          NL (s);
        END;
      END;
    END;

    (* dump the common symbols *)
    NL (s);
    M3Buf.PutText (s.buf, "_DATA\tSEGMENT"); NL(s);
    FOR i := 0 TO t.n_syms-1 DO
      WITH sym = t.symbols[i] DO
        IF (sym.kind = SKind.Bss) AND (sym.export) THEN
          M3Buf.PutText (s.buf, "COMM\t");
          M3ID.Put (s.buf, sym.id);
          M3Buf.PutChar (s.buf, ':');
          M3Buf.PutText (s.buf, AlignName [sym.offset]);
          M3Buf.PutChar (s.buf, ':');
          M3Buf.PutInt  (s.buf, sym.size);
        END;
      END;
    END;
    M3Buf.PutText (s.buf, "_DATA\tENDS"); NL(s);

    (* dump the bss symbols *)
    NL (s);
    M3Buf.PutText (s.buf, "_BSS\tSEGMENT"); NL(s);
    FOR i := 0 TO t.n_syms-1 DO
      WITH sym = t.symbols[i] DO
        IF (sym.kind = SKind.Bss) AND NOT (sym.export) AND (sym.used) THEN
          M3ID.Put (s.buf, sym.id);
          M3Buf.PutChar (s.buf, '\t');
          M3Buf.PutText (s.buf, AlignDefn [sym.offset]);
          M3Buf.PutChar (s.buf, '\t');
          M3Buf.PutInt  (s.buf, sym.size);
          M3Buf.PutText (s.buf, " DUP (?)"); NL(s);
        END;
      END;
    END;
    M3Buf.PutText (s.buf, "_BSS\tENDS"); NL(s);

    (* sort the symbols & relocs by  (kind, offset) *)
    SortSymbols (s);
    SortRelocs (s);

    (* dump the segments *)
    s.next_sym   := 0;
    s.next_reloc := 0;
    s.next_line  := 0;
    FOR seg := FIRST (t.bytes) TO LAST (t.bytes) DO
      NL (s);
      M3Buf.PutText (s.buf, SegName [seg]);
      M3Buf.PutText (s.buf, "\tSEGMENT"); NL(s);
      offset := 0;
      seg_kind := SegToKind [seg];
      seg_size := t.size [seg];
      next_sym_offset := NextSymOffset (s, seg_kind);
      next_reloc_offset := NextRelocOffset (s, seg_kind);

      WHILE (offset < seg_size) DO
        (* make sure we didn't skip any relocations or symbols *)
        <*ASSERT next_sym_offset >= offset *>
        <*ASSERT next_reloc_offset >= offset *>

        (* check for a line number *)
        WHILE (s.next_line < t.n_lines)
          AND (t.lines[s.next_line].pc <= offset) DO
          M3Buf.PutText (s.buf, "; Line ");
          M3Buf.PutInt  (s.buf, t.lines[s.next_line].line);
          NL (s);
          INC (s.next_line);
        END;

        (* dump the symbols attached to this byte *)
        WHILE (next_sym_offset = offset) DO
          WITH sym = t.symbols[s.sym_map[s.next_sym]] DO
            M3ID.Put (s.buf, sym.id);
            M3Buf.PutText (s.buf, ":"); NL(s);
          END;
          INC (s.next_sym);
          next_sym_offset := NextSymOffset (s, seg_kind);
        END;

        IF (next_reloc_offset = offset) THEN
          (* there are relocations to do here *)
          PutWord (s, offset, seg);
          WHILE (next_reloc_offset = offset) DO
            WITH reloc = t.relocs[s.reloc_map [s.next_reloc]] DO
              M3Buf.PutText (s.buf, " + ");
              M3ID.Put (s.buf, t.symbols[reloc.target_sym].id);
            END;
            INC (s.next_reloc);
            next_reloc_offset := NextRelocOffset (s, seg_kind);
          END;
          INC (offset, 4);
        ELSIF (next_sym_offset >= offset+4)
          AND (next_reloc_offset >= offset + 4) THEN
          PutWord (s, offset, seg);
          INC (offset, 4);
        ELSE
          (* we can only dump a byte *)
          PutByte (s, offset, seg);
          INC (offset, 1);
        END;

        NL (s);
      END;

      (* dump any remaining symbols in this segment *)
      WHILE (next_sym_offset < LAST (INTEGER)) DO
        (********
        WITH sym = t.symbols[s.sym_map[s.next_sym]] DO
          M3ID.Put (s.buf, sym.id);
          M3Buf.PutText (s.buf, ":"); NL(s);
        END;
        *******)
        INC (s.next_sym);
        next_sym_offset := NextSymOffset (s, seg_kind);
      END;

      M3Buf.PutText (s.buf, SegName [seg]);
      M3Buf.PutText (s.buf, "\tENDS"); NL(s);
    END;

    (* write the object file trailer *)
    M3Buf.PutText (s.buf, "END"); NL(s);
    M3Buf.Flush (s.buf, wr);

    (* reset the object file for another run *)
    FOR i := FIRST (t.size) TO LAST (t.size) DO t.size[i] := 0; END;
    t.sym_map := NIL;
    t.n_relocs := 0;

    (* Give the collector a chance. *)
    s.t         := NIL;
    s.buf       := NIL;
    s.sym_map   := NIL;
    s.reloc_map := NIL;
  END Dump;

PROCEDURE NL (VAR s: DState) =
  BEGIN
    M3Buf.PutText (s.buf, Target.EOL);
  END NL;

PROCEDURE SortSymbols (VAR s: DState) =
  (* CONST KindName = ARRAY SKind OF TEXT { "text","data","bss","extern" }; *)
  VAR t := s.t;  n := t.n_syms;

  PROCEDURE Cmp (xa, xb: INTEGER): [-1..+1] =
    BEGIN
      WITH a = t.symbols [xa],  b = t.symbols [xb] DO
        IF (a.kind < b.kind) THEN RETURN -1 END;
        IF (a.kind > b.kind) THEN RETURN +1 END;
        IF (a.offset < b.offset) THEN RETURN -1 END;
        IF (a.offset > b.offset) THEN RETURN +1 END;
        RETURN 0;
      END;
    END Cmp;

  BEGIN
    s.sym_map := NEW (REF ARRAY OF INTEGER, n);
    FOR i := 0 TO n-1 DO s.sym_map[i] := i; END;
    IntArraySort.Sort (s.sym_map^, Cmp);
  END SortSymbols;

PROCEDURE SortRelocs (VAR s: DState) =
  VAR t := s.t;  n := t.n_relocs;

  PROCEDURE Cmp (xa, xb: INTEGER): [-1..+1] =
    VAR oa, ob: INTEGER;
    BEGIN
      WITH ra = t.relocs [xa],  rb = t.relocs [xb] DO
        WITH a = t.symbols[ra.src_sym],  b = t.symbols[rb.src_sym] DO
          IF (a.kind < b.kind) THEN RETURN -1 END;
          IF (a.kind > b.kind) THEN RETURN +1 END;
          oa := a.offset + ra.src_offset;
          ob := b.offset + rb.src_offset;
          IF (oa < ob) THEN RETURN -1 END;
          IF (oa > ob) THEN RETURN +1 END;
          RETURN 0;
        END;
      END;
    END Cmp;

  BEGIN
    s.reloc_map := NEW (REF ARRAY OF INTEGER, n);
    FOR i := 0 TO n-1 DO s.reloc_map[i] := i; END;
    IntArraySort.Sort (s.reloc_map^, Cmp);
  END SortRelocs;

PROCEDURE NextSymOffset (VAR s: DState;  kind: SKind): INTEGER =
  BEGIN
    IF (s.next_sym < NUMBER (s.sym_map^)) THEN
      WITH sym = s.t.symbols[s.sym_map [s.next_sym]] DO
        IF (sym.kind = kind) THEN RETURN sym.offset; END;
      END;
    END;
    RETURN LAST (INTEGER);
  END NextSymOffset;

PROCEDURE NextRelocOffset (VAR s: DState;  kind: SKind): INTEGER =
  BEGIN
    IF (s.next_reloc < NUMBER (s.reloc_map^)) THEN
      WITH reloc = s.t.relocs[s.reloc_map [s.next_reloc]] DO
        WITH sym = s.t.symbols[reloc.src_sym] DO
          IF (sym.kind = kind) THEN RETURN sym.offset + reloc.src_offset; END;
        END;
      END;
    END;
    RETURN LAST (INTEGER);
  END NextRelocOffset;

CONST
  HexDigit = ARRAY [0..15] OF CHAR {'0','1','2','3','4','5','6','7',
                                    '8','9','a','b','c','d','e','f'};

PROCEDURE PutWord (VAR s: DState;  offs: INTEGER;  seg: Seg) =
  VAR b := s.t.bytes[seg];  val: INTEGER;
  BEGIN
    M3Buf.PutText (s.buf, "\tDD\t0");
    FOR i := offs+3 TO offs BY -1 DO
      val := b[i];
      M3Buf.PutChar (s.buf, HexDigit [Word.RightShift (val, 4)]);
      M3Buf.PutChar (s.buf, HexDigit [Word.And (val, 16_f)]);
    END;
    M3Buf.PutChar (s.buf, 'h');
  END PutWord;

PROCEDURE PutByte (VAR s: DState;  offs: INTEGER;  seg: Seg) =
  VAR val := s.t.bytes[seg][offs];
  BEGIN
    M3Buf.PutText (s.buf, "\tDB\t0");
    M3Buf.PutChar (s.buf, HexDigit [Word.RightShift (val, 4)]);
    M3Buf.PutChar (s.buf, HexDigit [Word.And (val, 16_f)]);
    M3Buf.PutChar (s.buf, 'h');
  END PutByte;

BEGIN
END MasmObjFile.
