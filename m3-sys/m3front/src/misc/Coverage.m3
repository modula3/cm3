(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Coverage.m3                                           *)
(* Last modified on Tue Jan  3 14:45:34 PST 1995 by kalsow     *)
(*      modified on Wed Mar 13 01:32:19 1991 by muller         *)

MODULE Coverage;

IMPORT Text, M3ID, Value, Host, Scanner, Target, TInt, CG;

TYPE
  ProcHead = REF RECORD
    next   : ProcHead;
    proc   : Value.T;
    name   : TEXT;
    offset : INTEGER;
  END;

CONST
  MaxLine = 100000;

VAR
  Header  : TEXT;
  Trailer : TEXT;

TYPE
  LineSeen = {no, yes, generated};

VAR
  minLine : INTEGER := LAST (INTEGER);
  maxLine : INTEGER := FIRST (INTEGER);
  used    : REF ARRAY OF LineSeen := NIL;
  procs   : ProcHead := NIL;
  nProcs  : INTEGER := 0;
  tbl     : CG.Var := NIL;
  lines_offset : INTEGER;

PROCEDURE NoteLine () =
  VAR line: INTEGER;  file: TEXT;
  BEGIN
    IF (NOT Host.coverage) THEN RETURN END;
    Scanner.Here (file, line);
    IF (line > MaxLine) THEN RETURN END;
    IF NOT Text.Equal (file, Host.filename) THEN RETURN END;
    minLine := MIN (minLine, line);
    maxLine := MAX (maxLine, line);
    WHILE (used = NIL) OR (LAST (used^) < line) DO Expand () END;
    used[line] := LineSeen.yes;
  END NoteLine;

PROCEDURE Expand () =
  BEGIN
    IF (used = NIL) THEN
      used := NEW (REF ARRAY OF LineSeen, 100);
    ELSE
      WITH new = NEW (REF ARRAY OF LineSeen, 2 * NUMBER (used^)) DO
        FOR i := 0 TO LAST (used^) DO new[i] := used[i] END;
        used := new;
      END;
    END;
  END Expand;

PROCEDURE NoteProcedure (v: Value.T) =
  BEGIN
    IF (NOT Host.coverage) THEN RETURN END;
    WITH p = NEW (ProcHead) DO
      p.next   := procs;
      p.proc   := v;
      p.offset := -1;
      procs := p;
    END;
    INC (nProcs);
  END NoteProcedure;

PROCEDURE GenerateTables () =
  VAR
    nLines    := MAX (0, maxLine - minLine) + 1;
    l_header  := TLen (Header);
    l_fname   := TLen (Host.filename);
    l_trailer := TLen (Trailer);
    size    : INTEGER;
    p       : ProcHead;
    i, len  : INTEGER;
  BEGIN
    IF (NOT Host.coverage) THEN RETURN END;

    (* compute the size of the coverrage tables *)
    size := 0;
    INC (size, l_header * Target.Char.size);     (*header*)
    INC (size, Target.Integer.size);             (*timestamp*)
    INC (size, Target.Integer.size);             (*fileLen*)
    INC (size, l_fname * Target.Char.size);      (*file*)
    INC (size, Target.Integer.size);             (*firstLine*)
    INC (size, Target.Integer.size);             (*nLines*)
    INC (size, nLines * Target.Integer.size);    (*lines*)
    INC (size, Target.Integer.size);             (*nProcs*)
    p := procs;  i := 0;
    WHILE (p # NIL) DO
      IF (p.proc # NIL) THEN
        p.name := M3ID.ToText (Value.CName (p.proc));
        len := TLen (p.name);
        INC (size, Target.Integer.size);         (*len[p]*)
        INC (size, len * Target.Char.size);      (*pname[p]*)
        INC (size, Target.Integer.size);         (*cnt[p]*)
        INC (i);
      END;
      p := p.next;
    END;
    INC (size, l_trailer * Target.Char.size);  (*trailer*)

    (* allocate the variable *)
    tbl := CG.Declare_global (M3ID.NoID, size, Target.Address.align,
                              CG.Type.Struct, 0, exported := FALSE, init := TRUE);

    (* initialize the coverage tables *)
    CG.Begin_init (tbl);
    size := 0;

    CG.Init_chars (size, Header, FALSE);
    INC (size, l_header * Target.Char.size);     (*header*)

    (* CG.Init_int (size, Target.Integer.size, TInt.Zero, FALSE); *)
    INC (size, Target.Integer.size);             (*timestamp*)

    CG.Init_intt (size, Target.Integer.size, Text.Length (Host.filename), FALSE);
    INC (size, Target.Integer.size);             (*fileLen*)

    CG.Init_chars (size, Host.filename, FALSE);
    INC (size, l_fname * Target.Char.size);      (*file*)

    CG.Init_intt (size, Target.Integer.size, minLine, FALSE);
    INC (size, Target.Integer.size);             (*firstLine*)

    CG.Init_intt (size, Target.Integer.size, nLines, FALSE);
    INC (size, Target.Integer.size);             (*nLines*)

    lines_offset := size;
    FOR x := 0 TO nLines-1 DO
      IF (used # NIL) AND (used [x+minLine] # LineSeen.no)
        THEN len := 0;
        ELSE len := -1;
      END;
      CG.Init_intt (size, Target.Integer.size, len, FALSE);
      INC (size, Target.Integer.size);    (*lines[x]*)
    END;

    CG.Init_intt (size, Target.Integer.size, i, FALSE);
    INC (size, Target.Integer.size);             (*nProcs*)

    p := procs;
    WHILE (p # NIL) DO
      IF (p.proc # NIL) THEN
        len := TLen (p.name);

        CG.Init_intt (size, Target.Integer.size, Text.Length (p.name), FALSE);
        INC (size, Target.Integer.size);         (*len[p]*)

        CG.Init_chars (size, p.name, FALSE);
        INC (size, len * Target.Char.size);      (*pname[p]*)

        (* CG.Init_int (size, Target.Integer.size, 0, FALSE); *)
        p.offset := size;
        INC (size, Target.Integer.size);         (*cnt[p]*)
      END;
      p := p.next;
    END;

    CG.Init_chars (size, Trailer, FALSE);
    INC (size, l_trailer * Target.Char.size);    (*trailer*)

    CG.End_init (tbl);
  END GenerateTables;

PROCEDURE TLen (t: TEXT): INTEGER =
  VAR Grain := Target.Integer.size DIV Target.Char.size;
  BEGIN
    RETURN (Text.Length (t) + Grain - 1) DIV Grain * Grain;
  END TLen;

PROCEDURE CountLine () =
  VAR line, offset: INTEGER;  file: TEXT;
  BEGIN
    IF (NOT Host.coverage) THEN RETURN END;
    Scanner.Here (file, line);
    IF (line > MaxLine) THEN RETURN END;
    IF NOT Text.Equal (file, Host.filename) THEN RETURN END;
    IF used [line] = LineSeen.generated THEN RETURN END;
    <*ASSERT tbl # NIL*>
    offset := lines_offset + (line - minLine) * Target.Integer.size;
    CG.Load_int (Target.Integer.cg_type, tbl, offset);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.Add (Target.Word.cg_type);
    CG.Store_int (Target.Integer.cg_type, tbl, offset);
    used [line] := LineSeen.generated;
  END CountLine;

PROCEDURE CountProcedure (v: Value.T) =
  VAR p: ProcHead;
  BEGIN
    IF (NOT Host.coverage) THEN RETURN END;
    <*ASSERT tbl # NIL*>

    (* find the corresponding Proc *)
    p := procs;
    WHILE (p # NIL) DO
      IF (p.proc = v) THEN EXIT END;
      p := p.next;
    END;
    <*ASSERT p # NIL *>

    CG.Load_int (Target.Integer.cg_type, tbl, p.offset);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.Add (Target.Word.cg_type);
    CG.Store_int (Target.Integer.cg_type, tbl, p.offset);
  END CountProcedure;

PROCEDURE Reset () =
  BEGIN
    minLine := LAST (INTEGER);
    maxLine := FIRST (INTEGER);
    used    := NIL;
    procs   := NIL;
    nProcs  := 0;
    tbl     := NIL;
  END Reset;

BEGIN
  (* This initialization is designed so that the special header and trailer
     strings recognized by the coverage analyzer don't appear as literals
     in this module. *)
  Header  := "<<";
  Header  := Header & "<<Coverage 1.0";
  Trailer := "Coverage 1.0>>";
  Trailer := Trailer & ">>";
END Coverage.
