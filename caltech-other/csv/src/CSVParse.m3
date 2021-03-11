(* $Id$ *)

MODULE CSVParse;
IMPORT Rd, Lex, FloatMode, Text, Scan, Thread;
IMPORT Debug, Fmt;

REVEAL
  T = Public BRANDED Brand OBJECT
    rd       : Rd.T;
    n, p, q  : CARDINAL;
    line     : TEXT;
    lNo      : CARDINAL;
    last     : TEXT;
    
  OVERRIDES
    init      := Init;
    startLine := StartLine;
    cell      := Cell;
    cellB     := CellB;
    int       := Int;
    lr        := LR;
    whatLine  := WhatLine;
    lastCell  := LastCell;
    lastLine  := LastLine;

  END;    

PROCEDURE LastLine(t : T) : TEXT = BEGIN RETURN t.line END LastLine;

PROCEDURE Init(t : T; rd : Rd.T) : T = 
  BEGIN t.rd := rd; t.lNo := 0; RETURN t END Init;

PROCEDURE StartLine(t : T) 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    INC(t.lNo);
    t.line := Rd.GetLine(t.rd); t.p := 0; t.q := 0; t.n := Text.Length(t.line);
    IF Debug.GetLevel() >= 20 THEN
      Debug.Out("line " & Fmt.Int(t.lNo) & " \"" & t.line & "\"")
    END
  END StartLine;

PROCEDURE WhatLine(t : T) : CARDINAL = BEGIN RETURN t.lNo END WhatLine;

PROCEDURE Cell(t : T) : TEXT RAISES { EndOfLine } =
  VAR
    res : TEXT;
  BEGIN
    IF t.cellB(res) THEN RETURN res ELSE RAISE EndOfLine END
  END Cell;

PROCEDURE CellB(t : T; VAR cell : TEXT) : BOOLEAN =
  VAR
    b, e : CARDINAL;
    
  CONST
    Seps = SET OF CHAR { '\r', ',' };
    
  BEGIN
    LOOP
      IF Text.Length(t.line) <= t.q THEN RETURN FALSE END;

      IF Text.GetChar(t.line,t.q) IN Seps THEN
        b := t.p; e := t.q;

        t.p := t.q+1;
        t.q := t.p;
        EXIT
      ELSIF t.q = t.n-1 THEN
        b := t.p; e := t.q+1;

        t.p := t.q+1;
        t.q := t.p;
        EXIT
      END;
      INC(t.q)
    END;
    t.last := Text.Sub(t.line, b, e-b);
    cell := t.last;
    RETURN TRUE
  END CellB;

PROCEDURE LastCell(t : T) : TEXT = BEGIN RETURN t.last END LastCell;

PROCEDURE Int(t : T) : INTEGER RAISES { EndOfLine, FloatMode.Trap, Lex.Error } =
  BEGIN RETURN Scan.Int(t.cell()) END Int;

PROCEDURE LR(t : T) : LONGREAL RAISES { EndOfLine, FloatMode.Trap, Lex.Error } =
  BEGIN RETURN Scan.LongReal(t.cell()) END LR;

BEGIN END CSVParse.
