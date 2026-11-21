(* $Id: CSVParse.m3,v 1.2 2012/10/15 08:26:01 mika Exp $ *)

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

PROCEDURE Cell(t : T; handleQuotes : BOOLEAN) : TEXT RAISES { EndOfLine } =
  VAR
    res : TEXT;
  BEGIN
    IF t.cellB(res, handleQuotes) THEN RETURN res ELSE RAISE EndOfLine END
  END Cell;

PROCEDURE CellB(t : T; VAR cell : TEXT; handleQuotes : BOOLEAN) : BOOLEAN =
  VAR
    b, e : CARDINAL;
    
  CONST
    Seps = SET OF CHAR { '\r', ',' };
    DQ = '"';
  BEGIN
    LOOP
      IF Text.Length(t.line) <= t.q THEN RETURN FALSE END;

      (* we have just consumed a field, at start of next *)
      
      WITH c = Text.GetChar(t.line,t.q) DO
        IF handleQuotes AND c = DQ THEN
          INC(t.p); INC(t.q); (* skip DQ *)
          
          (* handle DQ at EOL *)
          IF t.p = t.n THEN
            t.last := "";
            cell := t.last;
            RETURN TRUE
          END;
          LOOP
            IF t.q = t.n OR Text.GetChar(t.line, t.q) = DQ THEN
              (* at EOL or closing quote *)
              t.last := Text.Sub(t.line, t.p, t.q-t.p);
              cell := t.last;

              (* skip DQ *)
              t.p := t.q+1;

              (* skip sep if any *)
              IF t.p <= t.n-1 AND Text.GetChar(t.line, t.p) IN Seps THEN
                t.p := t.p + 1
              END;
              t.q := t.p;
                
              RETURN TRUE
            END;

            (* just gobble to next DQ or EOL *)
            INC(t.q)
          END
        END;
        
        IF c IN Seps THEN
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
      END
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
