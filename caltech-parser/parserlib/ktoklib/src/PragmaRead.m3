(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PragmaRead.m3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

MODULE PragmaRead;
IMPORT Rd;
IMPORT Pragma;
IMPORT PragmaTbl;
IMPORT CharRange;
IMPORT Text;
IMPORT Thread;
IMPORT FileRdErr;
(* IMPORT Term, Fmt; *)
FROM CharCodes IMPORT Q;
<* FATAL Rd.Failure, Thread.Alerted *>
REVEAL
  T = Public BRANDED OBJECT
    tab: PragmaTbl.T;
  OVERRIDES
    init := Init;
    add := Add;
    apply := Apply;
    error := Error;
  END;

PROCEDURE GetPragNames(tab: PragmaTbl.T): TEXT =
  VAR
    iterate := tab.iterate();
    result: TEXT := "";
    key: TEXT;
    prag: Pragma.T;
  BEGIN
    WHILE iterate.next(key, prag) DO
      IF NOT Text.Equal(key, "") THEN
        result := result & " " & key;
      END;
    END;
    RETURN result;
  END GetPragNames;

PROCEDURE Init(self: T): T =
  BEGIN
    self.tab := NEW(PragmaTbl.Default).init();
    RETURN self;
  END Init;

PROCEDURE Add(self: T; p: Pragma.T; name: TEXT) =
  BEGIN
    EVAL self.tab.put(name, p);
  END Add;

PROCEDURE ApplyOne(prag: Pragma.T; rd: Rd.T) =
  VAR
    pos: INTEGER;
  PROCEDURE LineStarts(c: CHAR): BOOLEAN =
    VAR
      peekLine: TEXT;
    BEGIN
      pos := Rd.Index(rd);
      TRY
        peekLine := CharRange.FilterText(Rd.GetLine(rd));
      EXCEPT
      | Rd.EndOfFile => RETURN c = '\000';
      END;
      IF Text.Length(peekLine) = 0 THEN
        RETURN c = '\000';
      ELSIF Text.GetChar(peekLine, 0) = c THEN
        Rd.Seek(rd, pos);
        TRY
          peekLine := Rd.GetLine(rd);
        EXCEPT
        | Rd.EndOfFile => <* ASSERT FALSE *>
        END;
        Rd.Seek(rd, pos + Text.FindChar(peekLine, c)+1);
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END LineStarts;
  BEGIN
    IF LineStarts('{') THEN
      WHILE NOT LineStarts('}') DO
        Rd.Seek(rd, pos);
        IF NOT LineStarts('\000') THEN
          Rd.Seek(rd, pos);
          prag.do(rd);
        END;
        IF pos = Rd.Index(rd) THEN
          Rd.Seek(rd, pos+1); (* avoid infinite loop *)
        END;
      END;
    ELSE
      Rd.Seek(rd, pos);
      prag.do(rd);
    END;
  END ApplyOne;


PROCEDURE Apply(self: T; rd: Rd.T) =
  VAR
    pos, len: INTEGER;
    key: TEXT;
    prag: Pragma.T;
  BEGIN
    TRY
      WHILE TRUE DO
        pos := Rd.Index(rd);
        CASE Rd.GetChar(rd) OF 
        | '%' =>
          WHILE Rd.GetChar(rd) IN CharRange.Letter DO
          END;
          len := Rd.Index(rd) - pos - 1;
          Rd.Seek(rd, pos);
          key := Rd.GetText(rd, len);
          IF self.tab.get(key, prag) THEN
            ApplyOne(prag, rd);
          ELSE
            self.error(rd, "Found " & Q(key) &
              ", which is not" & GetPragNames(self.tab));
          END;
        | '\t', ' ', '\n' =>
        ELSE
          Rd.Seek(rd, pos);          
          IF self.tab.get("", prag) THEN
            prag.do(rd);
          ELSE
            self.error(rd, "missing `%'");
          END;
        END;
      END;
    EXCEPT
      Rd.EndOfFile =>
    END;
  END Apply;
  
PROCEDURE Error(<*UNUSED*>self: T; rd: Rd.T; message: TEXT) =
  BEGIN
    FileRdErr.E(rd, message);
  END Error;

BEGIN
END PragmaRead.
