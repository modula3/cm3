(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE Override;
IMPORT Rd;
IMPORT TextTextTbl;
IMPORT TextBooleanTbl;
IMPORT TextWr, Wr;
IMPORT CharCodes;
IMPORT FileRdErr;
IMPORT Thread;
<* FATAL Wr.Failure, Thread.Alerted *>
REVEAL
  T = Public BRANDED OBJECT
    wr: Wr.T;
    which: TextBooleanTbl.T; (* name -> overridden? *)
    specs: TextTextTbl.T;
  OVERRIDES
    init := Init;
    add := Add;
    importRemaining := ImportRemaining;
    getProcAssignText := GetProcAssignText;
    getText := GetText;
    overridden := Overridden;
  END;
PROCEDURE Overridden(self: T; name: TEXT): BOOLEAN =
  VAR
    result, found: BOOLEAN;
  BEGIN
    found := self.which.get(name, result);
    <* ASSERT found *>
    RETURN result;
  END Overridden;

PROCEDURE Init(self: T; specs: TextTextTbl.T): T =
  VAR
    iter := specs.iterate();
    key, value: TEXT;
  BEGIN
    self.wr := TextWr.New();
    self.which := NEW(TextBooleanTbl.Default).init();
    self.specs := specs;
    WHILE iter.next(key, value) DO
      EVAL self.which.put(key, FALSE);
    END;
    RETURN self;
  END Init;

PROCEDURE Add(self: T; name, body: TEXT; rd: Rd.T) =
  BEGIN
    Wr.PutText(self.wr, body);
    IF NOT self.which.put(name, TRUE) THEN
      FileRdErr.E(rd, CharCodes.Q(name) & " is undefined; cannot override");
    END;
  END Add;

PROCEDURE ImportRemaining(self: T) =
  VAR
    iter := self.which.iterate();
    name, mn: TEXT;
    overridden: BOOLEAN;
  BEGIN
    WHILE iter.next(name, overridden) DO
      IF NOT overridden THEN
        EVAL self.specs.get(name, mn);
        Wr.PutText(self.wr, "  " & name & " = " & mn & "." & name & ";\n");
      END;
    END;
  END ImportRemaining;

PROCEDURE GetProcAssignText(self: T): TEXT =
  VAR
    iter := self.which.iterate();
    name, form: TEXT;
    overridden: BOOLEAN;
    wr := TextWr.New();
  BEGIN
    WHILE iter.next(name, overridden) DO
      IF overridden THEN
        EVAL self.specs.get(name, form);
        Wr.PutText(wr, "    " & name & " := Proc_" & name & ";\n");
      END;
    END;
    RETURN TextWr.ToText(wr);
  END GetProcAssignText;

PROCEDURE GetText(self: T): TEXT =
  BEGIN
    RETURN TextWr.ToText(self.wr);
  END GetText;

BEGIN
END Override.
