(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: FmtTable.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE FmtTable;
IMPORT Text;
IMPORT TextWr, Fmt;
IMPORT Wr, Thread;
<* FATAL Wr.Failure, Thread.Alerted *>

REVEAL
  T = Public BRANDED OBJECT
    wr: Wr.T;
    lineLen: INTEGER;
    lmargin: TEXT;
  OVERRIDES
    init := Init;
    putText := PutText;
    putInt := PutInt;
    toText := ToText;
  END;

PROCEDURE Init(self: T; lmargin: TEXT := "    "): T =
  BEGIN
    self.wr := TextWr.New();
    self.lineLen := 0;
    self.lmargin := lmargin;
    Wr.PutText(self.wr, self.lmargin);
    RETURN self;
  END Init;

PROCEDURE PutText(self: T; t: TEXT) =
  VAR
    len := Text.Length(t);
  BEGIN
    IF self.lineLen + len > 71 THEN
      self.lineLen := 0;
      Wr.PutText(self.wr, ",\n" & self.lmargin);
    END;
    IF self.lineLen # 0 THEN
      Wr.PutText(self.wr, ", ");
      INC(self.lineLen, 2);
    END;
    Wr.PutText(self.wr, t);
    INC(self.lineLen, len);
  END PutText; 

PROCEDURE PutInt(self: T; i: INTEGER) =
  BEGIN
    self.putText(Fmt.Int(i));
  END PutInt;

PROCEDURE ToText(self: T): TEXT =
  BEGIN
    RETURN TextWr.ToText(self.wr);
  END ToText;
    
BEGIN
END FmtTable.
