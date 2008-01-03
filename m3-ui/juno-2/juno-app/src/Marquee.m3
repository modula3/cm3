(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 10:15:31 PST 1994 by heydon                   *)
(*      modified on Mon Oct 19 10:22:55 PST 1992 by gnelson                  *)

MODULE Marquee;

IMPORT HVSplit, Split, TextVBT, Text, Font, Axis;

<* FATAL Split.NotAChild *>

REVEAL 
  Private = HVSplit.T BRANDED "Marquee.Private" OBJECT END;
  T = Public BRANDED "Marquee.T" OBJECT
    nameWidth, argWidth, width: INTEGER;
  OVERRIDES
    init := Init;
    erase := Erase;
    putName := PutName;
    newline := Newline;
    putArg := PutArg;
    remArg := RemArg;
  END;

CONST Blanks = ARRAY [0..79] OF CHAR{' ', ..};

PROCEDURE TextFromLen(n: INTEGER) : TEXT =
(* Return a text of "n" blanks. *)
  BEGIN
    RETURN Text.FromChars(SUBARRAY(Blanks, 0, MAX(0, n)))
  END TextFromLen;

PROCEDURE Init(m: T; height := 8; nameWidth := 10; 
    argWidth := 12; font := Font.BuiltIn): T =
  BEGIN
    EVAL HVSplit.T.init(m, Axis.T.Ver, adjustable := FALSE);
    m.argWidth := argWidth;
    m.nameWidth := nameWidth;
    m.width := argWidth + nameWidth + 1;
    FOR i := 1 TO height - 1 DO
      Split.AddChild(m, TextVBT.New(
        TextFromLen(m.width), halign := 0.0, fnt := font))
    END;
    Split.AddChild(m, TextVBT.New("", halign := 0.0, fnt := font));
    RETURN m
  END Init;

PROCEDURE Erase(m: T) =
  BEGIN
    TextVBT.Put(Split.Pred(m, NIL), "")
  END Erase;

PROCEDURE Newline(m: T) =
  VAR 
    v := Split.Succ(m, NIL);  
    last := Split.Pred(m, NIL);
    lastLine := TextVBT.Get(last); 
  BEGIN
    TextVBT.Put(last, 
      lastLine & TextFromLen(m.width - Text.Length(lastLine)));
    Split.Delete(m, v);
    TextVBT.Put(v, "");
    Split.Insert(m, last, v)
  END Newline;

PROCEDURE PutName(m: T; nm: TEXT) =
  VAR nmP := TextFromLen(m.nameWidth - Text.Length(nm)); BEGIN
    TextVBT.Put(Split.Pred(m, NIL), nmP & nm)
  END PutName;

PROCEDURE PutArg(m: T; nm: TEXT) =
  VAR last := Split.Pred(m, NIL); BEGIN
    TextVBT.Put(last, TextVBT.Get(last) & " " & nm)
  END PutArg;

PROCEDURE RemArg(m:T) =
  VAR 
    last := Split.Pred(m, NIL);
    lastLine := TextVBT.Get(last);
    i := Text.Length(lastLine) - 1;
  BEGIN
    WHILE Text.GetChar(lastLine, i) # ' ' DO DEC(i) END;
    TextVBT.Put(last, Text.Sub(lastLine, 0, i))
  END RemArg;
  
BEGIN
END Marquee.
   
