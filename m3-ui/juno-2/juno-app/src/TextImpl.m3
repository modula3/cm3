(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 20 21:06:58 PDT 1995 by heydon                   *)

MODULE TextImpl;

IMPORT ExternalProc;
FROM ExternalProc IMPORT Closure, Bind;
IMPORT JunoScope;
IMPORT JunoArgs;
IMPORT Text, Atom, Fmt;

VAR (*CONST*)
  TextAtom := Atom.FromText("Text");
  Cat      := Atom.FromText("Cat");
  Length   := Atom.FromText("Length");
  Sub      := Atom.FromText("Sub");
  GetChar  := Atom.FromText("GetChar");
  FromChar := Atom.FromText("FromChar");
  FromNum  := Atom.FromText("FromNum");
  FindCh   := Atom.FromText("FindChar");
  FindChR  := Atom.FromText("FindCharR");

PROCEDURE New(): JunoScope.Mod =
  VAR
    scp := JunoScope.New(NIL, size := 6);
    res := NEW(JunoScope.Mod, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(TextAtom, scp);
    Bind(Cat,	   NEW(Closure, invoke := CatProc),      in := 2, out := 1);
    Bind(Length,   NEW(Closure, invoke := LengthProc),   in := 1, out := 1);
    Bind(Sub,      NEW(Closure, invoke := SubProc),      in := 3, out := 1);
    Bind(GetChar,  NEW(Closure, invoke := GetCharProc),  in := 2, out := 1);
    Bind(FromChar, NEW(Closure, invoke := FromCharProc), in := 1, out := 1);
    Bind(FromNum,  NEW(Closure, invoke := FromNumProc),  in := 2, out := 1);
    Bind(FindCh,   NEW(Closure, invoke := FindChProc),   in := 2, out := 1);
    Bind(FindChR,  NEW(Closure, invoke := FindChRProc),  in := 2, out := 1);
    RETURN res
  END New;

PROCEDURE CatProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    t := JunoArgs.ReadText(2, err);
    u := JunoArgs.ReadText(1, err);
  BEGIN
    IF err THEN RETURN FALSE END;
    JunoArgs.WriteText(3, Text.Cat(t, u));
    RETURN TRUE
  END CatProc;

PROCEDURE LengthProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); BEGIN
    IF err THEN RETURN FALSE END;
    JunoArgs.WriteInt(2, Text.Length(t));
    RETURN TRUE
  END LengthProc;

PROCEDURE SubProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    t := JunoArgs.ReadText(3, err);
    i := JunoArgs.ReadInt(2, err);
    n := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND i >= 0 AND n >=0 THEN
      JunoArgs.WriteText(4, Text.Sub(t, i, n));
      RETURN TRUE
    END;
    RETURN FALSE
  END SubProc;

PROCEDURE GetCharProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    t := JunoArgs.ReadText(2, err);
    n := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND n >= 0 AND n < Text.Length(t) THEN
      JunoArgs.WriteInt(3, ORD(Text.GetChar(t, n)));
      RETURN TRUE
    END;
    RETURN FALSE
  END GetCharProc;

PROCEDURE FromCharProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR err := FALSE; ch := JunoArgs.ReadInt(1, err); BEGIN
    IF NOT err AND ch >= 0 AND ch < 256 THEN
      JunoArgs.WriteText(2, Text.FromChar(VAL(ch, CHAR)));
      RETURN TRUE
    END;
    RETURN FALSE
  END FromCharProc;

PROCEDURE FromNumProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    v := JunoArgs.ReadReal(2, err);
    pr := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND pr >= 0 THEN
      JunoArgs.WriteText(3, Fmt.Real(FLOAT(v, REAL), prec := pr));
      RETURN TRUE
    END;
    RETURN FALSE
  END FromNumProc;

PROCEDURE FindChProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    t := JunoArgs.ReadText(2, err);
    ch := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND ch >= 0 AND ch < 256 THEN
      JunoArgs.WriteInt(3, Text.FindChar(t, VAL(ch, CHAR)));
      RETURN TRUE
    END;
    RETURN FALSE
  END FindChProc;

PROCEDURE FindChRProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    t := JunoArgs.ReadText(2, err);
    ch := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND ch >= 0 AND ch < 256 THEN
      JunoArgs.WriteInt(3, Text.FindCharR(t, VAL(ch, CHAR)));
      RETURN TRUE
    END;
    RETURN FALSE
  END FindChRProc;

BEGIN
END TextImpl.
