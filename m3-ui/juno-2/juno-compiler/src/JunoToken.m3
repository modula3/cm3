(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar 17 14:28:29 PST 1995 by heydon                   *)
(*      modified on Fri Aug  7 21:54:06 PDT 1992 by myers                    *)

MODULE JunoToken;

IMPORT Atom, JunoValue, TextWr, Wr;

CONST
  Ops = ARRAY OF TEXT{
    ";", ".", ",", ":", "(", ")", "{", "}", "[", "]", ":=", "::", "|",
    "->", "~", "=", "#", "<", ">", "<=", ">=", "+", "-", "*", "/", "&"};
  KeyWds = ARRAY OF TEXT{
    "MODULE", "PRIVATE", "END", "IMPORT", "CONST", "VAR", "PRED", "FUNC",
    "PROC", "UI", "IS", "SKIP", "ABORT", "IF", "FI", "DO", "OD",
    "SAVE", "IN", "NIL", "TRUE", "FALSE", "OR", "AND", "NOT",
    "E", "CONG", "PARA", "HOR", "VER", "REL", "DIV", "MOD"};
  ResvdIds = ARRAY OF TEXT{
    "REAL", "TEXT", "PAIR", "INT", "FLOOR", "CEILING", "ROUND", "ABS", "SIN",
    "COS", "LN", "EXP", "CAR", "CDR", "MAX", "MIN", "ATAN"};

PROCEDURE Copy(t: T): T =
  BEGIN
    RETURN NEW(T, kind := t.kind, val := t.val, num := t.num)
  END Copy;

PROCEDURE Unparse(t: JunoValue.T): TEXT =
  <*FATAL Wr.Failure*>
  VAR wr := TextWr.New(); BEGIN
    JunoValue.Unparse(wr, t);
    RETURN TextWr.ToText(wr)
  END Unparse;

PROCEDURE ToName(t: T): TEXT =
  BEGIN
    CASE t.kind OF
    | Kind.LitReal =>
        RETURN "Real(" & Unparse(JunoValue.RefReal(t.num)) & ")"
    | Kind.LitText =>
        RETURN "Text(" & Unparse(t.val) & ")"
    | Kind.Id =>
        RETURN "Id(" & Atom.ToText(NARROW(t.val, Atom.T)) & ")"
    | FIRST(Op)..LAST(Op) =>
        RETURN "Op(" & Ops[ORD(t.kind) - ORD(FIRST(Op))] & ")"
    | FIRST(KeyWd)..LAST(KeyWd) =>
        RETURN "KeyWd(" & KeyWds[ORD(t.kind) - ORD(FIRST(KeyWd))] & ")"
    | FIRST(ResvdId)..LAST(ResvdId) =>
        RETURN "ResvdId(" & ResvdIds[ORD(t.kind) - ORD(FIRST(ResvdId))] & ")"
    | Kind.Comment =>
        RETURN "Comment(" & NARROW(t.val, TEXT) & ")"
    | Kind.EndMarker =>
        RETURN "EndMarker()"
    | Kind.Unknown =>
        RETURN "?()"
    END
  END ToName;

PROCEDURE ToText(t: T): TEXT =
  BEGIN
    CASE t.kind OF
    | Kind.LitReal =>
        RETURN Unparse(JunoValue.RefReal(t.num))
    | Kind.LitText =>
        IF t.val = NIL
          THEN RETURN "<Text-Literal>"
          ELSE RETURN Unparse(t.val)
        END
    | Kind.Id =>
        IF t.val = NIL
          THEN RETURN "<Identifier>"
          ELSE RETURN Atom.ToText(NARROW(t.val, Atom.T))
        END
    | Kind.Comment =>
        IF t.val = NIL
          THEN RETURN "<Comment>"
          ELSE RETURN NARROW(t.val, TEXT)
        END
    | FIRST(Op)..LAST(Op) =>
        RETURN Ops[ORD(t.kind) - ORD(FIRST(Op))]
    | FIRST(KeyWd)..LAST(KeyWd) =>
        RETURN KeyWds[ORD(t.kind) - ORD(FIRST(KeyWd))]
    | FIRST(ResvdId)..LAST(ResvdId) =>
        RETURN ResvdIds[ORD(t.kind) - ORD(FIRST(ResvdId))]
    | Kind.EndMarker =>
        RETURN "<End-Of-File>"
    | Kind.Unknown =>
        RETURN "??UNKNOWN??"
    END
  END ToText;

BEGIN END JunoToken.
