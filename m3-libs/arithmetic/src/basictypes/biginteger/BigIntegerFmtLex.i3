INTERFACE BigIntegerFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Formatting of integers of arbitrary size

*)

IMPORT (*Lex AS L, FloatMode, Rd, Thread,*)
       BigInteger AS B, Fmt AS F;

FROM FmtLexSupport IMPORT Precedence;

(*==========================*)

TYPE
  T = B.T;
  FmtStyle = RECORD base : F.Base := 10; END;
  TexStyle = FmtStyle;

(*
PROCEDURE Lex(rd: Rd.T; defaultBase: Base := 10): INTEGER
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};
PROCEDURE Scan(txt: TEXT; defaultBase: Base := 10): INTEGER
  RAISES {L.Error, FloatMode.Trap};
*)
PROCEDURE Fmt (READONLY x : T; READONLY style := FmtStyle{}) : TEXT;

PROCEDURE Tex (x : T; READONLY style := TexStyle{}; within := Precedence.sum) : TEXT;

(*==========================*)
END BigIntegerFmtLex.
