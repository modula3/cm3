INTERFACE BigIntegerFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Formatting of integers of arbitrary size

*)

(*==========================*)

IMPORT BigIntegerBasic AS B, Lex AS L, FloatMode, Rd, Thread;


TYPE
  T = B.T;
  Base = [2..16];

PROCEDURE Lex(rd: Rd.T; defaultBase: Base := 10): INTEGER
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};
PROCEDURE Scan(txt: TEXT; defaultBase: Base := 10): INTEGER
  RAISES {L.Error, FloatMode.Trap};
PROCEDURE Fmt(READONLY x : T; base: Base := 10): TEXT;

(*==========================*)
END BigIntegerFmtLex.
