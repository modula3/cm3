GENERIC INTERFACE IntegerFmtLex(I);
(*Copyright (c) 1996, m3na project

Abstract: Generic formatting and parsing of integer types

2/17/96  Harry George    Initial version
*)

IMPORT Thread, Wr, Fmt AS F;
FROM FmtLexSupport IMPORT Precedence;

(*==========================*)
TYPE
  T = I.T;
  FmtStyle = RECORD base : F.Base := 10; END;
  TexStyle = RECORD base : F.Base := 10; END;

<*INLINE*>
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT;

PROCEDURE FmtArray(READONLY a:ARRAY OF T;
                   style     :=FmtStyle{};
                   cellwidth :CARDINAL:=4;
                   linewidth :CARDINAL:=60):TEXT RAISES {Thread.Alerted, Wr.Failure};

PROCEDURE Tex (x : T; READONLY style := TexStyle{}; within := Precedence.sum) : TEXT;

(*==========================*)
END IntegerFmtLex.
