INTERFACE BigIntegerFmtLex;
(*Copyright (c) 1996, m3na project*)

IMPORT BigInteger AS B;
IMPORT Fmt AS F;

FROM FmtLexSupport IMPORT Precedence;

(*==========================*)

TYPE T = B.T;

TYPE FmtStyle = RECORD base: F.Base := 10;  END;

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}): TEXT;

TYPE TexStyle = FmtStyle;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}; within := Precedence.sum):
  TEXT;

(*==========================*)
END BigIntegerFmtLex.
