INTERFACE BigIntegerFmtLex;
(* Arithmetic for Modula-3, see doc for details *)

IMPORT BigInteger AS B;
IMPORT Rd, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;

FROM FmtLexSupport IMPORT Precedence;


TYPE T = B.T;

TYPE FmtStyle = RECORD base: F.Base := 10;  END;

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}; ): TEXT;

TYPE TexStyle = FmtStyle;

PROCEDURE Tex
  (x: T; READONLY style := TexStyle{}; within := Precedence.sum; ): TEXT;

TYPE LexStyle = RECORD END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};

END BigIntegerFmtLex.
