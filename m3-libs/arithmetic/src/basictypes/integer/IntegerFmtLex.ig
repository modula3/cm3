GENERIC INTERFACE IntegerFmtLex(I);
(*Copyright (c) 1996, m3na project*)

IMPORT Wr, Thread;
IMPORT Fmt AS F;
FROM FmtLexSupport IMPORT Precedence;

(*==========================*)
TYPE T = I.T;

TYPE FmtStyle = RECORD base: F.Base := 10;  END;

<*INLINE*>
PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT;

PROCEDURE FmtArray (READONLY a        : ARRAY OF T;
                             style                   := FmtStyle{};
                             cellwidth: CARDINAL     := 4;
                             linewidth: CARDINAL     := 60          ): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

TYPE TexStyle = RECORD base: F.Base := 10;  END;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}; within := Precedence.sum):
  TEXT;

(*==========================*)
END IntegerFmtLex.
