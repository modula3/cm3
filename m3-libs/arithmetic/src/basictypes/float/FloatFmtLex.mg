GENERIC MODULE FloatFmtLex(FI);
(*Copyright (c) 1996, m3na project*)

IMPORT Rd, Thread;
(*IMPORT Fmt AS F;*)
IMPORT Lex AS L;
IMPORT FloatMode;
IMPORT Text AS Tx;
IMPORT TextExtras AS Txe;
FROM FmtLexSupport IMPORT Precedence, Parenthesize;

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT =
  BEGIN
    RETURN FI.Fmt(x, style.style, style.prec, style.literal);
  END Fmt;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}; within: Precedence):
  TEXT =
  VAR
    result           := FI.Fmt(x, style.style, style.prec);
    epos  : CARDINAL := 0;
  BEGIN
    IF Txe.FindSub(result, "e", epos) THEN
      RETURN Parenthesize(
               Tx.Sub(result, 0, epos) & "\\cdot 10^{"
                 & Tx.Sub(result, epos + 1, Tx.Length(result) - epos - 1)
                 & "}", Precedence.product, within);
    ELSE
      RETURN result;
    END;
  END Tex;


PROCEDURE Lex (rd: Rd.T;  <*UNUSED*>READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN FI.Lex(rd);
  END Lex;

BEGIN
END FloatFmtLex.
