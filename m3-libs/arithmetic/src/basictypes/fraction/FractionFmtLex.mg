GENERIC MODULE FractionFmtLex(RF);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Rd, Thread;
(*IMPORT Fmt AS F;*)
IMPORT Lex AS L;
IMPORT FloatMode;
(*IMPORT Arithmetic AS Arith;*)
IMPORT FmtLexSupport AS FSup;
FROM FmtLexSupport IMPORT Precedence;

<* UNUSED *>
CONST
  Module = "FractionFmtLex.";

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}; ): TEXT =
  BEGIN
    RETURN "Fraction{n:=" & RF.Fmt(x.n, style.elemStyle) & "," & "d:="
             & RF.Fmt(x.d, style.elemStyle) & "}";
  END Fmt;

PROCEDURE Tex (READONLY x     : T;
               READONLY style       := TexStyle{};
                        within      := Precedence.Sum; ): TEXT =
  VAR t: TEXT;
  BEGIN
    IF TexFlag.Fraction IN style.flags THEN
      t := "\\frac{" & RF.Tex(x.n, style.elemStyle, Precedence.Sum) & "}{"
             & RF.Tex(x.d, style.elemStyle, Precedence.Sum) & "}";
    ELSE
      t := RF.Tex(x.n, style.elemStyle, Precedence.Product) & " / "
             & RF.Tex(x.d, style.elemStyle, Precedence.Power);
    END;
    RETURN FSup.Parenthesize(t, Precedence.Product, within);
  END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  VAR z: T;
  BEGIN
    z.n := RF.Lex(rd);
    FSup.AssertChar(rd, style.sep);
    z.d := RF.Lex(rd);
    RETURN z;
  END Lex;

BEGIN
END FractionFmtLex.
