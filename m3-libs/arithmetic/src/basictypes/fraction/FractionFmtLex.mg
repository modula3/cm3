GENERIC MODULE FractionFmtLex(RF);
(*Copyright (c) 1996, m3na project

   Abstract: Formatting and parsing fraction numbers *)

IMPORT Rd, Wr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
(*FROM NADefinitions IMPORT Error,Err;*)
FROM FmtLexSupport IMPORT Precedence, Parenthesize;

<*UNUSED*>
CONST Module = "FractionFmtLex.";

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}): TEXT =
  VAR t: TEXT;
  BEGIN
    t := "Fraction{n:=" & RF.Fmt(x.n, style.elemStyle) & "," & "d:="
           & RF.Fmt(x.d, style.elemStyle) & "}";
    RETURN t;
  END Fmt;

PROCEDURE Tex (READONLY x     : T;
               READONLY style       := TexStyle{};
                        within      := Precedence.sum): TEXT =
  VAR t: TEXT;
  BEGIN
    IF TexFlag.fraction IN style.flags THEN
      t := "\\frac{" & RF.Tex(x.n, style.elemStyle, Precedence.sum) & "}{"
             & RF.Tex(x.d, style.elemStyle, Precedence.sum) & "}";
    ELSE
      t := RF.Tex(x.n, style.elemStyle, Precedence.product) & " / "
             & RF.Tex(x.d, style.elemStyle, Precedence.power);
    END;
    RETURN Parenthesize(t, Precedence.product, within);
  END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
END FractionFmtLex.
