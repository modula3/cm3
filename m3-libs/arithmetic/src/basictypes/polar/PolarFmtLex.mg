GENERIC MODULE PolarFmtLex(R, RF);
(*Arithmetic for Modula-3, see doc for details*)

IMPORT Rd, Thread;
(*IMPORT Fmt AS F;*)
IMPORT Lex AS L;
IMPORT FloatMode;
IMPORT FmtLexSupport AS FSup;
FROM FmtLexSupport IMPORT Precedence;

<*UNUSED*>
CONST Module = "PolarFmtLex.";

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}): TEXT =
  VAR t: TEXT;
  BEGIN
    t := "Polar{radius:=" & RF.Fmt(x.radius, style.elemStyle) & ","
           & "angle:=" & RF.Fmt(x.angle, style.elemStyle) & "}";
    RETURN t;
  END Fmt;

PROCEDURE Tex (READONLY x     : T;
               READONLY style       := TexStyle{};
                        within      := Precedence.Sum): TEXT =
  VAR t: TEXT;
  BEGIN
    IF R.IsZero(x.radius) OR R.IsZero(x.angle) THEN
      t := RF.Tex(x.radius, style.elemStyle, within);
    ELSE
      t := FSup.Parenthesize(
             RF.Tex(x.radius, style.elemStyle, Precedence.Product)
               & " \\cdot e^{"
               & RF.Tex(x.angle, style.elemStyle, Precedence.Sum) & " i}",
             Precedence.Product, within);
    END;
    RETURN t;
  END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  VAR z: T;
  BEGIN
    z.radius := RF.Lex(rd);
    FSup.AssertChar(rd, style.sep);
    z.angle := RF.Lex(rd);
    RETURN z;
  END Lex;

BEGIN
END PolarFmtLex.
