GENERIC MODULE VectorFmtLex(RF);
(*Copyright (c) 1996, m3na project*)

(*FROM NADefinitions IMPORT Error,Err;*)
IMPORT Rd, Wr, TextWr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
FROM FmtLexSupport IMPORT Precedence;

<*UNUSED*>
CONST Module = "VectorFmt.";

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR wr := TextWr.New();
  BEGIN
    Wr.PutText(wr, "V" & F.Int(NUMBER(x^)) & "{");
    FOR i := FIRST(x^) TO LAST(x^) DO
      Wr.PutText(wr, F.Pad(RF.Fmt(x[i], style.elemStyle), style.width));
      IF i # LAST(x^) THEN Wr.PutText(wr, ", "); END;
    END;
    Wr.PutText(wr, "}\n");
    RETURN TextWr.ToText(wr);
  END Fmt;

PROCEDURE Tex (         x     : T;
               READONLY style       := TexStyle{}; <*UNUSED*>
                        within      := Precedence.sum         ): TEXT
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    wr        := TextWr.New();
    sep: TEXT;
  BEGIN
    IF TexFlag.vertical IN style.flags THEN
      sep := " \\\\\n";
      Wr.PutText(wr, "\\left(\\begin{array}{c}\n");
    ELSE
      sep := style.sep;
      Wr.PutText(wr, "\\left(");
    END;
    FOR i := FIRST(x^) TO LAST(x^) DO
      Wr.PutText(wr, RF.Tex(x[i], style.elemStyle, Precedence.sum));
      IF i # LAST(x^) THEN Wr.PutText(wr, sep); END;
    END;
    IF TexFlag.vertical IN style.flags THEN
      Wr.PutText(wr, "\\end{array}\\right)\n");
    ELSE
      Wr.PutText(wr, "\\right)\n");
    END;
    RETURN TextWr.ToText(wr);
  END Tex;


PROCEDURE Lex (rd: Rd.T; READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  BEGIN
  END Lex;

BEGIN
END VectorFmtLex.
