GENERIC MODULE IntegerFmtLex(I);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Integers

   2/17/96 Harry George Initial version *)
IMPORT Rd, Wr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
IMPORT TextWr;
FROM FmtLexSupport IMPORT Precedence;


<* UNUSED *>
CONST
  Module = "IntegerFmtLex.";

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}; ): TEXT =
  BEGIN
    RETURN F.Int(x, style.base);
  END Fmt;

PROCEDURE FmtArray (READONLY a        : ARRAY OF I.T;
                             style                     := FmtStyle{};
                             cellwidth: CARDINAL       := 4;
                             linewidth: CARDINAL       := 60;         ):
  TEXT RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    wr        := TextWr.New();
    n         := NUMBER(a);
    n1        := 0;
    nn        := n - 1;
    currwidth := 0;

  BEGIN
    Wr.PutText(wr, "A" & Fmt(n) & "{");
    FOR i := n1 TO nn DO
      Wr.PutText(wr, F.Pad(Fmt(a[i], style), cellwidth));
      IF i # nn THEN Wr.PutText(wr, ", "); END;
      INC(currwidth, cellwidth + 2);
      IF currwidth > linewidth THEN
        Wr.PutText(wr, "\n   ");
        currwidth := 0;
      END;
    END;
    Wr.PutText(wr, "}\n");
    RETURN TextWr.ToText(wr);
  END FmtArray;

PROCEDURE Tex
  (x: T; READONLY style := TexStyle{}; <* UNUSED *> within: Precedence; ):
  TEXT =
  BEGIN
    IF style.base = 10 THEN
      RETURN F.Int(x, style.base);
    ELSE
      RETURN F.Int(x, style.base) & "_{" & F.Int(style.base) & "}";
    END;
  END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN L.Int(rd, style.base);
  END Lex;

BEGIN
END IntegerFmtLex.
