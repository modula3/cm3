GENERIC MODULE VectorFmtLex(RF, RSeq);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Rd, Wr, TextWr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
IMPORT FmtLexSupport AS FSup;
FROM FmtLexSupport IMPORT Precedence;

<* UNUSED *>
CONST
  Module = "VectorFmt.";

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}; ): TEXT
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

PROCEDURE Tex (                      x     : T;
                            READONLY style       := TexStyle{};
               <* UNUSED *>          within      := Precedence.sum): TEXT
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
  VAR
    seq := NEW(RSeq.T).init();
    n   := 0;
  BEGIN
    TRY
      (* read the values into a sequence *)
      REPEAT
        seq.addhi(RF.Lex(rd, style.elemStyle));
        (* IO.Put("new data: "&RF.Fmt(item.data)&"\n"); *)
        INC(n);
      UNTIL
        NOT FSup.CheckChar(rd, style.sep) OR FSup.CheckChar(rd, style.term);
    EXCEPT
    | Rd.EndOfFile =>            (* treat like termination character *)
    END;

    (* copy the list elements into the vector's array *)
    WITH z = NEW(T, seq.size()) DO
      FOR i := FIRST(z^) TO LAST(z^) DO z[i] := seq.get(i); END;
      RETURN z;
    END;
  END Lex;

BEGIN
END VectorFmtLex.
