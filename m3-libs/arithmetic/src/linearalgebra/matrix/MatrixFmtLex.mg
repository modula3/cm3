GENERIC MODULE MatrixFmtLex(RF, V, VF, M);
(*Arithmetic for Modula-3, see doc for details*)

IMPORT Rd, Wr, TextWr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
IMPORT FmtLexSupport AS FSup;

<*UNUSED*>
CONST Module = "MatrixFmtLex.";

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR wr := TextWr.New();
  BEGIN
    Wr.PutText(
      wr, "M" & F.Int(NUMBER(x^)) & "x" & F.Int(NUMBER(x[0])) & "{\n");
    FOR i := FIRST(x^) TO LAST(x^) DO
      Wr.PutText(wr, "V" & F.Int(NUMBER(x[0])) & "{");
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        Wr.PutText(
          wr, F.Pad(RF.Fmt(x[i, j], style.elemStyle), style.width));
        IF j # LAST(x[0]) THEN Wr.PutText(wr, ", "); END;
      END;
      Wr.PutText(wr, "}");
      IF i # LAST(x^) THEN Wr.PutText(wr, ","); END;
      Wr.PutText(wr, "\n");
    END;
    Wr.PutText(wr, "}\n");
    RETURN TextWr.ToText(wr);
  END Fmt;

PROCEDURE Dup (str: TEXT; n: CARDINAL): TEXT
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR wr := TextWr.New();
  BEGIN
    FOR j := 0 TO n - 1 DO Wr.PutText(wr, str); END;
    RETURN TextWr.ToText(wr);
  END Dup;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR wr := TextWr.New();
  BEGIN
    Wr.PutText(wr, "\\left(\\begin{array}{");
    Wr.PutText(wr, Dup("c", NUMBER(x[0])));
    Wr.PutText(wr, "}\n");
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[i]) TO LAST(x[i]) DO
        Wr.PutText(wr, RF.Tex(x[i, j], style.elemStyle));
        IF j < LAST(x[i]) THEN Wr.PutText(wr, " & "); END;
      END;
      Wr.PutText(wr, " \\\\\n");
    END;
    Wr.PutText(wr, "\\end{array}\\right)\n");
    RETURN TextWr.ToText(wr);
  END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  TYPE
    List = REF RECORD
                 prev: List;
                 row : V.T;
               END;
  VAR
    vectorLexStyle := VF.LexStyle{
                        sep := style.colSep, term := style.rowTerm,
                        elemStyle := style.elemStyle};
    item := NEW(List, prev := NIL, row := VF.Lex(rd, vectorLexStyle));
    n    := 1;
    m    := NUMBER(item.row^);
  BEGIN
    TRY
      (*read the values into a list*)
      WHILE NOT FSup.CheckChar(rd, style.matTerm) DO
        item := NEW(List, prev := item, row := VF.Lex(rd, vectorLexStyle));
        IF NUMBER(item.row^) # m THEN
          RAISE L.Error;         (*should be NA.Error, size_mismatch*)
        END;
        INC(n);
      END;
    EXCEPT
    | Rd.EndOfFile =>            (*treat like termination character*)
    END;

    (*copy the list elements into the vector's array*)
    VAR z := NEW(M.T, n, m);
    BEGIN
      FOR i := n - 1 TO 0 BY -1 DO
        z[i] := item.row^;
        item := item.prev;
      END;
      <*ASSERT item=NIL*>
      RETURN z;
    END;
  END Lex;

BEGIN
END MatrixFmtLex.
