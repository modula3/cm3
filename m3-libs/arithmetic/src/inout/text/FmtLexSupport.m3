MODULE FmtLexSupport;
(* Arithmetic for Modula-3, see doc for details*)

IMPORT Rd, Thread;
IMPORT Lex AS L;

<* UNUSED *>
CONST
  Module = "FmtLexSupport.";

PROCEDURE Parenthesize (t: TEXT; inner, outer: Precedence; ): TEXT =
  BEGIN
    IF inner < outer THEN
      RETURN "\\left(" & t & "\\right)";
    ELSE
      RETURN t;
    END;
  END Parenthesize;

PROCEDURE AssertChar (rd: Rd.T; sep: CHAR; )
  RAISES {L.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      L.Skip(rd, SET OF CHAR{' '});
      IF sep # ' ' AND Rd.GetChar(rd) # sep THEN
        Rd.UnGetChar(rd);
        RAISE L.Error;
      END;
    EXCEPT
    | Rd.EndOfFile => RAISE L.Error;
    END;
  END AssertChar;

PROCEDURE CheckChar (rd: Rd.T; sep: CHAR; ): BOOLEAN
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    L.Skip(rd, SET OF CHAR{' '});
    IF sep # ' ' AND Rd.GetChar(rd) # sep THEN
      Rd.UnGetChar(rd);
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END CheckChar;

BEGIN
END FmtLexSupport.
