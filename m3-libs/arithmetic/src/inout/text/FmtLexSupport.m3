MODULE FmtLexSupport;
(*Copyright (c) 1996, m3na project*)

IMPORT Rd, Thread;
IMPORT Lex AS L;

<*UNUSED*>
CONST Module = "FmtLexSupport.";

PROCEDURE Parenthesize (t: TEXT; inner, outer: Precedence): TEXT =
  BEGIN
    IF inner < outer THEN
      RETURN "\\left(" & t & "\\right)";
    ELSE
      RETURN t;
    END;
  END Parenthesize;

PROCEDURE AssertSeparator (rd: Rd.T; sep: CHAR; ) RAISES {L.Error} =
  BEGIN
    TRY
      L.Skip(rd, SET OF CHAR{' '});
      IF sep # ' ' AND Rd.GetChar(rd) # sep THEN
        Rd.UnGetChar(rd);
        RAISE L.Error;
      END;
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Thread.Alerted => RAISE L.Error;
    END;
  END AssertSeparator;

PROCEDURE CheckSeparator (rd: Rd.T; sep: CHAR; ): BOOLEAN =
  BEGIN
    TRY
      L.Skip(rd, SET OF CHAR{' '});
      IF sep # ' ' AND Rd.GetChar(rd) # sep THEN
        Rd.UnGetChar(rd);
        RETURN FALSE;
      END;
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN FALSE;
    END;
    RETURN TRUE;
  END CheckSeparator;

BEGIN
END FmtLexSupport.
