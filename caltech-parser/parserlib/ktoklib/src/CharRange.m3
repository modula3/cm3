(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: CharRange.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE CharRange;
IMPORT Text;
IMPORT TextRd, Rd, Thread;
IMPORT CharCodes, FileRdErr;
(* IMPORT Term; *)

PROCEDURE FromText(t: TEXT): T =
  VAR
    rd := TextRd.New(CharCodes.StripDelims(t));
    c, left: CHAR := '\000';
    result: T := NoChars;
    negate, range := FALSE;
    <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    TRY
      WHILE TRUE DO
        c := Rd.GetChar(rd);
        IF c = '^' THEN
          negate := TRUE;
        ELSIF c = '-' THEN
          <* ASSERT left # '\000' *>
          range := TRUE;
        ELSE
          Rd.UnGetChar(rd);
          c := CharCodes.GetChar(rd);
          IF range THEN
            result := result + T{left..c};
            range := FALSE;
          ELSE
            result := result + T{c};
            left := c;
          END;
        END;
      END;
    EXCEPT
      Rd.EndOfFile =>
      IF range THEN
        FileRdErr.E(NIL, "CharRange should not end in '-'");
      END;
    END;
    IF negate THEN
      result := AllChars - result;
    END;
    (*    Term.Wr("range: ");
          FOR c := FIRST(CHAR) TO LAST(CHAR) DO
          IF c IN result THEN
          Term.Wr(Text.FromChar(c));
          END;
          END;
          Term.WrLn(""); *)
    RETURN result;
  END FromText;
  
PROCEDURE FilterText(t: TEXT; replace: T := WhiteSpace;
                     with: CHAR := '\000'): TEXT =
  VAR
    chars := NEW(REF ARRAY OF CHAR, Text.Length(t));
    j: INTEGER := 0;
  BEGIN
    FOR i := 0 TO LAST(chars^) DO
      chars[j] := Text.GetChar(t, i);
      IF chars[j] IN replace THEN
        IF with = '\000' THEN
          DEC(j);
        ELSE
          chars[j] := with;
        END;
      END;
      INC(j);
    END;
    RETURN Text.FromChars(SUBARRAY(chars^, 0, j));
  END FilterText;
  
PROCEDURE Size(a: T): INTEGER =
  VAR
    count: INTEGER := 0;
  BEGIN
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      IF i IN a THEN
        INC(count);
      END;
    END;
    RETURN count;
  END Size;

BEGIN
END CharRange.
