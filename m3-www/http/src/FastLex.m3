(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Sun Feb 18 07:22:40 PST 1996 by steveg    *)

MODULE FastLex;

IMPORT Lex, Rd, Text, Thread, UnsafeRd;

PROCEDURE Scan (rd: Rd.T; READONLY cs: SET OF CHAR := Lex.NonBlanks): TEXT
  RAISES {Rd.Failure, Thread.Alerted} =
  CONST BufSize = 256;
  VAR
    res: TEXT                             := NIL;
    i                                     := 0;
    c  : CHAR;
    buf: ARRAY [0 .. BufSize - 1] OF CHAR;
  BEGIN
    TRY
      LOOP
        c := UnsafeRd.FastGetChar(rd);
        IF NOT (c IN cs) THEN UnsafeRd.FastUnGetChar(rd); EXIT END;
        IF i = BufSize THEN
          IF res = NIL THEN
            res := Text.FromChars(buf);
          ELSE
            res := res & Text.FromChars(buf);
          END;
          i := 0
        END;
        buf[i] := c;
        INC(i)
      END
    EXCEPT
      Rd.EndOfFile =>            (* SKIP *)
    END;
    IF res = NIL THEN
      RETURN Text.FromChars(SUBARRAY(buf, 0, i))
    ELSE
      RETURN res & Text.FromChars(SUBARRAY(buf, 0, i))
    END;
  END Scan;

PROCEDURE Skip(
    rd: Rd.T; READONLY cs: SET OF CHAR := Lex.Blanks)
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      LOOP
	IF NOT (UnsafeRd.FastGetChar(rd) IN cs) THEN
	  UnsafeRd.FastUnGetChar(rd);
	  RETURN
	END
      END
    EXCEPT Rd.EndOfFile => (* SKIP *)
    END
  END Skip;

BEGIN
END FastLex.
